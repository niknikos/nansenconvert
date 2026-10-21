# Validation of Biotic and Acoustic data before they are written as XML.
#
# The Biotic checks follow the Nansen Biotic Explorer's QC (check ids,
# thresholds and severities from its R/qc_functions.R and qc_thresholds.csv),
# adapted to the four Biotic tables. Code checks use the active code
# lists (Nansen or Custom); taxonomy checks use nansentaxonomy; positions on
# land use nansencore's maritime zones. Schema validation uses the NMD
# schemas bundled in schemas/, and the written XML is read back with
# RstoxData, the reader StoX uses.
#
# Every check returns findings: severity (error / warning / info), category,
# check id, where, message and suggestion. Errors block the download unless
# the user overrides them.

VALIDATION_THRESHOLDS <- list(
  soaktime_max_h       = 10,      # Biotic Explorer: 600 minutes
  distance_max_nmi     = 50,
  speed_min_kn         = 0.1,
  speed_max_kn         = 15,
  speed_tolerance      = 0.5,     # allowed mismatch between distance and speed x time
  depth_max_m          = 12000,
  vertical_opening_max = 100,
  trawldoorspread_max  = 500,
  length_max_m         = 4,       # Biotic Explorer: 400 cm
  weight_max_kg        = 500,
  mean_weight_min_g    = 0.05,
  mean_weight_max_g    = 200000,
  unit_ratio           = 8,       # length vs species median: a cm/mm mix-up
  lw_min_n             = 10,
  lw_resid_mad         = 4,
  transit_speed_max_kn = 20,      # between consecutive acoustic intervals
  nasc_outlier_mad     = 5,       # log10(sa) above median + k x MAD
  gap_info_h           = 1
)

# The helpers are sourced with the app folder as working directory, so the
# absolute path is fixed here; relative paths are kept as a fallback
SCHEMA_DIR_CANDIDATES <- unique(c(normalizePath(file.path(getwd(), "schemas"), winslash = "/", mustWork = FALSE),
                                  "schemas", file.path("..", "schemas")))

# ---------------------------------------------------------------------------
# Findings
# ---------------------------------------------------------------------------

findings_collector <- function() {
  rows <- list()
  add <- function(severity, category, check, where, message, suggestion = "") {
    n <- max(length(where), length(message))
    if (n == 0) return(invisible(NULL))
    rows[[length(rows) + 1]] <<- data.frame(
      severity = rep_len(severity, n), category = rep_len(category, n),
      check = rep_len(check, n), where = rep_len(as.character(where), n),
      message = rep_len(as.character(message), n),
      suggestion = rep_len(as.character(suggestion), n),
      stringsAsFactors = FALSE
    )
    invisible(NULL)
  }
  get <- function() {
    if (!length(rows)) {
      return(data.frame(severity = character(0), category = character(0), check = character(0),
                        where = character(0), message = character(0), suggestion = character(0),
                        stringsAsFactors = FALSE))
    }
    out <- do.call(rbind, rows)
    out[order(match(out$severity, c("error", "warning", "info")), out$category, out$check), , drop = FALSE]
  }
  list(add = add, get = get)
}

findings_summary <- function(f) {
  sev <- c("error", "warning", "info")
  stats::setNames(vapply(sev, function(s) sum(f$severity == s), integer(1)), sev)
}

# Limits how many rows one check reports; the rest are summarised in one row
.add_limited <- function(fc, severity, category, check, where, message, suggestion = "", max_rows = 25) {
  n <- length(where)
  if (n == 0) return(invisible(NULL))
  keep <- seq_len(min(n, max_rows))
  fc$add(severity, category, check, where[keep],
         if (length(message) > 1) message[keep] else message,
         if (length(suggestion) > 1) suggestion[keep] else suggestion)
  if (n > max_rows) {
    fc$add(severity, category, check, "...", sprintf("%d more row(s) with the same problem", n - max_rows))
  }
}

# "prefix value" labels; nothing at all when there are no values
# (paste() would return one empty label)
.w <- function(prefix, x) if (length(x)) paste(prefix, x) else character(0)
.num <- function(x) suppressWarnings(as.numeric(x))
.chr <- function(x) { x <- trimws(as.character(x)); x[is.na(x) | x == "NA"] <- ""; x }
.has <- function(df, cols) !is.null(df) && all(cols %in% names(df))
.is_int <- function(x) { x <- .chr(x); x == "" | grepl("^-?[0-9]+$", x) }
.empty <- function(x) .chr(x) == ""

.dist_nmi <- function(lat1, lon1, lat2, lon2) {
  r <- pi / 180
  a <- sin((lat2 - lat1) * r / 2)^2 + cos(lat1 * r) * cos(lat2 * r) * sin((lon2 - lon1) * r / 2)^2
  2 * 3440.065 * asin(pmin(1, sqrt(a)))
}

.parse_date <- function(x) as.Date(sub("Z$", "", .chr(x)), optional = TRUE)
.parse_secs <- function(x) {
  s <- sub("Z$", "", .chr(x))
  out <- rep(NA_real_, length(s))
  ok <- grepl("^[0-9]{1,2}:[0-9]{2}(:[0-9]{2}([.][0-9]*)?)?$", s)
  out[ok] <- vapply(strsplit(s[ok], ":", fixed = TRUE), function(v) {
    v <- as.numeric(v); v[1] * 3600 + v[2] * 60 + if (length(v) > 2) v[3] else 0
  }, numeric(1))
  out
}

.on_land <- function(lat, lon) {
  ok <- !is.na(lat) & !is.na(lon) & abs(lat) <= 90 & abs(lon) <= 180
  out <- rep(FALSE, length(lat))
  if (!any(ok)) return(out)
  z <- tryCatch(nansencore::assign_maritime_zone(lon = lon[ok], lat = lat[ok]), error = function(e) NULL)
  if (is.null(z)) return(out)
  out[ok] <- is.na(z$mrgid)
  out
}

# ---------------------------------------------------------------------------
# Schema validation and read-back
# ---------------------------------------------------------------------------

schema_path <- function(file) {
  for (d in SCHEMA_DIR_CANDIDATES) {
    p <- file.path(d, file)
    if (file.exists(p)) return(normalizePath(p, winslash = "/"))
  }
  NA_character_
}

#' Validates an XML file against its NMD schema ("biotic" or "acoustic")
validate_xml_schema <- function(xml_path, type = c("biotic", "acoustic")) {
  type <- match.arg(type)
  fc <- findings_collector()
  xsd <- schema_path(if (type == "biotic") "bioticv3_1.xsd" else "nmdechosounderv1.xsd")
  if (is.na(xsd)) {
    fc$add("info", "schema", "SCHEMA_NOT_RUN", "file", "Schema file not found in schemas/; schema check skipped.")
    return(fc$get())
  }
  res <- tryCatch({
    doc <- xml2::read_xml(xml_path)
    xml2::xml_validate(doc, xml2::read_xml(xsd))
  }, error = function(e) e)
  if (inherits(res, "error")) {
    fc$add("error", "schema", "SCHEMA_UNREADABLE", "file", paste("The XML could not be read:", conditionMessage(res)))
    return(fc$get())
  }
  if (!isTRUE(res)) {
    err <- gsub("\\{[^}]*\\}", "", attr(res, "errors"))
    err <- gsub("'[^']*' is not a valid value", "a value is not valid", err)
    tab <- sort(table(err), decreasing = TRUE)
    fc$add("error", "schema", "SCHEMA_INVALID", sprintf("%d place(s)", as.integer(tab)), names(tab),
           "Fix the data behind these elements; the file does not follow the NMD schema.")
  }
  fc$get()
}

#' Reads the written XML back with RstoxData (as StoX does) and compares counts
readback_xml <- function(xml_path, type = c("biotic", "acoustic"), expected = NULL) {
  type <- match.arg(type)
  fc <- findings_collector()
  if (!requireNamespace("RstoxData", quietly = TRUE)) {
    fc$add("info", "read-back", "READBACK_NOT_RUN", "file", "RstoxData is not installed; read-back skipped.")
    return(fc$get())
  }
  x <- tryCatch(RstoxData::readXmlFile(xml_path), error = function(e) e)
  if (inherits(x, "error")) {
    fc$add("error", "read-back", "READBACK_FAILED", "file", paste("StoX's reader could not read the file:", conditionMessage(x)))
    return(fc$get())
  }
  tabs <- if (type == "biotic") c("fishstation", "catchsample", "individual") else c("distance", "sa")
  for (t in tabs) {
    got <- if (is.null(x[[t]])) 0L else nrow(x[[t]])
    exp <- expected[[t]]
    if (!is.null(exp) && !is.na(exp) && got != exp) {
      fc$add("error", "read-back", "READBACK_COUNT", t,
             sprintf("StoX's reader found %d %s record(s); %d were written.", got, t, exp))
    }
  }
  fc$add("info", "read-back", "READBACK_OK", "file",
         paste0("Read by RstoxData: ", paste(sprintf("%s %d", tabs,
                vapply(tabs, function(t) if (is.null(x[[t]])) 0L else nrow(x[[t]]), integer(1))), collapse = ", "), "."))
  fc$get()
}

# ---------------------------------------------------------------------------
# Biotic
# ---------------------------------------------------------------------------

BIOTIC_CODE_FIELDS <- list(
  missions      = c(missiontype = "missiontype", platform = "platform"),
  fishstations  = c(nation = "nation", catchplatform = "platform", gear = "gear",
                    haulvalidity = "haulvalidity", stationtype = "fishstationtype"),
  catchesamples = c(sampletype = "sampletype", lengthmeasurement = "lengthmeasurement",
                    conservation = "conservation", catchproducttype = "producttype",
                    sampleproducttype = "producttype"),
  individuals   = c(sex = "sex", maturationstage = "maturationstage",
                    lengthresolution = "lengthresolution", individualproducttype = "producttype")
)

#' Reads a Biotic XML (RstoxData) into the four Biotic tables
biotic_tables_from_xml <- function(xml_path) {
  x <- RstoxData::readXmlFile(xml_path)
  to_chr <- function(d) { d <- as.data.frame(d); d[] <- lapply(d, function(v) as.character(v)); d }
  list(missions = to_chr(x$mission), fishstations = to_chr(x$fishstation),
       catchesamples = to_chr(x$catchsample), individuals = to_chr(x$individual))
}

#' All checks on an existing Biotic XML file: the schema, StoX's reader and
#' the data checks. Used for uploaded files and for Nansis exports.
validate_biotic_xml_file <- function(xml_path, lists = NULL) {
  f_schema <- validate_xml_schema(xml_path, "biotic")
  fc <- findings_collector()
  tabs <- tryCatch(biotic_tables_from_xml(xml_path), error = function(e) e)
  if (inherits(tabs, "error")) {
    fc$add("error", "read-back", "READBACK_FAILED", "file", paste("StoX's reader could not read the file:", conditionMessage(tabs)))
    return(rbind(fc$get(), f_schema))
  }
  fc$add("info", "read-back", "READBACK_OK", "file",
         sprintf("Read by RstoxData: fishstation %d, catchsample %d, individual %d.",
                 nrow(tabs$fishstations), nrow(tabs$catchesamples), nrow(tabs$individuals)))
  rbind(validate_biotic(tabs, lists), f_schema, fc$get())
}

#' All Biotic checks. tables: missions, fishstations, catchesamples, individuals
validate_biotic <- function(tables, lists = NULL, th = VALIDATION_THRESHOLDS, check_land = TRUE) {
  fc <- findings_collector()
  ms <- tables$missions; fs <- tables$fishstations
  cs <- tables$catchesamples; ind <- tables$individuals

  # --- Structure and keys ---------------------------------------------------
  if (is.null(ms) || !nrow(ms)) {
    fc$add("error", "structure", "MISSION_MISSING", "missions", "No mission row.")
  } else {
    for (k in c("missiontype", "startyear", "platform", "missionnumber")) {
      if (!k %in% names(ms) || .empty(ms[[k]][1])) {
        fc$add("error", "structure", "MISSION_KEY_MISSING", "mission", sprintf("%s is required.", k))
      }
    }
    for (k in intersect(c("startyear", "missionnumber"), names(ms))) {
      if (!.is_int(ms[[k]][1])) fc$add("error", "structure", "MISSION_KEY_INTEGER", "mission",
                                       sprintf("%s must be a whole number (is '%s').", k, ms[[k]][1]))
    }
    if (nrow(ms) > 1) fc$add("warning", "structure", "MISSION_MULTIPLE", "missions",
                             sprintf("%d mission rows; only the first is written.", nrow(ms)))
  }
  sn <- if (.has(fs, "serialnumber")) .chr(fs$serialnumber) else character(0)
  if (length(sn)) {
    bad <- sn == "" | !.is_int(sn)
    .add_limited(fc, "error", "structure", "STN_SERIAL_INVALID", .w("station row", which(bad)),
                 "serialnumber is missing or not a whole number.")
    dup <- unique(sn[duplicated(sn) & sn != ""])
    .add_limited(fc, "error", "structure", "XF_SERIAL_REUSED", .w("station", dup),
                 "serialnumber is used by more than one station; its catches and fish would be written twice.",
                 "Give each station its own serialnumber.")
  }
  if (.has(cs, c("serialnumber", "catchsampleid"))) {
    k <- paste(.chr(cs$serialnumber), .chr(cs$catchsampleid))
    dup <- unique(k[duplicated(k)])
    .add_limited(fc, "error", "structure", "CATCH_KEY_DUPLICATE", .w("station / sample", dup),
                 "Two catch samples share the same serialnumber and catchsampleid.")
    orphan <- !.chr(cs$serialnumber) %in% sn
    .add_limited(fc, "error", "structure", "CATCH_ORPHAN",
                 .w("station", unique(.chr(cs$serialnumber)[orphan])),
                 "Catch samples for a station that is not in fishstations; they are not written.")
    bad <- !.is_int(cs$catchsampleid) | .empty(cs$catchsampleid)
    .add_limited(fc, "error", "structure", "CATCH_ID_INVALID", .w("catch row", which(bad)),
                 "catchsampleid is missing or not a whole number.")
  }
  if (.has(ind, c("serialnumber", "catchsampleid", "specimenid"))) {
    nospec <- .empty(ind$specimenid)
    if (any(nospec)) fc$add("warning", "structure", "IND_SPECIMEN_MISSING", sprintf("%d fish", sum(nospec)),
                            "Fish without a specimenid are left out of the XML.")
    badspec <- !nospec & !.is_int(ind$specimenid)
    .add_limited(fc, "error", "structure", "IND_SPECIMEN_INVALID", .w("fish row", which(badspec)),
                 "specimenid must be a whole number.")
    k <- paste(.chr(ind$serialnumber), .chr(ind$catchsampleid), .chr(ind$specimenid))
    dupk <- k[duplicated(k) & !nospec]
    if (length(dupk)) fc$add("error", "structure", "IND_KEY_DUPLICATE", sprintf("%d fish", length(dupk)),
                             "Fish share station, catch sample and specimenid with another fish.",
                             "Each fish needs its own specimenid within its catch sample.")
    nolink <- .empty(ind$catchsampleid)
    if (any(nolink)) fc$add("warning", "structure", "IND_NOT_LINKED", sprintf("%d fish", sum(nolink)),
                            "Fish not linked to a catch sample (no matching species and station); they are not written.")
    if (.has(cs, c("serialnumber", "catchsampleid"))) {
      ck <- paste(.chr(cs$serialnumber), .chr(cs$catchsampleid))
      ik <- paste(.chr(ind$serialnumber), .chr(ind$catchsampleid))
      orphan <- !nolink & !ik %in% ck
      if (any(orphan)) fc$add("warning", "structure", "IND_ORPHAN", sprintf("%d fish", sum(orphan)),
                              "Fish point to a catch sample that does not exist; they are not written.")
    }
  }

  # --- Codes ----------------------------------------------------------------
  for (tab in names(BIOTIC_CODE_FIELDS)) {
    d <- tables[[tab]]
    if (is.null(d) || !nrow(d)) next
    for (field in names(BIOTIC_CODE_FIELDS[[tab]])) {
      if (!field %in% names(d)) next
      id <- BIOTIC_CODE_FIELDS[[tab]][[field]]
      v <- .chr(d[[field]])
      bad <- !code_list_valid(id, v, lists)
      if (!any(bad)) next
      vals <- sort(unique(v[bad]))
      label <- CODE_LIST_DEFS$label[CODE_LIST_DEFS$id == id]
      mode <- if (code_list_mode(id, lists) == "custom") "custom" else "Nansen"
      fc$add("warning", "codes", "CODE_INVALID", paste0(tab, "$", field),
             sprintf("%d row(s) with code(s) not in the %s list (%s): %s", sum(bad), label, mode,
                     paste(utils::head(vals, 10), collapse = ", ")),
             "Choose a code from the list, or add it to a custom list.")
    }
  }

  # --- Stations ---------------------------------------------------------------
  if (!is.null(fs) && nrow(fs)) {
    where <- paste("station", .chr(fs$serialnumber))
    lat1 <- .num(fs$latitudestart); lon1 <- .num(fs$longitudestart)
    lat2 <- .num(fs$latitudeend);   lon2 <- .num(fs$longitudeend)
    miss <- is.na(lat1) | is.na(lon1)
    .add_limited(fc, "warning", "station", "STN_COORD_MISSING", where[miss], "Start position is missing.")
    rng <- (!is.na(lat1) & abs(lat1) > 90) | (!is.na(lon1) & abs(lon1) > 180) |
      (!is.na(lat2) & abs(lat2) > 90) | (!is.na(lon2) & abs(lon2) > 180)
    .add_limited(fc, "error", "station", "STN_COORD_RANGE", where[rng], "A position is out of range.")
    zero <- !is.na(lat1) & !is.na(lon1) & lat1 == 0 & lon1 == 0
    .add_limited(fc, "warning", "station", "STN_COORD_ZERO", where[zero], "Start position is 0, 0.")
    if (check_land) {
      land <- .on_land(lat1, lon1)
      .add_limited(fc, "warning", "station", "STN_ON_LAND", where[land],
                   sprintf("Start position %.4f, %.4f is on land.", lat1[land], lon1[land]))
    }
    d1 <- .parse_date(fs$stationstartdate); d2 <- .parse_date(fs$stationstopdate)
    t1 <- .parse_secs(fs$stationstarttime); t2 <- .parse_secs(fs$stationstoptime)
    start <- as.numeric(d1) * 86400 + t1; stop <- as.numeric(d2) * 86400 + t2
    neg <- !is.na(start) & !is.na(stop) & stop < start
    .add_limited(fc, "warning", "station", "STN_DATE_ORDER", where[neg], "Station stops before it starts.")
    if (.has(ms, c("missionstartdate", "missionstopdate"))) {
      m1 <- .parse_date(ms$missionstartdate[1]); m2 <- .parse_date(ms$missionstopdate[1])
      out <- !is.na(d1) & ((!is.na(m1) & d1 < m1) | (!is.na(m2) & d1 > m2))
      .add_limited(fc, "warning", "station", "STN_DATE_OUTSIDE_MISSION", where[out],
                   "Station date is outside the mission dates.")
    }
    dupt <- !is.na(start) & duplicated(start) | (!is.na(start) & duplicated(start, fromLast = TRUE))
    .add_limited(fc, "warning", "station", "STN_DUPLICATE_TIME", where[dupt], "Another station starts at the same date and time.")
    dist <- .num(fs$distance)
    bad <- !is.na(dist) & (dist <= 0 | dist > th$distance_max_nmi)
    .add_limited(fc, "warning", "station", "STN_DISTANCE_RANGE", where[bad],
                 sprintf("Towed distance %.2f nmi is implausible.", dist[bad]))
    # Recorded distance against the start and end positions (position typos)
    gc <- .dist_nmi(lat1, lon1, lat2, lon2)
    off <- !is.na(dist) & !is.na(gc) & gc > 0.2 & abs(dist - gc) / pmax(gc, dist) > th$speed_tolerance
    .add_limited(fc, "warning", "station", "STN_DISTANCE_POSITIONS", where[off],
                 sprintf("Towed distance %.2f nmi, but the start and end positions are %.2f nmi apart.", dist[off], gc[off]),
                 "Check the start and end positions.")
    spd <- .num(fs$vesselspeed)
    bad <- !is.na(spd) & (spd < th$speed_min_kn | spd > th$speed_max_kn)
    .add_limited(fc, "warning", "station", "STN_SPEED_RANGE", where[bad], sprintf("Vessel speed %.1f kn is implausible.", spd[bad]))
    soak <- .num(fs$soaktime)
    bad <- !is.na(soak) & (soak <= 0 | soak > th$soaktime_max_h)
    .add_limited(fc, "warning", "station", "STN_SOAKTIME_RANGE", where[bad], sprintf("Soak time %.2f h is implausible.", soak[bad]))
    # Distance against speed x time: catches position and time typos
    hours <- ifelse(!is.na(soak) & soak > 0, soak, (stop - start) / 3600)
    expect <- spd * hours
    off <- !is.na(dist) & !is.na(expect) & expect > 0 & abs(dist - expect) / expect > th$speed_tolerance
    .add_limited(fc, "warning", "station", "STN_EFFORT_INCONSISTENT", where[off],
                 sprintf("Towed distance %.2f nmi, but %.1f kn for %.2f h gives %.2f nmi.", dist[off], spd[off], hours[off], expect[off]),
                 "Check the start and end positions, times and speed.")
    for (col in intersect(c("bottomdepthstart", "bottomdepthstop"), names(fs))) {
      v <- .num(fs[[col]])
      bad <- !is.na(v) & (v <= 0 | v > th$depth_max_m)
      .add_limited(fc, "warning", "station", "STN_DEPTH_RANGE", where[bad], sprintf("%s %.0f m is implausible.", col, v[bad]))
    }
    if (.has(fs, c("fishingdepthmax", "bottomdepthstart"))) {
      fd <- .num(fs$fishingdepthmax); bd <- .num(fs$bottomdepthstart)
      bad <- !is.na(fd) & !is.na(bd) & fd > bd * 1.1 + 5
      .add_limited(fc, "warning", "station", "STN_FISHING_BELOW_BOTTOM", where[bad],
                   sprintf("Fishing depth %.0f m is below the bottom depth %.0f m.", fd[bad], bd[bad]))
    }
    geo <- list(verticaltrawlopening = th$vertical_opening_max, trawldoorspread = th$trawldoorspread_max)
    for (col in intersect(names(geo), names(fs))) {
      v <- .num(fs[[col]])
      bad <- !is.na(v) & (v <= 0 | v > geo[[col]])
      .add_limited(fc, "warning", "station", "STN_GEAR_GEOMETRY", where[bad], sprintf("%s %.1f m is implausible.", col, v[bad]))
    }
  }

  # --- Catch samples ---------------------------------------------------------
  if (!is.null(cs) && nrow(cs)) {
    where <- paste0("station ", .chr(cs$serialnumber), " / sample ", .chr(cs$catchsampleid),
                    if ("scientificname" %in% names(cs)) paste0(" (", .chr(cs$scientificname), ")") else "")
    cw <- .num(cs$catchweight); cn <- .num(cs$catchcount)
    .add_limited(fc, "warning", "catch", "CATCH_WEIGHT_MISSING", where[is.na(cw)], "catchweight is missing.")
    .add_limited(fc, "info", "catch", "CATCH_COUNT_MISSING", where[is.na(cn)], "catchcount is missing.")
    neg <- (!is.na(cw) & cw < 0) | (!is.na(cn) & cn < 0)
    .add_limited(fc, "error", "catch", "CATCH_NEGATIVE", where[neg], "Negative catch weight or count.")
    if ("lengthsampleweight" %in% names(cs)) {
      lw <- .num(cs$lengthsampleweight)
      bad <- !is.na(lw) & !is.na(cw) & lw > cw * 1.001
      .add_limited(fc, "error", "catch", "CATCH_SUBSAMPLE_EXCEEDS", where[bad],
                   sprintf("Length sample weight %.3f kg exceeds the catch weight %.3f kg.", lw[bad], cw[bad]))
    }
    mw <- cw * 1000 / cn
    bad <- !is.na(mw) & is.finite(mw) & (mw < th$mean_weight_min_g | mw > th$mean_weight_max_g)
    .add_limited(fc, "warning", "catch", "CATCH_MEAN_WEIGHT", where[bad], sprintf("Mean weight %.2f g per fish is implausible.", mw[bad]))
    if (.has(cs, c("serialnumber", "scientificname"))) {
      k <- paste(.chr(cs$serialnumber), tolower(.chr(cs$scientificname)))
      dup <- unique(k[duplicated(k)])
      .add_limited(fc, "info", "catch", "CATCH_DUPLICATE_SPECIES", dup,
                   "Species recorded in more than one catch sample at this station; fish are linked to the first one.")
    }
    if (.has(ind, c("serialnumber", "catchsampleid")) && "lengthsamplecount" %in% names(cs)) {
      n_ind <- table(paste(.chr(ind$serialnumber), .chr(ind$catchsampleid)))
      ck <- paste(.chr(cs$serialnumber), .chr(cs$catchsampleid))
      have <- as.numeric(n_ind[ck]); lsc <- .num(cs$lengthsamplecount)
      bad <- !is.na(have) & !is.na(lsc) & have > lsc
      .add_limited(fc, "warning", "catch", "IND_EXCEEDS_SAMPLECOUNT", where[bad],
                   sprintf("%d fish recorded, but lengthsamplecount is %d.", as.integer(have[bad]), as.integer(lsc[bad])))
    }
  }

  # --- Individuals -----------------------------------------------------------
  if (!is.null(ind) && nrow(ind)) {
    where <- paste0("station ", .chr(ind$serialnumber), " / sample ", .chr(ind$catchsampleid), " / fish ", .chr(ind$specimenid))
    len <- .num(ind$length); wt <- .num(ind$individualweight)
    bad <- !is.na(len) & (len <= 0 | len > th$length_max_m)
    .add_limited(fc, "warning", "individual", "IND_LENGTH_RANGE", where[bad], sprintf("Length %.3f m is implausible.", len[bad]))
    bad <- !is.na(wt) & (wt <= 0 | wt > th$weight_max_kg)
    .add_limited(fc, "warning", "individual", "IND_WEIGHT_RANGE", where[bad], sprintf("Weight %.3f kg is implausible.", wt[bad]))
    sp <- NULL
    if (.has(cs, c("serialnumber", "catchsampleid", "scientificname"))) {
      sp <- .chr(cs$scientificname)[match(paste(.chr(ind$serialnumber), .chr(ind$catchsampleid)),
                                          paste(.chr(cs$serialnumber), .chr(cs$catchsampleid)))]
    }
    if (!is.null(sp)) {
      med <- tapply(len, sp, stats::median, na.rm = TRUE)
      ratio <- len / as.numeric(med[sp])
      bad <- !is.na(ratio) & (ratio > th$unit_ratio | ratio < 1 / th$unit_ratio)
      .add_limited(fc, "warning", "individual", "IND_LENGTH_UNIT", where[bad],
                   sprintf("Length %.3f m is far from the %s median (%.3f m): a cm/mm mix-up?", len[bad], sp[bad], as.numeric(med[sp[bad]])))
      # Length-weight outliers: log-log fit per species, residuals in MAD units
      for (s in unique(sp[!is.na(sp)])) {
        i <- which(sp == s & !is.na(len) & !is.na(wt) & len > 0 & wt > 0)
        if (length(i) < th$lw_min_n) next
        fit <- stats::lm(log(wt[i]) ~ log(len[i]))
        r <- stats::residuals(fit); m <- stats::mad(r)
        if (!is.finite(m) || m == 0) next
        out <- i[abs(r - stats::median(r)) / m > th$lw_resid_mad]
        .add_limited(fc, "info", "individual", "IND_LW_OUTLIER", where[out],
                     sprintf("%s: weight %.3f kg is unusual for length %.3f m.", s, wt[out], len[out]), max_rows = 10)
      }
    }
  }

  # --- Taxonomy ---------------------------------------------------------------
  if (.has(cs, "scientificname")) {
    names_cs <- .chr(cs$scientificname)
    noaphia <- if ("aphia" %in% names(cs)) .empty(cs$aphia) else rep(TRUE, nrow(cs))
    if (any(noaphia)) {
      tab <- table(names_cs[noaphia])
      fc$add("error", "taxonomy", "TAX_APHIA_MISSING", names(tab),
             sprintf("%d catch sample(s) without an aphia code (required by NMDBiotic).", as.integer(tab)),
             "Match the species to the taxonomy in Catchsamples.")
    }
    u <- unique(names_cs[names_cs != ""])
    if (length(u)) {
      res <- taxonomy_resolve(u)
      ref <- taxonomy_reference()
      unres <- u[is.na(res$aphia)]
      if (length(unres)) {
        sug <- taxonomy_suggest(unres)
        fc$add("warning", "taxonomy", "TAX_UNRESOLVED", unres, "Name not found in nansentaxonomy.",
               ifelse(is.na(sug[unres]), "Check the name online in WoRMS.", paste("Did you mean", sug[unres], "?")))
      }
      if ("aphia" %in% names(cs)) {
        given <- tapply(.chr(cs$aphia), names_cs, function(a) a[a != ""][1])
        exp <- stats::setNames(res$aphia, u)
        inc <- u[!is.na(exp[u]) & !is.na(given[u]) & given[u] != exp[u]]
        if (length(inc)) fc$add("warning", "taxonomy", "TAX_APHIA_INCONSISTENT", inc,
                                sprintf("aphia %s does not match the taxonomy (%s).", given[inc], exp[inc]))
      }
      ri <- match(tolower(u), tolower(ref$scientificname))
      syn <- !is.na(ri) & !ref$status[ri] %in% c("accepted", NA) & !is.na(ref$valid_name[ri]) &
        tolower(ref$valid_name[ri]) != tolower(u)
      if (any(syn)) fc$add("info", "taxonomy", "TAX_SYNONYM", u[syn],
                           sprintf("Not the accepted name (%s); accepted name: %s.", ref$status[ri[syn]], ref$valid_name[ri[syn]]))
      notsp <- !is.na(ri) & !ref$rank[ri] %in% c("Species", "Subspecies", NA, "")
      if (any(notsp)) fc$add("info", "taxonomy", "TAX_NOT_SPECIES", u[notsp],
                             sprintf("Recorded at %s level, not species.", tolower(ref$rank[ri[notsp]])))
      ph <- !is.na(res$is_placeholder_tsn) & res$is_placeholder_tsn
      if (any(ph)) fc$add("info", "taxonomy", "TAX_PLACEHOLDER_TSN", u[ph],
                          sprintf("catchcategory %s is an NMD placeholder code, not an ITIS TSN.", res$catchcategory[ph]))
    }
  }
  fc$get()
}

# ---------------------------------------------------------------------------
# Acoustic
# ---------------------------------------------------------------------------

#' Reads an NMD Echosounder XML (RstoxData) into the six Acoustic sheets
acoustic_sheets_from_xml <- function(xml_path) {
  x <- RstoxData::readXmlFile(xml_path)
  to_df <- function(d) as.data.frame(d, stringsAsFactors = FALSE)
  dist <- to_df(x$distance)
  key <- paste(dist$log_start, dist$start_time)
  did <- function(d) match(paste(d$log_start, d$start_time), key)
  dist <- cbind(distance_id = seq_len(nrow(dist)), dist)
  fr <- to_df(x$frequency); fr <- cbind(distance_id = did(fr), fr)
  ct <- to_df(x$ch_type);   ct <- cbind(distance_id = did(ct), ct)
  sb <- to_df(x$sa_by_acocat); sb <- cbind(distance_id = did(sb), sb)
  sa <- to_df(x$sa); sa <- cbind(distance_id = did(sa), sa)
  names(sa)[names(sa) == "sa"] <- "value"
  list(root_metadata = to_df(x$echosounder_dataset), distances = dist, frequencies = fr,
       ch_types = ct, sa_by_acocat = sb, sa = sa)
}

#' All Acoustic checks on the six Acoustic sheets. biotic (optional) is
#' the four Biotic tables, used to check that each acoustic category has
#' trawl samples; acocat_level is passed to the Nansen acocat mapping.
validate_acoustic <- function(sheets, lists = NULL, biotic = NULL, th = VALIDATION_THRESHOLDS,
                              acocat_level = "specific", check_land = TRUE) {
  fc <- findings_collector()
  md <- sheets$root_metadata; dist <- sheets$distances; fr <- sheets$frequencies
  sa <- sheets$sa

  # --- Metadata ---------------------------------------------------------------
  if (is.null(md) || !nrow(md)) {
    fc$add("error", "metadata", "ACO_META_MISSING", "root_metadata", "No metadata row.")
  } else {
    for (k in c("report_time", "lsss_version", "nation", "platform", "cruise")) {
      if (!k %in% names(md) || .empty(md[[k]][1])) fc$add("error", "metadata", "ACO_META_MISSING", k,
                                                          sprintf("%s is required.", k))
    }
    if ("platform" %in% names(md) && !.empty(md$platform[1]) && !.is_int(md$platform[1])) {
      fc$add("error", "metadata", "ACO_PLATFORM_INTEGER", "platform",
             sprintf("platform must be a whole number (is '%s').", md$platform[1]))
    }
    for (k in c("nation", "platform")) {
      if (k %in% names(md) && !.empty(md[[k]][1]) && !code_list_valid(k, md[[k]][1], lists)) {
        fc$add("warning", "codes", "CODE_INVALID", paste0("root_metadata$", k),
               sprintf("'%s' is not in the %s code list.", md[[k]][1], k))
      }
    }
  }

  # --- Distances --------------------------------------------------------------
  if (!is.null(dist) && nrow(dist)) {
    o <- order(.num(dist$distance_id))
    dist <- dist[o, , drop = FALSE]
    where <- paste0("distance ", dist$distance_id, " (", .chr(dist$start_time), ")")
    key <- paste(.chr(dist$log_start), .chr(dist$start_time))
    dup <- duplicated(key)
    .add_limited(fc, "error", "distance", "ACO_DIST_DUPLICATE", where[dup], "Two distances share log_start and start_time.")
    ts <- as.POSIXct(.chr(dist$start_time), tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
    te <- as.POSIXct(.chr(dist$stop_time), tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
    .add_limited(fc, "error", "distance", "ACO_TIME_INVALID", where[is.na(ts) | is.na(te)],
                 "start_time or stop_time cannot be read (expected YYYY-MM-DD hh:mm:ss).")
    dur_h <- as.numeric(difftime(te, ts, units = "hours"))
    .add_limited(fc, "error", "distance", "ACO_TIME_ORDER", where[!is.na(dur_h) & dur_h < 0], "stop_time is before start_time.")
    .add_limited(fc, "warning", "distance", "ACO_TIME_ZERO", where[!is.na(dur_h) & dur_h == 0], "The interval lasts 0 seconds.")
    if (nrow(dist) > 1) {
      ot <- order(ts)
      tsn <- as.numeric(ts); ten <- as.numeric(te)     # seconds, so NA padding keeps the type
      prev_end <- c(NA, ten[ot][-length(ot)])
      gap_h <- (tsn[ot] - prev_end) / 3600
      ov <- ot[!is.na(gap_h) & gap_h < -1 / 3600]
      .add_limited(fc, "warning", "distance", "ACO_TIME_OVERLAP", where[ov], "The interval starts before the previous one ends.")
      lat_s <- .num(dist$lat_start); lon_s <- .num(dist$lon_start)
      lat_e <- .num(dist$lat_stop);  lon_e <- .num(dist$lon_stop)
      hop <- .dist_nmi(c(NA, lat_e[ot][-length(ot)]), c(NA, lon_e[ot][-length(ot)]), lat_s[ot], lon_s[ot])
      spd <- hop / pmax(gap_h, 1 / 60)
      jump <- ot[!is.na(spd) & gap_h >= 0 & hop > 1 & spd > th$transit_speed_max_kn]
      .add_limited(fc, "warning", "distance", "ACO_POSITION_JUMP", where[jump],
                   "The position jumps from the previous interval faster than a vessel can sail.",
                   "Check the GPS positions.")
      big <- ot[!is.na(gap_h) & gap_h > th$gap_info_h]
      if (length(big)) fc$add("info", "distance", "ACO_TIME_GAP", sprintf("%d place(s)", length(big)),
                              sprintf("Gaps of more than %g h between intervals (e.g. overnight or between transects).", th$gap_info_h))
    }
    len <- .num(dist$integrator_dist)
    .add_limited(fc, "warning", "distance", "ACO_LENGTH_INVALID", where[is.na(len) | len <= 0], "integrator_dist is missing or not positive.")
    spd <- len / dur_h
    bad <- !is.na(spd) & is.finite(spd) & len > 0.05 & (spd < th$speed_min_kn | spd > th$speed_max_kn)
    .add_limited(fc, "warning", "distance", "ACO_SPEED_RANGE", where[bad],
                 sprintf("%.2f nmi in %.1f min is %.1f kn.", len[bad], dur_h[bad] * 60, spd[bad]))
    lg <- .num(dist$log_start)
    if (sum(!is.na(lg)) > 1) {
      ot <- order(ts); dl <- diff(lg[ot])
      back <- ot[-1][!is.na(dl) & dl < 0]
      .add_limited(fc, "warning", "distance", "ACO_LOG_DECREASING", where[back], "log_start goes down compared with the previous interval.")
    }
    lat <- .num(dist$lat_start); lon <- .num(dist$lon_start)
    miss <- is.na(lat) | is.na(lon)
    .add_limited(fc, "warning", "distance", "ACO_COORD_MISSING", where[miss], "Start position is missing.")
    rng <- !miss & (abs(lat) > 90 | abs(lon) > 180)
    .add_limited(fc, "error", "distance", "ACO_COORD_RANGE", where[rng], "Start position is out of range (999 = no GPS fix?).")
    if (check_land) {
      land <- .on_land(lat, lon)
      .add_limited(fc, "warning", "distance", "ACO_ON_LAND", where[land], "Start position is on land.")
    }
  }

  # --- Frequencies and depths --------------------------------------------------
  if (!is.null(fr) && nrow(fr)) {
    where <- paste("distance", fr$distance_id)
    pair <- function(a, b, check, msg, slack = 0) {
      if (!all(c(a, b) %in% names(fr))) return()
      x <- .num(fr[[a]]); y <- .num(fr[[b]])
      bad <- !is.na(x) & !is.na(y) & x > y + slack
      .add_limited(fc, "warning", "depth", check, where[bad], sprintf(msg, x[bad], y[bad]))
    }
    pair("upper_integrator_depth", "lower_integrator_depth", "ACO_DEPTH_INTEGRATOR", "Upper integration depth %.1f m is below the lower one (%.1f m).")
    pair("upper_interpret_depth", "lower_interpret_depth", "ACO_DEPTH_INTERPRET", "Upper interpretation depth %.1f m is below the lower one (%.1f m).")
    # min_bot_depth comes from an Echoview depth (from the surface) and
    # max_bot_depth from a range (from the transducer), so the minimum can
    # exceed the maximum by the transducer depth; allow 15 m for that
    pair("min_bot_depth", "max_bot_depth", "ACO_DEPTH_BOTTOM",
         "Minimum bottom depth %.1f m is far below the maximum (%.1f m).", slack = 15)
    if (!is.null(sa) && nrow(sa) && "num_pel_ch" %in% names(fr)) {
      npc <- .num(fr$num_pel_ch)[match(sa$distance_id, fr$distance_id)]
      bad <- !is.na(npc) & .num(sa$ch) > npc
      if (any(bad)) fc$add("warning", "depth", "ACO_CHANNEL_RANGE", sprintf("%d sa value(s)", sum(bad)),
                           "Channel number is higher than num_pel_ch.")
    }
  }

  # --- NASC values and categories ----------------------------------------------
  if (!is.null(sa) && nrow(sa)) {
    v <- .num(sa$value)
    where <- paste0("distance ", sa$distance_id, " / acocat ", sa$acocat, " / ch ", sa$ch)
    .add_limited(fc, "error", "nasc", "ACO_NASC_NEGATIVE", where[!is.na(v) & v < 0], "Negative NASC.")
    .add_limited(fc, "error", "nasc", "ACO_NASC_MISSING", where[is.na(v)], "NASC value is missing.")
    lv <- log10(v[!is.na(v) & v > 0])
    if (length(lv) >= 20) {
      cut <- stats::median(lv) + th$nasc_outlier_mad * stats::mad(lv)
      out <- !is.na(v) & v > 0 & suppressWarnings(log10(v)) > cut
      .add_limited(fc, "info", "nasc", "ACO_NASC_OUTLIER", where[out],
                   sprintf("NASC %.0f is far above the rest (median %.1f).", v[out], 10^stats::median(lv)), max_rows = 10)
    }
    ac <- .chr(sa$acocat)
    bad <- !code_list_valid("acocat", ac, lists)
    if (any(bad)) fc$add("error", "codes", "ACO_ACOCAT_INVALID", paste("acocat", sort(unique(ac[bad]))),
                         "acocat is not in the acoustic category list.",
                         "Choose Nansen acoustic categories or add the code to the custom list.")
  }

  # --- Biological support: each category with NASC has trawl samples ----------
  if (!is.null(biotic) && .has(biotic$catchesamples, "scientificname") && !is.null(sa) && nrow(sa)) {
    sp <- unique(.chr(biotic$catchesamples$scientificname))
    sp <- sp[sp != ""]
    cat_bio <- unique(as.character(acocat_map_for(sp, lists, acocat_level)$acocat))
    cat_sa <- unique(.chr(sa$acocat[.num(sa$value) > 0]))
    ignore <- c("1", "6")    # OTHER and PLANK need no fish samples
    if (code_list_mode("acocat", lists) == "custom") ignore <- character(0)
    lack <- setdiff(cat_sa, c(cat_bio, ignore))
    tab <- code_list_table("acocat", lists)
    if (length(lack)) fc$add("warning", "biology", "ACO_NO_TRAWL_SUPPORT", paste("acocat", lack),
                             sprintf("NASC in category %s, but no catch in the Biotic file maps to it.",
                                     ifelse(is.na(tab$value[match(lack, tab$code)]), lack, tab$value[match(lack, tab$code)])),
                             "StoX needs biological samples to turn this NASC into abundance.")
    if (.has(biotic$fishstations, "stationstartdate") && !is.null(dist) && nrow(dist)) {
      bd <- .parse_date(biotic$fishstations$stationstartdate)
      ad <- as.Date(substr(.chr(dist$start_time), 1, 10), optional = TRUE)
      if (any(!is.na(bd)) && any(!is.na(ad))) {
        outside <- !is.na(bd) & (bd < min(ad, na.rm = TRUE) - 1 | bd > max(ad, na.rm = TRUE) + 1)
        if (any(outside)) fc$add("info", "biology", "ACO_TRAWL_OUTSIDE_PERIOD", sprintf("%d station(s)", sum(outside)),
                                 "Trawl stations more than a day outside the acoustic period.")
      }
    }
  }
  fc$get()
}

#' Species_percent per Echoview region should sum to 100. Uses Region_ID when
#' the export has it; otherwise regions are grouped by cell and PRC_NASC.
validate_species_percent <- function(nasc, tolerance = 0.5) {
  fc <- findings_collector()
  if (!.has(nasc, c("Species_percent", "PRC_NASC"))) return(fc$get())
  pct <- .num(nasc$Species_percent)
  if ("Region_ID" %in% names(nasc)) {
    key <- paste(.chr(nasc$Date_S), .chr(nasc$Interval), .chr(nasc$Layer), .chr(nasc$Region_ID))
    how <- "Region_ID"
  } else {
    key <- paste(.chr(nasc$Date_S), .chr(nasc$Time_S), .chr(nasc$Layer), .chr(nasc$PRC_NASC))
    how <- "cell and PRC_NASC (no Region_ID column)"
  }
  s <- tapply(pct, key, sum, na.rm = TRUE)
  bad <- names(s)[abs(s - 100) > tolerance]
  if (length(bad)) {
    .add_limited(fc, "warning", "nasc", "ACO_SPECIES_PERCENT", bad,
                 sprintf("Species_percent sums to %.2f in this region (grouped by %s).", s[bad], how),
                 "The species shares of a region should add up to 100.", max_rows = 15)
  }
  fc$get()
}

# ---------------------------------------------------------------------------
# Display helpers shared by the Validation panels
# ---------------------------------------------------------------------------

#' Coloured counts of errors, warnings and notes
findings_badges <- function(f) {
  s <- findings_summary(f)
  # Inline colours: the converter pages use both Bootstrap 4 and 5
  badge <- function(n, label, bg, fg = "#fff") {
    span(style = sprintf(paste0("display:inline-block; padding:3px 10px; margin-right:8px; border-radius:12px;",
                                " font-size:0.9rem; background:%s; color:%s;"), bg, fg),
         paste(n, label))
  }
  div(style = "margin-bottom:8px;",
      badge(s[["error"]], "error(s)", if (s[["error"]] > 0) "#dc3545" else "#6c757d"),
      badge(s[["warning"]], "warning(s)", if (s[["warning"]] > 0) "#ffc107" else "#6c757d",
            if (s[["warning"]] > 0) "#212529" else "#fff"),
      badge(s[["info"]], "note(s)", "#cfe2ff", "#084298"))
}

#' Findings table, coloured by severity
findings_dt <- function(f) {
  DT::formatStyle(
    DT::datatable(f, rownames = FALSE, filter = "top",
                  options = list(pageLength = 10, scrollX = TRUE, order = list())),
    "severity", target = "row",
    backgroundColor = DT::styleEqual(c("error", "warning", "info"), c("#f8d7da", "#fff3cd", "#e7f1fb"))
  )
}

#' Counts of written elements in an XML document, for the read-back check
xml_element_counts <- function(doc, names) {
  stats::setNames(lapply(names, function(n) length(xml2::xml_find_all(doc, sprintf("//*[local-name()='%s']", n)))), names)
}
