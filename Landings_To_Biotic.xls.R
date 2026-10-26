# app.R - Landing-site samples -> Biotic workbook (Biotic.xlsx)
# For catches sampled at landing sites instead of on a research vessel (the
# TAFIRI Lake Tanganyika stock-assessment sheet): one row per measured fish,
# with the sampling information repeated on every row. Each fishing unit's
# sample becomes a station, so the workbook feeds the Biotic.xls_To_Biotic.xml
# tab like the BioData_To_Biotic.xls one does.
library(shiny)
library(bslib)
library(readxl)
library(writexl)
library(dplyr)
library(DT)
library(shinyjs)
if (!exists("validate_biotic", mode = "function")) {
  for (h in file.path("helpers", c("code_lists.R", "taxonomy.R", "validation.R"))) source(h, local = TRUE)
}

# ---------------------------------------------------------------------------
# 0) What the sheet is expected to look like
# ---------------------------------------------------------------------------

# Columns are found by their header text (case and spacing ignored), so the
# order does not matter and the long field-form headers can differ a little.
LANDINGS_COLUMNS <- c(
  sn = "^sn",
  site = "village|^landing site$",         unit = "fishing unit id",
  date = "sampling date",                  gear = "^fishing unit type",
  land_lat = "landing site lat",           land_lon = "landing site long",
  fish_lat = "fishing location lat",       fish_lon = "fishing location long",
  ground = "^fishing ground",              fish_h = "time spent in fishing",
  mesh = "mesh size",                      nlamps = "number of lamps",
  watts = "total watts",
  lates_ad = "adult.*lates",               lates_juv = "juvenile.*lates",
  stolo = "total weight of stolothrissa",  limno = "total weight of limnothrissa",
  unsorted = "unsorted total catch",       other = "total weight of other species",
  shrimp = "shrimps",
  species = "^species measured",           tl = "^total length",
  w = "^weight \\(g",                      sex = "^sex",
  ms = "^ms$",                             gonad = "gonad weight",
  remarks = "^remarks"
)
LANDINGS_REQUIRED <- c("site", "date", "gear", "species", "tl")
LANDINGS_MEASUREMENT <- c("species", "tl", "w", "sex", "ms", "gonad")

# Species of the sheet: catch totals are given per group, fish by name.
# Aphia ids are from WoRMS (nansentaxonomy does not have these species).
LANDINGS_SPECIES <- data.frame(
  pattern = c("LATES", "STOLO", "LIMNO"),
  name    = c("Lates stappersii", "Stolothrissa tanganicae", "Limnothrissa miodon"),
  aphia   = c("1525847", "1017071", "1026860"),
  common  = c("Lake Tanganyika lates", "Tanganyika sardine", "Tanganyika sprat"),
  stringsAsFactors = FALSE
)
LANDINGS_CATCH_COLUMNS <- list(
  "Lates stappersii"        = c("lates_ad", "lates_juv"),
  "Stolothrissa tanganicae" = "stolo",
  "Limnothrissa miodon"     = "limno"
)

# The NMD gear list has no code for these gears: custom codes, to be replaced
# by the national ones when there are any
LANDINGS_GEAR_DEFAULTS <- c(LN = "9101", RN = "9102", HL = "9103", GN = "9104")
LANDINGS_GEAR_NAMES <- c(LN = "Lift net", RN = "Ring net", HL = "Hooks and lines", GN = "GN")

landings_default_options <- function() {
  list(
    nation = "155", missiontype = "18", platform = "9999", missionnumber = "1",
    cruise = "LANDING-SITES", callsignal = "NONE",
    platformname = "Landing-site sampling (no vessel)",
    purpose = "Fishery catch and biological sampling at landing sites",
    start_time = "12:00:00Z",                 # not recorded; StoX needs a date-time
    type_test = "11", type_other = "9",       # "Test fishing" remarks -> acoustic-target station
    gear_map = LANDINGS_GEAR_DEFAULTS,
    lat_range = c(3.3, 8.9), lon_range = c(29, 31.3), southern = TRUE,
    max_far_km = 60, fallback_positions = TRUE, predict_weights = TRUE,
    sampletype = "20", conservation = "1", producttype = "1", lengthmeasurement = "E",
    lengthresolution = "1"                    # 1 mm
  )
}

# ---------------------------------------------------------------------------
# 1) Reading
# ---------------------------------------------------------------------------

landings_squash <- function(x) {
  x <- gsub(" ", " ", as.character(x))
  x <- trimws(gsub("\\s+", " ", x))
  x[!is.na(x) & x == ""] <- NA_character_
  x
}

#' Header text -> standard column names. Returns the original header of each
#' standard column (NA when absent).
landings_map_columns <- function(headers) {
  h <- tolower(landings_squash(headers))
  h[is.na(h)] <- ""
  out <- stats::setNames(rep(NA_integer_, length(LANDINGS_COLUMNS)), names(LANDINGS_COLUMNS))
  used <- rep(FALSE, length(h))
  for (k in names(LANDINGS_COLUMNS)) {
    hit <- which(grepl(LANDINGS_COLUMNS[[k]], h) & !used)
    if (length(hit)) { out[k] <- hit[1]; used[hit[1]] <- TRUE }
  }
  out
}

#' Reads the first sheet as text and renames the recognised columns.
#' @return list(data, map, missing): missing = required columns not found
landings_read <- function(path, sheet = 1) {
  raw <- suppressMessages(readxl::read_excel(path, sheet = sheet, col_types = "text", .name_repair = "minimal"))
  map <- landings_map_columns(names(raw))
  d <- data.frame(row = seq_len(nrow(raw)) + 1L, stringsAsFactors = FALSE)
  for (k in names(LANDINGS_COLUMNS)) {
    d[[k]] <- if (is.na(map[k])) rep(NA_character_, nrow(raw)) else as.character(raw[[map[k]]])
  }
  d <- d[rowSums(!is.na(d[, names(LANDINGS_COLUMNS), drop = FALSE])) > 0, , drop = FALSE]
  list(data = d, map = map, headers = names(raw), missing = setdiff(LANDINGS_REQUIRED, names(map)[!is.na(map)]))
}

# ---------------------------------------------------------------------------
# 2) Cleaning helpers
# ---------------------------------------------------------------------------

#' "7April2026", "16-Apr-2026\t", "30march2026 " -> Date (NA when unreadable)
landings_parse_text_date <- function(s) {
  s <- tolower(gsub("\\s", "", as.character(s)))
  m <- regmatches(s, regexec("^(\\d{1,2})[-/]?([a-z]+)[-/]?(\\d{4})$", s))
  as.Date(vapply(m, function(x) {
    if (length(x) != 4) return(NA_character_)
    mo <- match(substr(x[3], 1, 3), tolower(month.abb))
    if (is.na(mo)) NA_character_ else sprintf("%s-%02d-%02d", x[4], mo, as.integer(x[2]))
  }, ""))
}

#' Excel serial numbers and written dates -> Date. A few dates (under 5%) more
#' than 180 days after the 90th percentile are taken to have a mistyped year
#' (30march2027 in a 2025-26 sheet) and get the year of the median date.
landings_parse_dates <- function(x) {
  x <- landings_squash(x)
  n <- suppressWarnings(as.numeric(x))
  out <- as.Date(n, origin = "1899-12-30")
  txt <- is.na(n) & !is.na(x)
  out[txt] <- landings_parse_text_date(x[txt])
  fixed <- 0L
  if (sum(!is.na(out)) >= 20) {
    mid <- stats::median(out, na.rm = TRUE)
    late <- !is.na(out) & out > stats::quantile(out, 0.9, na.rm = TRUE, type = 1) + 180
    if (any(late) && mean(late[!is.na(out)]) < 0.05) {
      cand <- as.Date(sub("^\\d{4}", format(mid, "%Y"), format(out[late])))
      out[late] <- cand
      fixed <- sum(late)
    }
  }
  attr(out, "n_numeric") <- sum(!is.na(n)); attr(out, "n_text") <- sum(txt)
  attr(out, "n_text_parsed") <- sum(!is.na(out[txt])); attr(out, "n_year_fixed") <- fixed
  out
}

landings_titlecase <- function(x) sub(" ([a-z])$", " \\U\\1", tools::toTitleCase(tolower(x)), perl = TRUE)

landings_hav_km <- function(la1, lo1, la2, lo2) {
  r <- pi / 180
  h <- sin((la2 - la1) * r / 2)^2 + cos(la1 * r) * cos(la2 * r) * sin((lo2 - lo1) * r / 2)^2
  2 * 6371 * asin(pmin(1, sqrt(h)))
}

# ---------------------------------------------------------------------------
# 3) Conversion: standard-named sheet -> the four Biotic tables
# ---------------------------------------------------------------------------

#' @param d data.frame from landings_read()$data (all columns text)
#' @param opts landings_default_options(), modified
#' @return list(tables = missions/fishstations/catchesamples/individuals,
#'   report = counts and the adjustments made, for the user to review)
landings_to_biotic <- function(d, opts = landings_default_options()) {
  opts <- utils::modifyList(landings_default_options(), opts)
  num <- function(x) suppressWarnings(as.numeric(x))
  for (k in setdiff(names(LANDINGS_COLUMNS), names(d))) d[[k]] <- NA_character_
  if (!"row" %in% names(d)) d$row <- seq_len(nrow(d)) + 1L
  for (k in names(LANDINGS_COLUMNS)) d[[k]] <- landings_squash(d[[k]])
  notes <- character(0); note <- function(...) notes <<- c(notes, sprintf(...))
  note("%d rows read.", nrow(d))

  # Rows holding only a fish measurement belong to the station of the row above
  info_cols <- setdiff(names(LANDINGS_COLUMNS), LANDINGS_MEASUREMENT)
  cont <- which(rowSums(!is.na(d[, info_cols, drop = FALSE])) == 0 &
                  rowSums(!is.na(d[, LANDINGS_MEASUREMENT, drop = FALSE])) > 0 & seq_len(nrow(d)) > 1)
  for (i in cont) d[i, info_cols] <- d[i - 1, info_cols]
  if (length(cont)) note("%d measurement-only row(s) given the station of the row above.", length(cont))

  # --- clean
  d$site_key <- toupper(d$site)
  d$gear_c   <- toupper(d$gear)
  dt <- landings_parse_dates(d$date)
  d$date_c <- dt
  note("Dates: %d Excel dates, %d written dates (%d read), %d blank%s.", attr(dt, "n_numeric"), attr(dt, "n_text"),
       attr(dt, "n_text_parsed"), sum(is.na(d$date)),
       if (attr(dt, "n_year_fixed")) sprintf("; %d with a mistyped year corrected", attr(dt, "n_year_fixed")) else "")
  for (k in c("land_lat", "land_lon", "fish_lat", "fish_lon", "fish_h", "mesh", "nlamps", "watts", "lates_ad", "lates_juv",
              "stolo", "limno", "tl", "w", "gonad")) d[[k]] <- num(d[[k]])
  up <- toupper(d$species)
  hit <- matrix(sapply(LANDINGS_SPECIES$pattern, function(p) grepl(p, up, fixed = TRUE)), nrow = length(up))
  d$sp <- LANDINGS_SPECIES$name[max.col(hit, ties.method = "first")]
  d$sp[rowSums(hit) == 0] <- NA_character_
  d$sex_c <- c(F = "1", M = "2")[toupper(d$sex)]

  # --- stations: runs of rows that share the same sampling information
  key <- do.call(paste, c(d[, c("site_key", "unit", "date", "gear_c", "fish_lat", "fish_lon", "ground",
                                 "lates_ad", "lates_juv", "stolo", "limno", "unsorted", "other", "shrimp")], sep = "|"))
  d$run <- cumsum(c(TRUE, key[-1] != key[-length(key)]))
  st <- d %>% group_by(run) %>% summarise(across(c(row, site_key, unit, date_c, gear_c, ground, land_lat, land_lon, fish_lat,
          fish_lon, fish_h, mesh, watts, nlamps, lates_ad, lates_juv, stolo, limno, remarks), first), n_rows = n(), .groups = "drop")
  note("%d station block(s) found.", nrow(st))

  box_ok <- function(la, lo) !is.na(la) & !is.na(lo) & abs(la) >= opts$lat_range[1] & abs(la) <= opts$lat_range[2] &
    lo >= opts$lon_range[1] & lo <= opts$lon_range[2]
  swap <- function(la, lo) {
    s <- !is.na(la) & !is.na(lo) & abs(la) >= opts$lon_range[1] & abs(la) <= opts$lon_range[2] &
      abs(lo) >= opts$lat_range[1] & abs(lo) <= opts$lat_range[2]
    list(la = ifelse(s, lo, la), lo = ifelse(s, la, lo), n = sum(s))
  }
  sw <- swap(st$fish_lat, st$fish_lon); st$fish_lat <- sw$la; st$fish_lon <- sw$lo; n_sw <- sw$n
  sw <- swap(st$land_lat, st$land_lon); st$land_lat <- sw$la; st$land_lon <- sw$lo; n_sw <- n_sw + sw$n
  if (opts$southern) { st$fish_lat <- -abs(st$fish_lat); st$land_lat <- -abs(st$land_lat) }
  f_ok <- box_ok(st$fish_lat, st$fish_lon); l_ok <- box_ok(st$land_lat, st$land_lon)
  far <- f_ok & l_ok & landings_hav_km(st$fish_lat, st$fish_lon, st$land_lat, st$land_lon) > opts$max_far_km
  f_use <- f_ok & !far
  if (opts$fallback_positions) {
    gpos <- st[f_use & !is.na(st$ground), ] %>% group_by(ground) %>%
      summarise(g_lat = stats::median(fish_lat), g_lon = stats::median(fish_lon), .groups = "drop")
    spos <- st[l_ok & !is.na(st$site_key), ] %>% group_by(site_key) %>%
      summarise(s_lat = stats::median(land_lat), s_lon = stats::median(land_lon), .groups = "drop")
    st <- st %>% left_join(gpos, by = "ground") %>% left_join(spos, by = "site_key")
  } else {
    st$g_lat <- st$g_lon <- st$s_lat <- st$s_lon <- NA_real_
  }
  L <- c("fishing location", "fishing ground (median of its samples)", "landing site", "landing site (median of its samples)")
  st$pos_src <- dplyr::case_when(f_use ~ L[1], !is.na(st$g_lat) ~ L[2], l_ok ~ L[3], !is.na(st$s_lat) ~ L[4], TRUE ~ NA_character_)
  st$lat <- dplyr::case_when(st$pos_src %in% L[1] ~ st$fish_lat, st$pos_src %in% L[2] ~ st$g_lat, st$pos_src %in% L[3] ~ st$land_lat,
                             st$pos_src %in% L[4] ~ st$s_lat, TRUE ~ NA_real_)
  st$lon <- dplyr::case_when(st$pos_src %in% L[1] ~ st$fish_lon, st$pos_src %in% L[2] ~ st$g_lon, st$pos_src %in% L[3] ~ st$land_lon,
                             st$pos_src %in% L[4] ~ st$s_lon, TRUE ~ NA_real_)
  st$drop <- dplyr::case_when(is.na(st$date_c) ~ "no date", is.na(st$pos_src) ~ "no usable position", TRUE ~ NA_character_)
  dropped <- st[!is.na(st$drop), c("run", "row", "site_key", "unit", "date_c", "gear_c", "drop", "n_rows")]
  names(dropped) <- c("block", "first_row", "landing_site", "fishing_unit", "date", "gear", "reason", "rows")
  keep <- st[is.na(st$drop), ] %>% arrange(date_c, site_key, row)
  if (!nrow(keep)) stop("No station has both a date and a usable position; check the date and position columns.")
  keep$serialnumber <- seq_len(nrow(keep))
  note("Positions: %s.", paste(sprintf("%d from %s", as.integer(table(factor(keep$pos_src, L))), L), collapse = ", "))
  if (n_sw) note("%d position(s) with latitude and longitude swapped were corrected.", n_sw)
  if (any(far)) note("%d fishing position(s) more than %g km from their landing site were not used.", sum(far), opts$max_far_km)
  note("%d of %d station block(s) dropped (%s); %d kept.", nrow(dropped), nrow(st),
       if (nrow(dropped)) paste(sprintf("%d %s", as.integer(table(dropped$reason)), names(table(dropped$reason))), collapse = ", ") else "none",
       nrow(keep))

  gmap <- opts$gear_map
  gcode <- unname(gmap[keep$gear_c])
  gcode[!nzchar(gcode)] <- NA
  if (any(is.na(gcode))) note("%d station(s) have a gear with no code (%s); their gear is left empty.", sum(is.na(gcode)),
                              paste(unique(keep$gear_c[is.na(gcode)]), collapse = ", "))
  gname <- ifelse(keep$gear_c %in% names(LANDINGS_GEAR_NAMES), LANDINGS_GEAR_NAMES[keep$gear_c], keep$gear_c)
  is_test <- !is.na(keep$remarks) & grepl("test fishing", tolower(keep$remarks))
  no_na <- function(x) ifelse(is.na(x), NA_character_, as.character(x))
  fishstations <- data.frame(
    serialnumber = as.character(keep$serialnumber), nation = opts$nation, station = as.character(keep$serialnumber),
    stationstartdate = paste0(format(keep$date_c, "%Y-%m-%d"), "Z"), stationstarttime = opts$start_time,
    stationtype = ifelse(is_test, opts$type_test, opts$type_other),
    latitudestart = sprintf("%.5f", keep$lat), longitudestart = sprintf("%.5f", keep$lon),
    gear = gcode, gearcount = "1",
    soaktime = no_na(ifelse(!is.na(keep$fish_h) & keep$fish_h > 0 & keep$fish_h <= 24, keep$fish_h, NA)),
    landingsite = landings_titlecase(keep$site_key), fishingground = keep$ground, haulvalidity = "1",
    stationcomment = paste0(gname, "; fishing unit ", keep$unit,
                            ifelse(is.na(keep$mesh) | keep$mesh == 0, "", paste0("; mesh ", keep$mesh, " mm")),
                            ifelse(is.na(keep$nlamps) | keep$nlamps == 0, "",
                                   paste0("; ", keep$nlamps, " lamps", ifelse(is.na(keep$watts) | keep$watts == 0, "", paste0(", ", keep$watts, " W")))),
                            "; position from ", keep$pos_src, "; time of day not recorded (nominal ", sub("Z$", "", opts$start_time), ")"),
    stringsAsFactors = FALSE)

  # --- fish and catch samples (one per station and species)
  fish <- d[d$run %in% keep$run & !is.na(d$sp) & !is.na(d$tl) & d$tl > 0, ] %>%
    left_join(keep[, c("run", "serialnumber")], by = "run")
  in_keep <- d$run %in% keep$run
  note("Fish: %d kept; left out: %d of other or unnamed species, %d without a length, %d in dropped stations.", nrow(fish),
       sum(in_keep & !is.na(d$tl) & is.na(d$sp)), sum(in_keep & !is.na(d$sp) & (is.na(d$tl) | d$tl <= 0)),
       sum(!in_keep & !is.na(d$sp) & !is.na(d$tl)))

  tot <- function(cols, df) { v <- as.matrix(df[, cols, drop = FALSE]); s <- rowSums(v, na.rm = TRUE); s[rowSums(!is.na(v)) == 0] <- NA; s }
  totals <- do.call(rbind, lapply(names(LANDINGS_CATCH_COLUMNS), function(sp)
    data.frame(serialnumber = keep$serialnumber, sp = sp, catch_kg = tot(LANDINGS_CATCH_COLUMNS[[sp]], keep), stringsAsFactors = FALSE)))

  # Length-weight fit per species; fish measured but not weighed get a predicted
  # weight, so every sample has a complete sample weight to raise with
  has_w <- !is.na(fish$w) & fish$w > 0
  lw_tab <- data.frame(species = character(0), n = integer(0), a = numeric(0), b = numeric(0), r2 = numeric(0))
  fish$w_pred <- NA_real_
  for (s in unique(fish$sp)) {
    i <- which(fish$sp == s & has_w)
    if (length(i) < 10) next
    fit <- stats::lm(log10(w) ~ I(log10(tl / 10)), data = fish[i, ])
    lw_tab <- rbind(lw_tab, data.frame(species = s, n = length(i), a = unname(stats::coef(fit)[1]), b = unname(stats::coef(fit)[2]),
                                       r2 = summary(fit)$r.squared))
    j <- which(fish$sp == s & !has_w)
    if (opts$predict_weights && length(j)) fish$w_pred[j] <- 10^stats::predict(fit, data.frame(tl = fish$tl[j]))
  }
  meas <- fish %>% group_by(serialnumber, sp) %>%
    summarise(n_len = n(), n_pred = sum(!is.na(w_pred)),
              w_kg = sum(ifelse(!is.na(w) & w > 0, w, w_pred), na.rm = TRUE) / 1000, n_unw = sum(is.na(w) | w <= 0) - sum(!is.na(w_pred)),
              .groups = "drop")
  cs <- merge(totals, meas, by = c("serialnumber", "sp"), all = TRUE)
  cs <- cs[(!is.na(cs$catch_kg) & cs$catch_kg > 0) | !is.na(cs$n_len), , drop = FALSE]
  cs <- cs[order(cs$serialnumber, match(cs$sp, LANDINGS_SPECIES$name)), , drop = FALSE]
  cs$catchsampleid <- ave(cs$serialnumber, cs$serialnumber, FUN = seq_along)
  sp_i <- match(cs$sp, LANDINGS_SPECIES$name)
  rep_kg <- ifelse(!is.na(cs$catch_kg) & cs$catch_kg > 0, cs$catch_kg, NA)
  has_fish <- !is.na(cs$n_len)
  no_total <- has_fish & is.na(rep_kg)
  raised <- has_fish & !is.na(rep_kg) & cs$w_kg > rep_kg * 1.001
  cs$catchweight <- ifelse(no_total | raised, cs$w_kg, rep_kg)
  cs$lengthsampleweight <- ifelse(has_fish & cs$n_unw <= 0, pmin(cs$w_kg, cs$catchweight), NA)
  cmt <- function(old, new) ifelse(is.na(old), new, paste(old, new, sep = "; "))
  cs$comment <- NA_character_
  pred <- has_fish & !is.na(cs$n_pred) & cs$n_pred > 0
  cs$comment[pred] <- sprintf("%d of %d fish had no weight; predicted from the length-weight fit", cs$n_pred[pred], cs$n_len[pred])
  cs$comment[raised]   <- cmt(cs$comment[raised], sprintf("reported catch %.3f kg is below the measured weight; catch weight set to %.3f kg", rep_kg[raised], cs$w_kg[raised]))
  cs$comment[no_total] <- cmt(cs$comment[no_total], "catch weight not reported; set to the measured weight (lower bound)")
  adjustments <- data.frame(serialnumber = cs$serialnumber, species = cs$sp, adjustment = cs$comment, stringsAsFactors = FALSE)
  adjustments <- adjustments[!is.na(adjustments$adjustment), , drop = FALSE]
  note("Catch samples: %d (%d weight(s) predicted, %d catch raised to the measured weight, %d catch weight missing, %d without measured fish).",
       nrow(cs), sum(pred), sum(raised), sum(no_total), sum(!has_fish))
  catchesamples <- data.frame(
    serialnumber = as.character(cs$serialnumber), catchsampleid = as.character(cs$catchsampleid),
    commonname = LANDINGS_SPECIES$common[sp_i], aphia = LANDINGS_SPECIES$aphia[sp_i], scientificname = cs$sp,
    sampletype = opts$sampletype, conservation = opts$conservation, catchproducttype = opts$producttype,
    catchweight = no_na(ifelse(is.na(cs$catchweight), NA, sprintf("%.4f", cs$catchweight))),
    sampleproducttype = opts$producttype, lengthmeasurement = opts$lengthmeasurement,
    lengthsampleweight = no_na(ifelse(is.na(cs$lengthsampleweight), NA, sprintf("%.4f", cs$lengthsampleweight))),
    lengthsamplecount = no_na(cs$n_len), specimensamplecount = no_na(cs$n_len), catchcomment = cs$comment,
    stringsAsFactors = FALSE)

  fish <- fish %>% left_join(cs[, c("serialnumber", "sp", "catchsampleid")], by = c("serialnumber", "sp")) %>%
    arrange(serialnumber, catchsampleid, row)
  fish$specimenid <- ave(fish$row, fish$serialnumber, fish$catchsampleid, FUN = seq_along)
  individuals <- data.frame(
    serialnumber = as.character(fish$serialnumber), catchsampleid = as.character(fish$catchsampleid),
    specimenid = as.character(fish$specimenid), individualproducttype = opts$producttype,
    individualweight = no_na(ifelse(!is.na(fish$w) & fish$w > 0, sprintf("%.5f", fish$w / 1000), NA)),
    lengthresolution = opts$lengthresolution, length = sprintf("%.3f", fish$tl / 1000), sex = unname(fish$sex_c),
    gonadweight = no_na(ifelse(!is.na(fish$gonad) & fish$gonad > 0, sprintf("%.5f", fish$gonad / 1000), NA)),
    individualcomment = ifelse(is.na(fish$ms), NA_character_, paste0("MS=", fish$ms)),
    stringsAsFactors = FALSE)

  missions <- data.frame(
    missiontype = opts$missiontype, startyear = format(min(keep$date_c), "%Y"), platform = opts$platform,
    missionnumber = opts$missionnumber, missiontypename = "Landing-site sampling", callsignal = opts$callsignal,
    platformname = opts$platformname, cruise = opts$cruise,
    missionstartdate = paste0(format(min(keep$date_c), "%Y-%m-%d"), "Z"),
    missionstopdate = paste0(format(max(keep$date_c), "%Y-%m-%d"), "Z"), purpose = opts$purpose, stringsAsFactors = FALSE)

  stations_audit <- data.frame(serialnumber = keep$serialnumber, first_row = keep$row, landing_site = landings_titlecase(keep$site_key),
                               fishing_unit = keep$unit, date = keep$date_c, gear = keep$gear_c, position_source = keep$pos_src,
                               rows = keep$n_rows, stringsAsFactors = FALSE)
  list(tables = list(missions = missions, fishstations = fishstations, catchesamples = catchesamples, individuals = individuals),
       report = list(notes = notes, dropped = dropped, stations = stations_audit, adjustments = adjustments, lw = lw_tab,
                     counts = c(stations = nrow(fishstations), catchsamples = nrow(catchesamples), individuals = nrow(individuals),
                                dropped_stations = nrow(dropped)),
                     gears = sort(unique(keep$gear_c))))
}

landings_zip <- function(zipfile, files) {
  if (requireNamespace("zip", quietly = TRUE)) zip::zip(zipfile, files, mode = "cherry-pick")
  else utils::zip(zipfile, files, flags = "-j -q")
}

# ---------------------------------------------------------------------------
# 4) Interface
# ---------------------------------------------------------------------------

# Code-list choices, with a default that is not on the list (the placeholder platform) added
landings_choices <- function(id, lists, value = NULL, label = NULL) {
  ch <- code_list_choices(id, lists)
  if (!is.null(value) && nzchar(value) && !value %in% ch) ch <- c(ch, stats::setNames(value, label))
  ch
}

landings_card <- function(title, ...) {
  div(class = "card mb-3", div(class = "card-header fw-semibold", title), div(class = "card-body", ...))
}

landings_ui <- function(id) {
  ns <- NS(id)
  dflt <- landings_default_options()
  lists <- code_lists_default()
  fluidPage(
    useShinyjs(),
    titlePanel("Landing-site samples → Biotic.xlsx"),
    p(class = "text-muted",
      "For catches sampled at landing sites (one row per measured fish, with the sampling information repeated on",
      " every row). Each fishing unit's sample becomes a station. Download the workbook and convert it to XML in the",
      " Biotic.xls_To_Biotic.xml tab."),
    fluidRow(
      column(4,
        landings_card("1. Landing-site workbook",
          fileInput(ns("file"), NULL, accept = ".xlsx", buttonLabel = "Browse…", placeholder = "Landing-site sheet (.xlsx)"),
          uiOutput(ns("file_status"))
        ),
        landings_card("2. Mission",
          selectizeInput(ns("nation"), "Nation (code)", choices = code_list_choices("nation", lists), selected = dflt$nation,
                         options = list(create = TRUE, placeholder = "Choose or type a code")),
          selectizeInput(ns("missiontype"), "Mission type (code)", choices = code_list_choices("missiontype", lists), selected = dflt$missiontype,
                         options = list(create = TRUE, placeholder = "Choose or type a code")),
          selectizeInput(ns("platform"), "Platform (code)", choices = landings_choices("platform", lists, dflt$platform, paste(dflt$platform, "- placeholder (no vessel)")), selected = dflt$platform,
                         options = list(create = TRUE, placeholder = "Choose or type a code")),
          fluidRow(column(6, textInput(ns("cruise"), "Cruise", dflt$cruise)),
                   column(6, textInput(ns("missionnumber"), "Mission number", dflt$missionnumber))),
          textInput(ns("purpose"), "Purpose", dflt$purpose),
          helpText("There is no vessel: the platform code is a placeholder until one is agreed.")
        ),
        landings_card("3. Stations and gear",
          textInput(ns("start_time"), "Station time of day (not recorded)", sub("Z$", "", dflt$start_time)),
          fluidRow(column(6, selectizeInput(ns("type_other"), "Station type", choices = code_list_choices("fishstationtype", lists, blank = FALSE),
                                            selected = dflt$type_other)),
                   column(6, selectizeInput(ns("type_test"), "…for \"Test fishing\"", choices = code_list_choices("fishstationtype", lists, blank = FALSE),
                                            selected = dflt$type_test))),
          uiOutput(ns("gear_inputs")),
          helpText("Gear codes: the NMD gear list has none for these gears, so the defaults are custom codes.")
        ),
        landings_card("4. Options",
          checkboxInput(ns("fallback"), "Use the fishing ground's or landing site's position when the fishing location is missing", TRUE),
          checkboxInput(ns("predict"), "Predict the weight of fish measured but not weighed (length-weight fit)", TRUE),
          checkboxInput(ns("land_check"), "Check positions against the maritime zones (marine only; a lake reads as land)", FALSE),
          actionButton(ns("process"), "Convert", class = "btn-primary w-100 mb-2"),
          downloadButton(ns("dl_xlsx"), "Download Biotic.xlsx", class = "btn-success w-100 mb-2"),
          downloadButton(ns("dl_report"), "Download the conversion report (ZIP)", class = "btn-outline-primary w-100")
        )
      ),
      column(8,
        tabsetPanel(id = ns("tabs"),
          tabPanel("Summary", br(), uiOutput(ns("summary"))),
          tabPanel("Stations", br(), DTOutput(ns("tbl_stations"))),
          tabPanel("Catch samples", br(), DTOutput(ns("tbl_catch"))),
          tabPanel("Fish", br(), DTOutput(ns("tbl_fish"))),
          tabPanel("Dropped stations", br(), DTOutput(ns("tbl_dropped"))),
          tabPanel("Length-weight", br(), tableOutput(ns("tbl_lw")), DTOutput(ns("tbl_adj"))),
          tabPanel("Validation", br(), uiOutput(ns("validation_summary")), DTOutput(ns("validation_table")),
                   div(class = "mt-2", downloadButton(ns("dl_findings"), "Download findings (CSV)", class = "btn-outline-secondary btn-sm")))
        )
      )
    )
  )
}

landings_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    cl <- code_lists_get(session)
    rv <- reactiveValues(res = NULL, findings = NULL)

    # Nation, mission type, platform and station type follow the active code lists
    observeEvent(cl$lists, {
      for (nm in c("nation", "missiontype", "platform")) {
        updateSelectizeInput(session, nm, choices = if (nm == "platform") landings_choices(nm, cl$lists, "9999", "9999 - placeholder (no vessel)") else code_list_choices(nm, cl$lists),
                             selected = isolate(input[[nm]]),
                             options = list(create = TRUE, placeholder = "Choose or type a code"))
      }
      for (nm in c("type_other", "type_test")) {
        updateSelectizeInput(session, nm, choices = code_list_choices("fishstationtype", cl$lists, blank = FALSE), selected = isolate(input[[nm]]))
      }
    }, ignoreInit = TRUE)

    sheet <- reactive({
      req(input$file)
      res <- tryCatch(landings_read(input$file$datapath), error = function(e) e)
      if (inherits(res, "error")) {
        showNotification(paste("The workbook could not be read:", conditionMessage(res)), type = "error", duration = 10)
        req(FALSE)
      }
      res
    })
    observeEvent(input$file, { rv$res <- NULL; rv$findings <- NULL })

    output$file_status <- renderUI({
      if (is.null(input$file)) return(helpText("The first sheet is read; its columns are found by their header text."))
      s <- sheet()
      found <- sum(!is.na(s$map))
      tagList(
        p(class = "small mb-1", sprintf("%d rows, %d of %d expected columns recognised.", nrow(s$data), found, length(s$map))),
        if (length(s$missing)) div(class = "alert alert-danger small", strong("Required column(s) not found: "),
                                   paste(s$missing, collapse = ", "), ". Check the header text."),
        if (any(is.na(s$map)) && !length(s$missing))
          p(class = "small text-muted", "Not found (left empty): ", paste(names(s$map)[is.na(s$map)], collapse = ", "))
      )
    })

    # One code box per gear found in the sheet
    gears_found <- reactive({
      g <- toupper(landings_squash(sheet()$data$gear)); sort(unique(g[!is.na(g)]))
    })
    output$gear_inputs <- renderUI({
      if (is.null(input$file)) return(NULL)
      tagList(lapply(gears_found(), function(g) {
        old <- isolate(input[[paste0("gear_", g)]])
        textInput(session$ns(paste0("gear_", g)),
                  sprintf("Gear %s%s → Biotic gear code", g, if (g %in% names(LANDINGS_GEAR_NAMES) && g != LANDINGS_GEAR_NAMES[[g]]) paste0(" (", LANDINGS_GEAR_NAMES[[g]], ")") else ""),
                  value = if (!is.null(old)) old else if (g %in% names(LANDINGS_GEAR_DEFAULTS)) LANDINGS_GEAR_DEFAULTS[[g]] else "")
      }))
    })

    options_now <- reactive({
      gm <- vapply(gears_found(), function(g) { v <- input[[paste0("gear_", g)]]; if (is.null(v)) "" else trimws(v) }, "")
      list(nation = input$nation, missiontype = input$missiontype, platform = input$platform, missionnumber = input$missionnumber,
           cruise = input$cruise, purpose = input$purpose, start_time = paste0(sub("Z$", "", trimws(input$start_time)), "Z"),
           type_other = input$type_other, type_test = input$type_test, gear_map = gm,
           fallback_positions = isTRUE(input$fallback), predict_weights = isTRUE(input$predict))
    })

    observeEvent(input$process, {
      s <- sheet()
      if (length(s$missing)) {
        showNotification(paste("Required column(s) not found:", paste(s$missing, collapse = ", ")), type = "error", duration = 8)
        return()
      }
      res <- withProgress(message = "Converting…", value = 0.3, tryCatch(landings_to_biotic(s$data, options_now()), error = function(e) e))
      if (inherits(res, "error")) {
        showNotification(conditionMessage(res), type = "error", duration = 12)
        return()
      }
      tabs <- lapply(res$tables, as.data.frame)
      f <- withProgress(message = "Validating…", value = 0.6,
                        validate_biotic(tabs, cl$lists, check_land = isTRUE(input$land_check)))
      rv$res <- res; rv$findings <- f
      updateTabsetPanel(session, "tabs", selected = "Summary")
    })

    # --- outputs
    output$summary <- renderUI({
      r <- rv$res
      if (is.null(r)) return(p(class = "text-muted", "Load a workbook, check the settings and click Convert."))
      cn <- r$report$counts
      cards <- c(Stations = "stations", `Catch samples` = "catchsamples", Fish = "individuals", `Dropped stations` = "dropped_stations")
      tagList(
        div(class = "d-flex flex-wrap gap-3 mb-3",
            lapply(names(cards), function(lbl) div(class = "border rounded px-3 py-2",
              div(class = "fs-4 fw-semibold", format(cn[[cards[[lbl]]]], big.mark = ",")), div(class = "small text-muted", lbl)))),
        h6("What was done"), tags$ul(lapply(r$report$notes, tags$li)),
        if (!is.null(rv$findings)) tagList(h6("Checks"), findings_badges(rv$findings)),
        p(class = "small text-muted mt-3", "Download Biotic.xlsx and open it in the Biotic.xls_To_Biotic.xml tab to build and validate the XML.")
      )
    })
    prev <- function(x) datatable(x, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
    output$tbl_stations <- renderDT({ req(rv$res); prev(rv$res$tables$fishstations) })
    output$tbl_catch    <- renderDT({ req(rv$res); prev(rv$res$tables$catchesamples) })
    output$tbl_fish     <- renderDT({ req(rv$res); prev(utils::head(rv$res$tables$individuals, 2000)) })
    output$tbl_dropped  <- renderDT({ req(rv$res); prev(rv$res$report$dropped) })
    output$tbl_lw <- renderTable({ req(rv$res); x <- rv$res$report$lw; if (nrow(x)) { names(x) <- c("Species", "Fish", "a", "b", "R²"); x } else NULL },
                                 striped = TRUE, spacing = "xs", digits = 3)
    output$tbl_adj <- renderDT({ req(rv$res); prev(rv$res$report$adjustments) })
    output$validation_summary <- renderUI({
      if (is.null(rv$findings)) return(p(class = "text-muted small", "Convert a workbook to see the data checks."))
      tagList(p(class = "small mb-1", "Data checks on the converted tables. The schema and the StoX read-back run in the Biotic XML tab."),
              findings_badges(rv$findings))
    })
    output$validation_table <- renderDT({ req(rv$findings); findings_dt(rv$findings) })
    output$dl_findings <- downloadHandler(filename = function() paste0("Landings_validation_", Sys.Date(), ".csv"),
      content = function(file) utils::write.csv(if (is.null(rv$findings)) data.frame() else rv$findings, file, row.names = FALSE))

    observe({
      shinyjs::toggleState("dl_xlsx", condition = !is.null(rv$res))
      shinyjs::toggleState("dl_report", condition = !is.null(rv$res))
    })
    output$dl_xlsx <- downloadHandler(
      filename = function() "Biotic.xlsx",
      content = function(file) { req(rv$res); writexl::write_xlsx(rv$res$tables, file) }
    )
    output$dl_report <- downloadHandler(
      filename = function() format(Sys.time(), "Landings_report_%Y%m%d_%H%M.zip"),
      content = function(file) {
        req(rv$res)
        dir <- file.path(tempdir(), paste0("landings_", session$token)); dir.create(dir, showWarnings = FALSE)
        r <- rv$res$report
        w <- function(x, f) utils::write.csv(x, file.path(dir, f), row.names = FALSE)
        w(r$dropped, "dropped_stations.csv"); w(r$stations, "stations_and_source_rows.csv"); w(r$adjustments, "catch_adjustments.csv")
        w(r$lw, "length_weight_fits.csv"); if (!is.null(rv$findings)) w(rv$findings, "validation_findings.csv")
        w(data.frame(sheet_code = names(options_now()$gear_map), biotic_gear_code = unname(options_now()$gear_map)), "gear_codes_used.csv")
        writeLines(r$notes, file.path(dir, "conversion_notes.txt"))
        landings_zip(file, list.files(dir, full.names = TRUE))
      },
      contentType = "application/zip"
    )
  })
}

ui <- landings_ui("landings")
server <- function(input, output, session) landings_server("landings")

# Launch the application
shinyApp(ui, server)
