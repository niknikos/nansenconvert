# Code lists shared by the converters.
#
# "Nansen" lists follow the NMD reference catalogue (nansencore) and the
# EAF-Nansen acoustic categories (nansentaxonomy). Sourced by the converter
# scripts; safe to source more than once.

suppressPackageStartupMessages({
  library(nansencore)
  library(nansentaxonomy)  # attached: some of its functions read its datasets
})

# ---------------------------------------------------------------------------
# Acoustic categories (acocat)
# ---------------------------------------------------------------------------

# Group labels used in Echoview region classes instead of a species name
ACOCAT_GROUP_LABELS <- c(
  "autres"        = "OTHER", "others"      = "OTHER", "other"  = "OTHER",
  "unassigned"    = "OTHER",
  "plancton"      = "PLANK", "plankton"    = "PLANK",
  "mesopelagique" = "MESFI", "mesopelagic" = "MESFI"
)

#' The EAF-Nansen acoustic categories: acousticCategory, acocat, description
acocat_categories <- function() {
  cats <- as.data.frame(nansentaxonomy::acousticCategories)
  cats[order(cats$sortOrder), c("acousticCategory", "acocat", "parentCategory", "description", "sortOrder")]
}

#' acocat for each species or group label, from nansentaxonomy.
#'
#' A species can belong to a specific category (e.g. PILCH) and to its broad
#' parent (PEL1). level = "specific" keeps the specific one when there is one;
#' "broad" always uses the broad category. Names with no category -> OTHER.
#' @return data.frame(name, acousticCategory, acocat, source), one row per name
acocat_nansen <- function(names, level = c("specific", "broad")) {
  level <- match.arg(level)
  names <- as.character(names)
  cats <- acocat_categories()
  # PLANK is listed under PEL1 for display only; plankton NASC must never be
  # counted as small pelagic fish, so it stays its own category
  broad_of <- function(cat) {
    p <- cats$parentCategory[match(cat, cats$acousticCategory)]
    ifelse(is.na(p) | cat %in% "PLANK", cat, p)
  }
  tx <- as.data.frame(nansentaxonomy::taxonomy)[, c("scientificname", "acousticCategory")]
  tx <- tx[!is.na(tx$acousticCategory) & nzchar(tx$acousticCategory), ]
  tx$key <- tolower(trimws(tx$scientificname))
  tx$is_specific <- !is.na(cats$parentCategory[match(tx$acousticCategory, cats$acousticCategory)])
  tx$sort <- cats$sortOrder[match(tx$acousticCategory, cats$acousticCategory)]

  pick_one <- function(key) {
    rows <- tx[tx$key == key, , drop = FALSE]
    if (!nrow(rows)) return(NA_character_)
    if (level == "specific") {
      rows <- rows[order(!rows$is_specific, rows$sort), ]
      rows$acousticCategory[1]
    } else {
      b <- unique(broad_of(rows$acousticCategory))
      b[order(cats$sortOrder[match(b, cats$acousticCategory)])][1]
    }
  }

  u <- unique(names)
  key <- tolower(trimws(u))
  cat_u <- vapply(key, pick_one, character(1))
  src <- ifelse(is.na(cat_u), NA_character_, "nansentaxonomy")
  grp <- is.na(cat_u) & key %in% names(ACOCAT_GROUP_LABELS)
  cat_u[grp] <- ACOCAT_GROUP_LABELS[key[grp]]
  src[grp] <- "group label"
  if (level == "broad") cat_u <- broad_of(cat_u)
  none <- is.na(cat_u)
  cat_u[none] <- "OTHER"
  src[none] <- "not in taxonomy (OTHER)"

  out <- data.frame(
    name = u,
    acousticCategory = unname(cat_u),
    acocat = cats$acocat[match(cat_u, cats$acousticCategory)],
    source = unname(src),
    stringsAsFactors = FALSE
  )
  out[match(names, out$name), , drop = FALSE]
}

# ---------------------------------------------------------------------------
# Sex
# ---------------------------------------------------------------------------

#' NMD sex codes from field values: F -> 1 (Female), M -> 2 (Male).
#' "I" (undetermined) and anything unrecognised -> NA; numeric codes kept.
sex_nansen <- function(x) {
  v <- toupper(trimws(as.character(x)))
  out <- rep(NA_character_, length(v))
  out[v %in% c("F", "FEMALE", "FEMELLE")] <- "1"
  out[v %in% c("M", "MALE")] <- "2"
  num <- !is.na(v) & grepl("^[1-4]$", v)
  out[num] <- v[num]
  out
}

# ---------------------------------------------------------------------------
# Code-list registry: every coded field the converters write
# ---------------------------------------------------------------------------
#
# Each list has two modes:
#   "nansen" - the NMD reference list (nansencore) or, for acocat, the
#              EAF-Nansen acoustic categories (nansentaxonomy);
#   "custom" - a table the user defines: value (what appears in the input or
#              is chosen in the app) -> code (written to the XML) + description.
# "mapped" lists translate field values (species names, F/M, stages) to codes.

CODE_LIST_DEFS <- data.frame(
  id = c("acocat", "sex", "maturationstage", "nation", "platform", "missiontype",
         "fishstationtype", "gear", "haulvalidity", "sampletype", "lengthmeasurement",
         "conservation", "producttype", "lengthresolution"),
  label = c("Acoustic category (acocat)", "Sex", "Maturation stage", "Nation", "Platform",
            "Mission type", "Station type", "Gear", "Haul validity", "Sample type",
            "Length measurement", "Conservation", "Product type", "Length resolution"),
  used_in = c(
    "NASC -> Acoustic: Species_name -> acocat",
    "Individuals: sexe (F / M / I) -> sex",
    "Individuals: sms -> maturationstage",
    "Station defaults; NASC metadata",
    "Mission metadata; NASC metadata",
    "Mission metadata",
    "Station defaults",
    "Station defaults",
    "Station defaults",
    "Biotic sample defaults",
    "Biotic sample defaults",
    "Catch sample defaults",
    "Catch, sample and individual product type",
    "Individuals"
  ),
  mapped = c(TRUE, TRUE, TRUE, rep(FALSE, 11)),
  stringsAsFactors = FALSE
)

# The acocat codes used by the original LPA converter, offered as a starting
# point for a custom list (StoX projects built on the old files use them)
ACOCAT_LEGACY_LPA <- data.frame(
  value = c("Sardina pilchardus", "Engraulis encrasicolus", "Scomber colias",
            "Trachurus trachurus", "Trachurus trecae", "Trachurus picturatus",
            "Trachurus mediterraneus", "Sardinella aurita", "Sardinella maderensis",
            "Macroramphosus scolopax", "Macroramphosus gracilis", "Scomber scombrus",
            "Caranx rhonchus", "Plancton", "Mesopelagique", "Autres", "*"),
  code = as.character(c(1:16, 16)),
  description = c("Sardina pilchardus", "Engraulis encrasicolus", "Scomber colias",
                  "Trachurus trachurus", "Trachurus trecae", "Trachurus picturatus",
                  "Trachurus mediterraneus", "Sardinella aurita", "Sardinella maderensis",
                  "Macroramphosus scolopax", "Macroramphosus gracilis", "Scomber scombrus",
                  "Caranx rhonchus", "Plankton", "Mesopelagic", "Others",
                  "Any other name"),
  stringsAsFactors = FALSE
)

.nmd_reference_cache <- new.env()
nmd_reference <- function() {
  if (is.null(.nmd_reference_cache$ref)) {
    .nmd_reference_cache$ref <- tryCatch(nansencore::load_nmd_reference(), error = function(e) list())
  }
  .nmd_reference_cache$ref
}

empty_code_table <- function() {
  data.frame(value = character(0), code = character(0), description = character(0),
             stringsAsFactors = FALSE)
}

#' The Nansen list for one code list: data.frame(value, code, description)
code_list_nansen <- function(id) {
  if (id == "acocat") {
    cats <- acocat_categories()
    return(data.frame(value = cats$acousticCategory, code = as.character(cats$acocat),
                      description = cats$description, stringsAsFactors = FALSE))
  }
  ref <- nmd_reference()[[id]]
  if (is.null(ref)) return(empty_code_table())
  tab <- data.frame(value = as.character(ref$code), code = as.character(ref$code),
                    description = as.character(ref$description), stringsAsFactors = FALSE)
  if (id == "sex") {
    tab <- rbind(data.frame(value = c("F", "M"), code = c("1", "2"),
                            description = c("Female", "Male"), stringsAsFactors = FALSE), tab)
  }
  tab
}

#' Starting table for a new custom list
code_list_custom_default <- function(id) {
  if (id == "acocat") ACOCAT_LEGACY_LPA else code_list_nansen(id)
}

#' Default state: every list in Nansen mode
code_lists_default <- function() {
  stats::setNames(lapply(CODE_LIST_DEFS$id, function(id) {
    list(mode = "nansen", custom = code_list_custom_default(id))
  }), CODE_LIST_DEFS$id)
}

code_list_mode <- function(id, lists = NULL) {
  m <- lists[[id]]$mode
  if (is.null(m) || !m %in% c("nansen", "custom")) "nansen" else m
}

#' The active table (Nansen or custom) for one list
code_list_table <- function(id, lists = NULL) {
  if (code_list_mode(id, lists) == "custom") {
    tab <- lists[[id]]$custom
    if (is.null(tab)) return(empty_code_table())
    tab <- as.data.frame(tab, stringsAsFactors = FALSE)
    for (cn in c("value", "code", "description")) {
      tab[[cn]] <- if (cn %in% names(tab)) trimws(as.character(tab[[cn]])) else NA_character_
    }
    tab <- tab[!is.na(tab$code) & nzchar(tab$code), c("value", "code", "description"), drop = FALSE]
    tab$value[is.na(tab$value) | !nzchar(tab$value)] <- tab$code[is.na(tab$value) | !nzchar(tab$value)]
    return(tab)
  }
  code_list_nansen(id)
}

#' Named choices "code - description" = code, for selectInputs
code_list_choices <- function(id, lists = NULL, blank = TRUE) {
  tab <- code_list_table(id, lists)
  tab <- tab[!duplicated(tab$code), , drop = FALSE]
  ch <- stats::setNames(tab$code, paste0(tab$code, " - ", tab$description))
  if (blank) c("(none)" = "", ch) else ch
}

#' TRUE where a code is in the active list (NA and "" count as valid: absent)
code_list_valid <- function(id, codes, lists = NULL) {
  codes <- trimws(as.character(codes))
  empty <- is.na(codes) | !nzchar(codes) | codes == "NA"
  empty | codes %in% code_list_table(id, lists)$code
}

#' Translate field values to codes with the active list.
#'
#' Custom lists match the value column ignoring case; a "*" row is the code
#' for any value not listed. Nansen lists use the NMD codes directly, sex also
#' accepts F/M, and acocat comes from nansentaxonomy (see acocat_nansen()).
#' @return character vector of codes, NA where a value has no code
code_list_map <- function(id, values, lists = NULL, acocat_level = "specific") {
  values <- as.character(values)
  mode <- code_list_mode(id, lists)
  if (mode == "nansen") {
    if (id == "acocat") return(as.character(acocat_nansen(values, acocat_level)$acocat))
    if (id == "sex") return(sex_nansen(values))
  }
  tab <- code_list_table(id, lists)
  key <- tolower(trimws(values))
  out <- tab$code[match(key, tolower(trimws(tab$value)))]
  wild <- tab$code[tab$value == "*"]
  if (mode == "custom" && length(wild)) {
    out[is.na(out) & !is.na(values) & nzchar(trimws(values))] <- wild[1]
  }
  out
}

#' Size of one length class in cm for a lengthresolution code, read from the
#' list's description ("5 mm" -> 0.5); NA when it cannot be read
length_resolution_cm <- function(code, lists = NULL) {
  tab <- code_list_table("lengthresolution", lists)
  d <- tab$description[match(as.character(code), tab$code)]
  if (!length(d) || is.na(d)) return(NA_real_)
  m <- regmatches(d, regexec("^\\s*([0-9.]+)\\s*(mm|cm)\\s*$", d))[[1]]
  if (length(m) < 3) return(NA_real_)
  as.numeric(m[2]) / if (m[3] == "mm") 10 else 1
}

#' acocat mapping in the form build_acoustic_sheets() takes
acocat_map_for <- function(names, lists = NULL, acocat_level = "specific") {
  if (code_list_mode("acocat", lists) == "nansen") return(acocat_nansen(names, acocat_level))
  tab <- code_list_table("acocat", lists)
  u <- unique(as.character(names))
  code <- code_list_map("acocat", u, lists)
  direct <- tolower(trimws(u)) %in% tolower(trimws(tab$value))
  data.frame(
    name = u,
    acousticCategory = tab$description[match(code, tab$code)],
    acocat = code,
    source = ifelse(direct, "custom list", ifelse(is.na(code), "not in custom list", "custom list (*)")),
    stringsAsFactors = FALSE
  )
}

#' Code-list state for this session (shared by the converters through
#' session$userData); created in Nansen mode when the app runs on its own
code_lists_get <- function(session) {
  ud <- session$userData
  if (is.null(ud$code_lists)) {
    ud$code_lists <- shiny::reactiveValues(lists = code_lists_default())
  }
  ud$code_lists
}

#' Reads all custom lists and modes from a CSV (list, mode, value, code, description)
code_lists_from_csv <- function(path, lists = code_lists_default()) {
  d <- utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character", na.strings = "")
  need <- c("list", "mode", "value", "code", "description")
  miss <- setdiff(need, names(d))
  if (length(miss)) stop("Missing columns: ", paste(miss, collapse = ", "))
  for (id in intersect(unique(d$list), names(lists))) {
    rows <- d[d$list == id, , drop = FALSE]
    mode <- rows$mode[!is.na(rows$mode)][1]
    lists[[id]]$mode <- if (identical(mode, "custom")) "custom" else "nansen"
    tab <- rows[!is.na(rows$code), c("value", "code", "description"), drop = FALSE]
    if (nrow(tab)) lists[[id]]$custom <- tab
  }
  lists
}

#' Writes all modes and custom lists to a CSV
code_lists_to_csv <- function(lists, path) {
  rows <- lapply(names(lists), function(id) {
    tab <- lists[[id]]$custom
    if (is.null(tab) || !nrow(tab)) tab <- empty_code_table()[1, ]
    data.frame(list = id, mode = code_list_mode(id, lists), tab[, c("value", "code", "description")],
               stringsAsFactors = FALSE)
  })
  utils::write.csv(do.call(rbind, rows), path, row.names = FALSE, na = "")
}
