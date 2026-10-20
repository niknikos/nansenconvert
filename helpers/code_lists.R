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
