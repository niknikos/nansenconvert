# Taxonomy for the converters, from nansentaxonomy (the EAF-Nansen taxonomy
# built from WoRMS). Replaces the old bundled WorMS.xlsx file.
#
# - taxonomy_reference(): one row per scientific name, with aphia (AphiaID)
#   and catchcategory (ITIS TSN) from the nansentaxonomy crosswalk
# - taxonomy_resolve():   aphia / catchcategory for recorded names, saying how
#   each name was matched
# - taxonomy_suggest():   closest known name for likely typos
# - taxonomy_check_worms(): live check against WoRMS (needs internet)

suppressPackageStartupMessages(library(nansentaxonomy))

.taxonomy_cache <- new.env()

#' One row per scientific name: scientificname, aphia, catchcategory, class,
#' family, rank, status, valid_name (the shape the converters' WoRMS dialog uses)
taxonomy_reference <- function() {
  if (!is.null(.taxonomy_cache$ref)) return(.taxonomy_cache$ref)
  tx <- as.data.frame(nansentaxonomy::taxonomy)
  tx <- tx[!is.na(tx$scientificname) & nzchar(tx$scientificname), ]
  tx <- tx[!duplicated(tx$scientificname), ]
  ids <- nansentaxonomy::resolve_catchcategory(tx$scientificname)
  ref <- data.frame(
    scientificname = tx$scientificname,
    aphia          = as.character(ifelse(is.na(ids$AphiaID), tx$AphiaID, ids$AphiaID)),
    catchcategory  = as.character(ids$tsn),
    class          = tx$class,
    family         = tx$family,
    rank           = tx$rank,
    status         = tx$status,
    valid_name     = tx$valid_name,
    stringsAsFactors = FALSE
  )
  .taxonomy_cache$ref <- ref
  ref
}

#' aphia and catchcategory for recorded names, in input order:
#' input, matched_taxon, aphia, catchcategory, is_placeholder_tsn, matched_by, note
taxonomy_resolve <- function(names) {
  names <- as.character(names)
  r <- as.data.frame(nansentaxonomy::resolve_catchcategory(names))
  ref <- taxonomy_reference()
  # Names in the taxonomy but not in the TSN crosswalk still get their AphiaID
  miss <- is.na(r$AphiaID)
  hit <- match(tolower(trimws(names[miss])), tolower(ref$scientificname))
  r$AphiaID[miss] <- suppressWarnings(as.integer(ref$aphia[hit]))
  r$matched_by[miss & !is.na(r$AphiaID)] <- "taxonomy"
  data.frame(
    input              = names,
    matched_taxon      = r$matched_taxon,
    aphia              = as.character(r$AphiaID),
    catchcategory      = as.character(r$tsn),
    is_placeholder_tsn = r$is_placeholder_tsn,
    matched_by         = r$matched_by,
    note               = r$note,
    stringsAsFactors   = FALSE
  )
}

#' Closest known scientific name for each name (likely typos), or NA
taxonomy_suggest <- function(names, max_distance = 2) {
  names <- unique(as.character(names))
  out <- stats::setNames(rep(NA_character_, length(names)), names)
  if (!length(names)) return(out)
  s <- tryCatch(as.data.frame(nansentaxonomy::suggest_similar_species(names, max_distance = max_distance,
                                                                      max_suggestions = 1)),
                error = function(e) NULL)
  if (is.null(s) || !nrow(s)) return(out)
  s <- s[!is.na(s$suggestion), , drop = FALSE]
  s <- s[!duplicated(s$query), , drop = FALSE]
  out[s$query] <- s$suggestion
  out
}

#' Live WoRMS check (fuzzy match on the WoRMS server). Returns a data.frame:
#' query, worms_status, valid_name, valid_AphiaID, status, rank, note;
#' or a character error message when WoRMS cannot be reached
taxonomy_check_worms <- function(names) {
  names <- unique(as.character(names[!is.na(names) & nzchar(names)]))
  if (!length(names)) return(data.frame())
  res <- tryCatch(
    nansentaxonomy::validate_against_worms(names, marine_only = FALSE, verbose = FALSE),
    error = function(e) conditionMessage(e)
  )
  if (is.character(res)) return(paste("WoRMS could not be checked:", res))
  res <- as.data.frame(res)
  keep <- intersect(c("query", "worms_status", "scientificname", "valid_name", "valid_AphiaID",
                      "status", "rank", "ambiguous", "note"), names(res))
  res[, keep, drop = FALSE]
}

#' Names given in a CSV for promotion to the next nansentaxonomy release
taxonomy_new_names_csv <- function(worms_rows, path) {
  utils::write.csv(worms_rows, path, row.names = FALSE, na = "")
}
