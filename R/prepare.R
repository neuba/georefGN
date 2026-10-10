#' Prepare occurrence records
#'
#' Standardises a GBIF download (data frame, result of `rgbif::occ_data()` or
#' `rgbif::occ_search()`) for the georeferencing workflow: missing Darwin Core
#' columns are created, dates are parsed with their precision (day, month or
#' year), the collector key and the collection number are extracted.
#'
#' Used columns: `species`, `countryCode`, `decimalLatitude`, `decimalLongitude`,
#' `coordinateUncertaintyInMeters`, `locality`, `verbatimLocality`,
#' `municipality`, `county`, `eventDate`, `year`, `month`, `day`, `recordedBy`,
#' `recordNumber`. Other columns are kept.
#'
#' @param x Occurrence records.
#' @param params Parameters from [georef_params()] (unused for now, kept for consistency).
#' @return A data frame with added columns `.rid` (row id), `annee`, `mois`,
#'   `jour`, `t` (days since 1970), `tprec` (date precision), `tdemi`,
#'   `coll` and `coll_all` (collector keys, see [parse_collectors()]), `num_coll`,
#'   `num_full`, `serie`, `dup_key` (herbarium duplicate key), `loc_txt0`, `date_source`.
#' @export
#' @examples
#' ex <- georef_example()
#' occ <- prepare_occurrences(ex$occurrences)
#' table(occ$tprec, useNA = "ifany")
prepare_occurrences <- function(x, params = georef_params()) {
  if (!is.data.frame(x)) {
    if (!is.null(x$data)) x <- x$data
    else x <- dplyr::bind_rows(lapply(x, function(z) z$data))
  }
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  need <- c("species", "countryCode", "decimalLatitude", "decimalLongitude",
            "coordinateUncertaintyInMeters", "locality", "verbatimLocality",
            "municipality", "county", "stateProvince", "eventDate", "year", "recordedBy", "recordNumber", "month", "day")
  for (v in need) if (!v %in% names(x)) x[[v]] <- NA
  x$decimalLatitude  <- suppressWarnings(as.numeric(x$decimalLatitude))
  x$decimalLongitude <- suppressWarnings(as.numeric(x$decimalLongitude))
  x$coordinateUncertaintyInMeters <- suppressWarnings(as.numeric(x$coordinateUncertaintyInMeters))
  x$eventDate    <- as.character(x$eventDate)
  x$recordNumber <- as.character(x$recordNumber)
  x$recordedBy   <- as.character(x$recordedBy)
  x$countryCode  <- toupper(as.character(x$countryCode))
  x$year <- dplyr::coalesce(suppressWarnings(as.integer(x$year)),
                            suppressWarnings(as.integer(substr(x$eventDate, 1, 4))))
  x$.rid <- seq_len(nrow(x))

  an_max <- as.integer(format(Sys.Date(), "%Y"))
  x$annee <- as.integer(ifelse(!is.na(x$year) & x$year >= 1700 & x$year <= an_max, x$year, NA))
  mois_ed <- suppressWarnings(as.integer(ifelse(grepl("^[0-9]{4}-[0-9]{2}", x$eventDate), substr(x$eventDate, 6, 7), NA)))
  x$mois <- dplyr::coalesce(suppressWarnings(as.integer(x$month)), mois_ed)
  x$mois[!is.na(x$mois) & (x$mois < 1 | x$mois > 12)] <- NA
  j1 <- suppressWarnings(as.Date(substr(x$eventDate, 1, 10), format = "%Y-%m-%d"))
  j2 <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", x$annee, x$mois,
                                         suppressWarnings(as.integer(x$day))), format = "%Y-%m-%d"))
  x$jour <- dplyr::coalesce(j1, j2)
  x$jour[!is.na(x$jour) & (is.na(x$annee) | as.integer(format(x$jour, "%Y")) != x$annee)] <- NA
  tt <- time_num(x$jour, x$annee, x$mois)
  x$t <- tt$t; x$tprec <- tt$p; x$tdemi <- 0

  cl <- parse_collectors(x$recordedBy)
  x$coll <- cl$coll; x$coll_all <- cl$coll_all
  x$loc_txt0 <- loc_text(x)
  num <- parse_record_number(x$recordNumber, x$annee)
  x$num_coll <- num$number
  x$num_full <- num$full
  x$serie <- ifelse(is.na(x$coll) | is.na(x$num_coll), NA_character_, paste(x$coll, num$prefix, sep = "#"))
  x$dup_key <- ifelse(is.na(x$coll) | is.na(x$num_full), NA_character_, paste(x$coll, x$num_full, sep = "#"))
  x$date_source <- ifelse(x$tprec %in% 0, "date", ifelse(!is.na(x$tprec), "date_imprecise", NA_character_))
  x$date_deduite <- as.Date(NA); x$date_deduite_max <- as.Date(NA)
  x
}
