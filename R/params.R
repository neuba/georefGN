#' Parameters of the georeferencing workflow
#'
#' Builds the list of parameters used by [georeference()] and [validate_georef()].
#' Defaults reproduce the settings of the article (version 4 of the method).
#'
#' @param countries ISO 3166-1 alpha-2 codes of the countries covered by the
#'   gazetteer. Records from other countries are left untouched. Note that
#'   Benin is `"BJ"` (`"BN"` is Brunei).
#' @param fclasses GeoNames feature classes kept in the gazetteer:
#'   P (populated places), T (relief), H (hydrography), L (areas: forests, parks).
#' @param max_uncertainty_m Results with a larger uncertainty (metres) are discarded.
#' @param min_uncertainty_m Minimum uncertainty (metres) of a point taken from GeoNames.
#' @param keep_medium_confidence If `FALSE`, only points of confidence "haute" are kept.
#' @param check_coords Check the original coordinates against the GeoNames
#'   populated places of the declared country.
#' @param fix_swaps Correct latitude/longitude swaps and sign errors when the
#'   correction is unambiguous.
#' @param plausible_km A point is plausible when a GeoNames populated place of
#'   its declared country lies within this distance (km).
#' @param suspect_action `"flag"`: suspect points are kept but not used as
#'   anchors; `"regeoreference"`: they are treated as records without coordinates.
#' @param use_collector Use the collector step (phase 1).
#' @param collector_window_days Time window (+/- days) of phase 1.
#' @param anchor_spread_km Maximum spread of the anchors whose coordinates are reused.
#' @param anchor_link_km A place name is linked to an anchor when the GeoNames
#'   place is within this distance (km) of the anchor coordinates.
#' @param anchor_min_uncertainty_m Minimum uncertainty (metres) of reused coordinates.
#' @param team_anchors A record collected by a team serves as anchor for each team member.
#' @param use_duplicates Reuse the coordinates of herbarium duplicates (same
#'   collector key, same full collection number) before the collector step.
#' @param duplicate_spread_km Maximum spread of the duplicates whose coordinates are reused.
#' @param use_admin Prefer homonyms located in the administrative unit given by
#'   `stateProvince` / `county` (matched to GeoNames ADM1/ADM2 units).
#' @param city_km,admin_seat_km,stream_km,area_km,extended_relief_km Minimum
#'   uncertainty (km) of a point placed on a capital (PPLC), an administrative seat
#'   (PPLA, PPLA2), a stream, an area (class L: forest reserves, parks) or an
#'   extended relief (ranges, ridges, plateaus, valleys).
#' @param use_record_number Date undated records from their collection number.
#' @param max_numbers_per_day Maximum number of collection numbers assigned in one day.
#' @param max_number_gap Maximum gap between a number and each of its dated neighbours.
#' @param max_bracket_days Maximum interval between the two bracketing dates.
#' @param cautious_number_dates `FALSE`: the inferred date is the first possible
#'   date; `TRUE`: the whole possible interval is taken into account in phase 1.
#' @param fuzzy Enable approximate (fuzzy) place-name matching. Not recommended:
#'   in the validation it added very unreliable points. Requires 'stringdist'.
#' @param fuzzy_scope `"unresolved"` (texts without an exact match) or `"all"`.
#' @param fuzzy_jw_min Minimum Jaro-Winkler similarity.
#' @param fuzzy_min_nchar Minimum length of a name submitted to fuzzy matching.
#' @param fuzzy_min_uncertainty_m Minimum uncertainty of a fuzzy point (metres).
#' @param keep_fuzzy Keep fuzzy points in the output.
#' @param country_check Remove reconstructed points falling outside the declared
#'   country (requires 'CoordinateCleaner' and 'countrycode'; skipped otherwise).
#' @param ... Ignored; allows forward compatibility.
#'
#' @return A list of class `georef_params`.
#' @export
#' @examples
#' p <- georef_params(collector_window_days = 15)
#' p$collector_window_days
georef_params <- function(
    countries = c("CI", "GN", "LR", "SL", "ML", "BF", "GH", "BJ", "TG"),
    fclasses = c("P", "T", "H", "L"),
    max_uncertainty_m = 50000,
    min_uncertainty_m = 3000,
    keep_medium_confidence = TRUE,
    check_coords = TRUE,
    fix_swaps = TRUE,
    plausible_km = 50,
    suspect_action = c("flag", "regeoreference"),
    use_collector = TRUE,
    collector_window_days = 30,
    anchor_spread_km = 25,
    anchor_link_km = 25,
    anchor_min_uncertainty_m = 1000,
    team_anchors = TRUE,
    use_duplicates = TRUE,
    duplicate_spread_km = 10,
    use_admin = TRUE,
    city_km = 8, admin_seat_km = 5, stream_km = 25, area_km = 15, extended_relief_km = 15,
    use_record_number = TRUE,
    max_numbers_per_day = 100,
    max_number_gap = 300,
    max_bracket_days = 30,
    cautious_number_dates = FALSE,
    fuzzy = FALSE,
    fuzzy_scope = c("unresolved", "all"),
    fuzzy_jw_min = 0.93,
    fuzzy_min_nchar = 5,
    fuzzy_min_uncertainty_m = 5000,
    keep_fuzzy = TRUE,
    country_check = TRUE,
    ...) {
  p <- list(
    countries = toupper(countries), fclasses = fclasses,
    max_uncertainty_m = max_uncertainty_m, min_uncertainty_m = min_uncertainty_m,
    keep_medium_confidence = keep_medium_confidence,
    check_coords = check_coords, fix_swaps = fix_swaps, plausible_km = plausible_km,
    suspect_action = match.arg(suspect_action),
    use_collector = use_collector, collector_window_days = collector_window_days,
    anchor_spread_km = anchor_spread_km, anchor_link_km = anchor_link_km,
    anchor_min_uncertainty_m = anchor_min_uncertainty_m, team_anchors = team_anchors,
    use_duplicates = use_duplicates, duplicate_spread_km = duplicate_spread_km, use_admin = use_admin,
    city_km = city_km, admin_seat_km = admin_seat_km, stream_km = stream_km, area_km = area_km,
    extended_relief_km = extended_relief_km,
    use_record_number = use_record_number, max_numbers_per_day = max_numbers_per_day,
    max_number_gap = max_number_gap, max_bracket_days = max_bracket_days,
    cautious_number_dates = cautious_number_dates,
    fuzzy = fuzzy, fuzzy_scope = match.arg(fuzzy_scope), fuzzy_jw_min = fuzzy_jw_min,
    fuzzy_min_nchar = fuzzy_min_nchar, fuzzy_min_uncertainty_m = fuzzy_min_uncertainty_m,
    keep_fuzzy = keep_fuzzy, country_check = country_check)
  structure(p, class = "georef_params")
}

#' @export
print.georef_params <- function(x, ...) {
  cat("<georef_params>\n")
  for (n in names(x)) cat(sprintf("  %-26s %s\n", n, paste(format(x[[n]]), collapse = ", ")))
  invisible(x)
}

# modifie une copie des parametres
update_params <- function(p, ...) {
  a <- list(...)
  for (n in names(a)) p[[n]] <- a[[n]]
  p
}
