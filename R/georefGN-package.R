#' georefGN: georeferencing herbarium and GBIF occurrences with GeoNames
#'
#' Main functions:
#' \itemize{
#'   \item [download_geonames()], [read_gazetteer()]: gazetteer;
#'   \item [georeference()], [write_georef()]: georeferencing workflow;
#'   \item [validate_georef()], [write_validation()]: validation by repeated masking;
#'   \item [compare_conr()], [compare_reference()], [write_conr()]: effect on IUCN criterion B metrics.
#' }
#' Parameters are set with [georef_params()].
#'
#' @keywords internal
#' @importFrom dplyr %>% .data
"_PACKAGE"
