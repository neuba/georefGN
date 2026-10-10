#' Correct a swapped or sign-inverted coordinate
#'
#' Tests the seven transformations of a point (latitude/longitude swap, sign
#' changes and their combinations) and keeps the simplest one that brings the
#' point within `plausible_km` of a populated place of its declared country,
#' only if the choice is unambiguous (at equal complexity, the best candidate
#' must be at least twice as close as the next one).
#'
#' @param lat,lon Coordinates (single values).
#' @param country ISO2 country code.
#' @param gazetteer A `georef_gazetteer` from [read_gazetteer()].
#' @param plausible_km Plausibility distance (km).
#' @return `NULL` when no unambiguous correction exists, otherwise a list with
#'   `lat`, `lon` and `correction` (e.g. `"inversion lat/lon"`, `"signe lon"`).
#' @export
fix_coordinate <- function(lat, lon, country, gazetteer, plausible_km = 50) {
  g <- expand.grid(sw = c(FALSE, TRUE), sa = c(1, -1), sb = c(1, -1))
  g <- g[!(g$sw == FALSE & g$sa == 1 & g$sb == 1), ]
  nla <- ifelse(g$sw, g$sa * lon, g$sa * lat)
  nlo <- ifelse(g$sw, g$sb * lat, g$sb * lon)
  v <- abs(nla) <= 90 & abs(nlo) <= 180
  d <- rep(Inf, nrow(g))
  if (any(v)) d[v] <- dist_to_places(nla[v], nlo[v], rep(country, sum(v)), gazetteer)
  d[is.na(d)] <- Inf
  cx <- g$sw + (g$sa < 0) + (g$sb < 0)
  ok <- which(d <= plausible_km)
  if (length(ok)) {
    w <- ok[cx[ok] == min(cx[ok])]
    w <- w[order(d[w])]
    if (length(w) == 1 || d[w[2]] > max(plausible_km, 2 * d[w[1]])) {
      k <- w[1]
      lab <- c(if (g$sw[k]) "inversion lat/lon", if (g$sa[k] < 0) "signe lat", if (g$sb[k] < 0) "signe lon")
      return(list(lat = nla[k], lon = nlo[k], correction = paste(lab, collapse = " + ")))
    }
  }
  NULL
}

#' Check original coordinates
#'
#' A point is plausible when a GeoNames populated place of its declared country
#' lies within `plausible_km`. Suspect points are corrected with
#' [fix_coordinate()] when possible. Original values are kept in
#' `decimalLatitude_orig` and `decimalLongitude_orig`.
#'
#' @param occ Data frame from [prepare_occurrences()].
#' @param gazetteer A `georef_gazetteer`.
#' @param params Parameters from [georef_params()].
#' @return `occ` with columns `coord_statut` (`absente`, `ok`, `corrigee`,
#'   `suspecte`, `non_verifie`) and `coord_correction`.
#' @export
check_coordinates <- function(occ, gazetteer, params = georef_params()) {
  p <- params
  occ$decimalLatitude_orig  <- occ$decimalLatitude
  occ$decimalLongitude_orig <- occ$decimalLongitude
  occ$coord_statut <- ifelse(is.na(occ$decimalLatitude) | is.na(occ$decimalLongitude), "absente", "non_verifie")
  occ$coord_correction <- NA_character_
  if (!p$check_coords) return(occ)
  iv <- which(occ$coord_statut == "non_verifie" & occ$countryCode %in% gazetteer$countries)
  if (!length(iv)) return(occ)
  d0 <- dist_to_places(occ$decimalLatitude[iv], occ$decimalLongitude[iv], occ$countryCode[iv], gazetteer)
  occ$coord_statut[iv] <- ifelse(!is.na(d0) & d0 <= p$plausible_km, "ok", "suspecte")
  if (p$fix_swaps) for (i in iv[occ$coord_statut[iv] == "suspecte"]) {
    r <- fix_coordinate(occ$decimalLatitude[i], occ$decimalLongitude[i], occ$countryCode[i], gazetteer, p$plausible_km)
    if (!is.null(r)) {
      occ$decimalLatitude[i] <- r$lat; occ$decimalLongitude[i] <- r$lon
      occ$coord_statut[i] <- "corrigee"; occ$coord_correction[i] <- r$correction
    }
  }
  if (p$suspect_action == "regeoreference") {
    s <- occ$coord_statut == "suspecte"
    occ$decimalLatitude[s] <- NA_real_; occ$decimalLongitude[s] <- NA_real_
  }
  occ
}
