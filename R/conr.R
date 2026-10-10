.rang_cat <- c("LC/NT" = 0, "VU" = 1, "EN" = 2, "CR" = 3)

#' Provisional IUCN category (criterion B)
#'
#' Highest category whose EOO threshold (B1: < 100, 5 000, 20 000 km2) or AOO
#' threshold (B2: < 10, 500, 2 000 km2) is met and whose number of locations is
#' compatible (1, at most 5, at most 10). Continuing decline, severe
#' fragmentation and extreme fluctuations are ignored: this is a sensitivity
#' indicator, not a Red List assessment.
#'
#' @param eoo,aoo EOO and AOO (km2).
#' @param locations Number of locations.
#' @return Character vector: `"CR"`, `"EN"`, `"VU"`, `"LC/NT"` or `NA`.
#' @export
#' @examples
#' provisional_category(eoo = c(50, 3000, 1e5), aoo = c(8, 40, 300), locations = c(1, 4, 20))
provisional_category <- function(eoo, aoo, locations) {
  ok <- function(te, ta, tl) (((!is.na(eoo) & eoo < te) | (!is.na(aoo) & aoo < ta)) & !is.na(locations) & locations <= tl)
  dplyr::case_when(ok(100, 10, 1) ~ "CR", ok(5000, 500, 5) ~ "EN", ok(20000, 2000, 10) ~ "VU",
                   is.na(eoo) & is.na(aoo) ~ NA_character_, TRUE ~ "LC/NT")
}
cat_eoo <- function(x) dplyr::case_when(is.na(x) ~ NA_character_, x < 100 ~ "CR", x < 5000 ~ "EN", x < 20000 ~ "VU", TRUE ~ "LC/NT")
cat_aoo <- function(x) dplyr::case_when(is.na(x) ~ NA_character_, x < 10 ~ "CR", x < 500 ~ "EN", x < 2000 ~ "VU", TRUE ~ "LC/NT")
worst_cat <- function(a, b) {
  r <- pmax(unname(.rang_cat[a]), unname(.rang_cat[b]), na.rm = TRUE)
  names(.rang_cat)[match(r, .rang_cat)]
}
status_change <- function(c0, c1) dplyr::case_when(
  is.na(c1) ~ NA_character_, is.na(c0) ~ "nouvelle esp\u00e8ce \u00e9valuable",
  unname(.rang_cat[c1]) > unname(.rang_cat[c0]) ~ "plus menac\u00e9e",
  unname(.rang_cat[c1]) < unname(.rang_cat[c0]) ~ "moins menac\u00e9e", TRUE ~ "inchang\u00e9e")

# appel robuste aux fonctions ConR (arguments variables selon les versions)
conr_call <- function(fun, ...) { a <- list(...); do.call(fun, a[names(a) %in% names(formals(fun))]) }
conr_extract <- function(x, col, sp) {
  if (is.list(x) && !is.data.frame(x) && !is.null(x$results)) x <- x$results
  if (is.list(x) && !is.data.frame(x) && !is.null(x[[col]])) x <- x[[col]]
  if (is.data.frame(x)) {
    nm <- if ("tax" %in% names(x)) as.character(x$tax) else rownames(x)
    v <- if (col %in% names(x)) x[[col]] else x[[ncol(x)]]
    v <- stats::setNames(as.numeric(v), nm)
  } else v <- stats::setNames(as.numeric(x), names(x))
  if (is.null(names(v)) || mean(sp %in% names(v)) < 0.5) {
    if (length(v) == length(sp)) names(v) <- sp else stop("Cannot match ConR output to species (", col, ")")
  }
  v[sp]
}
conr_robust <- function(fun, XY, col, ...) {
  sp <- sort(unique(XY$tax))
  v <- tryCatch(conr_extract(conr_call(fun, XY = XY, ...), col, sp), error = function(e) NULL)
  if (!is.null(v)) return(v)
  v <- vapply(sp, function(s) tryCatch(unname(conr_extract(conr_call(fun, XY = XY[XY$tax == s, ], ...), col, s))[1],
                                       error = function(e) NA_real_), numeric(1))
  stats::setNames(v, sp)
}

#' Compare IUCN criterion B metrics before and after georeferencing
#'
#' Computes with 'ConR' the EOO, AOO, number of locations and of subpopulations
#' for three sets of points: (A) original GBIF coordinates only; (B) A plus the
#' reconstructed points of confidence "haute"; (C) A plus all reconstructed
#' points except fuzzy ones (optionally D: C plus fuzzy points). As in 'ConR',
#' an EOO that is missing or smaller than the AOO is set to the AOO.
#'
#' @param x A `georef_result` or a data frame such as its `occ_dedup` element
#'   (columns `decimalLatitude`, `decimalLongitude`, `species`, `geo_source`,
#'   `geo_confiance`).
#' @param cell_aoo_km AOO grid size (km).
#' @param cell_loc_km Grid size used to count locations (km).
#' @param resol_subpop_km Subpopulation resolution (km).
#' @param include_fuzzy Add set D with fuzzy points.
#' @param n_sim Number of simulations adding random location errors to the
#'   reconstructed points of set C (0 = none).
#' @param errors_km Empirical errors (km) used by the simulations, e.g.
#'   `v$errors$err_corr_km` from [validate_georef()]; a log-normal distribution
#'   (median 13.5 km, 90th percentile 170 km) is used when `NULL`.
#' @param verbose Print progress.
#' @return An object of class `georef_conr`: a list with `wide` (one row per
#'   species), `long`, `summary_sets`, `status_changes`, `transitions_AC`,
#'   `simulation` and `parameters`.
#' @export
#' @examples
#' \dontrun{
#' cmp <- compare_conr(res)
#' cmp$summary_sets
#' write_conr(cmp, "comparaison_conr.xlsx")
#' }
compare_conr <- function(x, cell_aoo_km = 2, cell_loc_km = 10, resol_subpop_km = 10, include_fuzzy = FALSE,
                         n_sim = 0, errors_km = NULL, verbose = TRUE) {
  if (!requireNamespace("ConR", quietly = TRUE)) stop("Package 'ConR' is required: install.packages('ConR')")
  if (inherits(x, "georef_result")) x <- x$occ_dedup
  pts <- data.frame(ddlat = as.numeric(x$decimalLatitude), ddlon = as.numeric(x$decimalLongitude),
                    tax = trimws(as.character(x$species)), src = as.character(x$geo_source),
                    conf = as.character(x$geo_confiance), stringsAsFactors = FALSE)
  pts <- pts[is.finite(pts$ddlat) & is.finite(pts$ddlon) & abs(pts$ddlat) <= 90 & abs(pts$ddlon) <= 180 &
               !is.na(pts$tax) & pts$tax != "", ]
  jeux <- list(A = pts[pts$src == "GBIF", ],
               B = pts[pts$src == "GBIF" | pts$conf %in% "haute", ],
               C = pts[pts$src == "GBIF" | (grepl("^(GeoNames|Collecteur)", pts$src) & pts$src != "GeoNames_flou"), ])
  if (include_fuzzy) jeux$D <- pts[pts$src != "aucune", ]
  jeux <- jeux[vapply(jeux, nrow, integer(1)) > 0]

  calculer <- function(d, id, rapide = FALSE) {
    if (verbose) message("ConR: set ", id, " (", nrow(d), " points, ", length(unique(d$tax)), " species)")
    XY <- unique(d[, c("ddlat", "ddlon", "tax")])
    sp <- sort(unique(XY$tax))
    eoo <- conr_robust(ConR::EOO.computing, XY, "eoo", export_shp = FALSE, write_shp = FALSE,
                       write_results = FALSE, show_progress = FALSE)
    aoo <- conr_robust(ConR::AOO.computing, XY, "aoo", Cell_size_AOO = cell_aoo_km, export_shp = FALSE,
                       write_shp = FALSE, write_results = FALSE, show_progress = FALSE)
    loc <- conr_robust(ConR::locations.comp, XY, "locations", method = "fixed_grid",
                       Cell_size_locations = cell_loc_km, show_progress = FALSE)
    sub <- if (rapide) stats::setNames(rep(NA_real_, length(sp)), sp) else
      conr_robust(ConR::subpop.comp, XY, "subpop", Resol_sub_pop = resol_subpop_km, show_progress = FALSE)
    nb <- as.data.frame(d %>% dplyr::group_by(.data$tax) %>%
                          dplyr::summarise(N_pts = dplyr::n(), N_geonames = sum(.data$src != "GBIF"), .groups = "drop"))
    out <- data.frame(tax = sp, jeu = id, N_pts_uniques = as.integer(table(XY$tax)[sp]),
                      EOO_brut = unname(eoo[sp]), AOO = unname(aoo[sp]), N_localites = unname(loc[sp]),
                      N_subpop = unname(sub[sp]), stringsAsFactors = FALSE)
    out <- dplyr::left_join(out, nb, by = "tax")
    out$Part_GeoNames <- round(out$N_geonames / out$N_pts, 3)
    out$EOO_ajuste <- is.na(out$EOO_brut) | (!is.na(out$AOO) & out$EOO_brut < out$AOO)
    out$EOO <- ifelse(out$EOO_ajuste, out$AOO, out$EOO_brut)
    out$Cat_EOO <- cat_eoo(out$EOO); out$Cat_AOO <- cat_aoo(out$AOO)
    out$Cat_seuils <- worst_cat(out$Cat_EOO, out$Cat_AOO)
    out$Cat_prov <- provisional_category(out$EOO, out$AOO, out$N_localites)
    out
  }
  long <- dplyr::bind_rows(Map(calculer, jeux, names(jeux)))
  wide <- long[, setdiff(names(long), "EOO_brut")] %>%
    tidyr::pivot_wider(names_from = "jeu", values_from = -dplyr::all_of(c("tax", "jeu")), names_glue = "{.value}_{jeu}") %>%
    as.data.frame()
  pct <- function(a, b) round(100 * (b - a) / a, 1)
  for (j in setdiff(names(jeux), "A")) {
    wide[[paste0("EOO_var_pct_A", j)]] <- pct(wide$EOO_A, wide[[paste0("EOO_", j)]])
    wide[[paste0("AOO_var_pct_A", j)]] <- pct(wide$AOO_A, wide[[paste0("AOO_", j)]])
    wide[[paste0("Loc_gain_A", j)]]    <- wide[[paste0("N_localites_", j)]] - wide$N_localites_A
    wide[[paste0("Statut_A", j)]]      <- status_change(wide$Cat_prov_A, wide[[paste0("Cat_prov_", j)]])
  }
  resume <- as.data.frame(long %>% dplyr::group_by(.data$jeu) %>% dplyr::summarise(
    n_especes = dplyr::n(), n_points = sum(.data$N_pts), n_points_geonames = sum(.data$N_geonames),
    EOO_mediane_km2 = stats::median(.data$EOO, na.rm = TRUE), AOO_mediane_km2 = stats::median(.data$AOO, na.rm = TRUE),
    n_CR = sum(.data$Cat_prov == "CR", na.rm = TRUE), n_EN = sum(.data$Cat_prov == "EN", na.rm = TRUE),
    n_VU = sum(.data$Cat_prov == "VU", na.rm = TRUE), n_LC_NT = sum(.data$Cat_prov == "LC/NT", na.rm = TRUE),
    .groups = "drop"))
  bilan <- dplyr::bind_rows(lapply(setdiff(names(jeux), "A"), function(j) {
    t <- as.data.frame(table(statut = wide[[paste0("Statut_A", j)]], useNA = "no"), stringsAsFactors = FALSE)
    if (nrow(t)) t$comparaison <- paste0("A vers ", j)
    t
  }))
  trans <- if (all(c("Cat_prov_A", "Cat_prov_C") %in% names(wide)))
    as.data.frame(dplyr::count(wide, .data$Cat_prov_A, .data$Cat_prov_C, name = "n_especes")) else NULL

  sim <- NULL
  if (n_sim > 0 && !is.null(jeux$C)) {
    errs <- if (!is.null(errors_km)) errors_km[is.finite(errors_km)] else NULL
    bruit <- function(n) if (length(errs) >= 50) sample(errs, n, replace = TRUE) else
      stats::rlnorm(n, log(13.5), (log(170) - log(13.5)) / stats::qnorm(0.9))
    sims <- lapply(seq_len(n_sim), function(k) {
      d <- jeux$C; g <- which(d$src != "GBIF")
      if (length(g)) {
        pp <- geosphere::destPoint(cbind(d$ddlon[g], d$ddlat[g]), stats::runif(length(g), 0, 360), bruit(length(g)) * 1000)
        d$ddlon[g] <- pp[, 1]; d$ddlat[g] <- pp[, 2]
      }
      calculer(d, paste0("sim", k), rapide = TRUE)
    })
    sim <- as.data.frame(dplyr::bind_rows(sims) %>% dplyr::group_by(.data$tax) %>% dplyr::summarise(
      EOO_med = stats::median(.data$EOO, na.rm = TRUE), AOO_med = stats::median(.data$AOO, na.rm = TRUE),
      P_CR = mean(.data$Cat_prov == "CR", na.rm = TRUE), P_EN = mean(.data$Cat_prov == "EN", na.rm = TRUE),
      P_VU = mean(.data$Cat_prov == "VU", na.rm = TRUE),
      P_menacee = mean(.data$Cat_prov %in% c("CR", "EN", "VU"), na.rm = TRUE), .groups = "drop"))
  }
  params <- data.frame(parametre = c("ConR", "maille AOO (km)", "maille localit\u00e9s (km)",
                                     "r\u00e9solution sous-populations (km)", "jeu flou inclus", "simulations", "date"),
                       valeur = c(as.character(utils::packageVersion("ConR")), cell_aoo_km, cell_loc_km,
                                  resol_subpop_km, include_fuzzy, n_sim, as.character(Sys.Date())))
  structure(list(wide = wide, long = long, summary_sets = resume, status_changes = bilan,
                 transitions_AC = trans, simulation = sim, parameters = params), class = "georef_conr")
}

#' @export
print.georef_conr <- function(x, ...) {
  cat("<georef_conr>\n"); print(x$summary_sets, row.names = FALSE)
  cat("\nChanges of provisional category compared with set A:\n"); print(x$status_changes, row.names = FALSE)
  invisible(x)
}

#' Write a ConR comparison to Excel (or CSV)
#'
#' @param x A `georef_conr`.
#' @param file Output `.xlsx` file. If 'writexl' is not installed, one CSV per sheet is written.
#' @return Invisibly, the file path(s).
#' @export
write_conr <- function(x, file = "comparaison_conr.xlsx") {
  sheets <- c(list(Synthese = x$wide, Resume_jeux = x$summary_sets, Bilan_statuts = x$status_changes),
              if (!is.null(x$transitions_AC)) list(Transitions_A_C = x$transitions_AC),
              if (!is.null(x$simulation)) list(Simulation_C = x$simulation),
              list(Detail_long = x$long, Parametres = x$parameters))
  if (requireNamespace("writexl", quietly = TRUE)) {
    writexl::write_xlsx(sheets, file); return(invisible(file))
  }
  out <- vapply(names(sheets), function(n) {
    f <- sub("\\.xlsx$", paste0("_", n, ".csv"), file); utils::write.csv(sheets[[n]], f, row.names = FALSE); f
  }, character(1))
  invisible(out)
}

#' Compare provisional categories with a reference assessment
#'
#' @param x A `georef_conr` or its `wide` table.
#' @param reference Data frame with one row per taxon.
#' @param tax_col,cat_col Names of the taxon and category columns of `reference`.
#' @return A data frame with, for each set, the number of species in common,
#'   of agreements, and of provisional categories less or more threatened than
#'   the reference.
#' @export
#' @examples
#' w <- data.frame(tax = c("a", "b", "c"), Cat_prov_A = c("EN", "VU", "LC/NT"),
#'                 Cat_prov_C = c("EN", "LC/NT", "LC/NT"))
#' ref <- data.frame(tax = c("a", "b", "c"), Category_Final = c("EN", "VU", "VU"))
#' compare_reference(w, ref, cat_col = "Category_Final")
compare_reference <- function(x, reference, tax_col = "tax", cat_col = "Category_Final") {
  w <- if (inherits(x, "georef_conr")) x$wide else x
  ref <- data.frame(tax = reference[[tax_col]], ref_cat = reference[[cat_col]], stringsAsFactors = FALSE)
  m <- merge(w, ref, by = "tax")
  sets <- sub("^Cat_prov_", "", grep("^Cat_prov_", names(m), value = TRUE))
  dplyr::bind_rows(lapply(sets, function(j) {
    c1 <- m[[paste0("Cat_prov_", j)]]; ok <- !is.na(c1) & !is.na(m$ref_cat)
    r1 <- .rang_cat[c1[ok]]; r0 <- .rang_cat[m$ref_cat[ok]]
    data.frame(jeu = j, n_communes = sum(ok), accord = sum(r1 == r0), pct_accord = round(100 * mean(r1 == r0), 1),
               moins_menacee = sum(r1 < r0), plus_menacee = sum(r1 > r0))
  }))
}
