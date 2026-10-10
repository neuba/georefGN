#' Validate the georeferencing by repeated masking
#'
#' Coordinates of a small random sample of georeferenced records are masked and
#' the workflow is run with and without the collector steps (herbarium duplicates
#' and collector itineraries; "sans collecteur" is text matching only); all other records
#' keep their coordinates and can serve as anchors, as in real use. The
#' operation is repeated `n_rep` times. With `design = "partition"` the samples
#' are disjoint, so the replicates are independent and the 95\% confidence
#' interval of the mean (Student's t) is valid; the difference between the two
#' variants is paired by replicate.
#'
#' The error is the great-circle distance between the estimated and the true
#' point, computed against three versions of the truth: `brute` (GBIF
#' coordinates as they are), `plausible` (only records whose true coordinates
#' are plausible) and `corrigee` (true coordinates corrected for unambiguous
#' swaps and sign errors).
#'
#' @param x Occurrence records (see [prepare_occurrences()]).
#' @param gazetteer A `georef_gazetteer`.
#' @param params Parameters from [georef_params()].
#' @param n_sample Records masked per replicate.
#' @param n_rep Number of replicates.
#' @param design `"partition"` (disjoint samples, recommended) or `"independent"`.
#' @param max_fraction Maximum fraction of reference records masked in one replicate.
#' @param mask_duplicates Also mask the herbarium duplicates of each tested
#'   specimen (same collector, same collection number), so that they cannot be
#'   used as anchors.
#' @param seed Random seed (`NULL` to use the current state).
#' @param verbose Print progress.
#' @return An object of class `georef_validation`: a list with `by_replicate`,
#'   `summary` (mean, 95\% CI, range across replicates), `difference` (paired
#'   with - without collector), `by_source`, `by_confidence`, `by_date` (pooled
#'   errors) and `errors` (one row per masked record and variant).
#' @export
#' @examples
#' ex <- georef_example()
#' gaz <- read_gazetteer(ex$geonames_dir, countries = "CI", verbose = FALSE)
#' v <- validate_georef(ex$occurrences, gaz, georef_params(countries = "CI"),
#'                      n_sample = 30, n_rep = 3, verbose = FALSE)
#' v
#' plot(v)
validate_georef <- function(x, gazetteer, params = georef_params(), n_sample = 300, n_rep = 10,
                            design = c("partition", "independent"), max_fraction = 0.25,
                            mask_duplicates = FALSE, seed = 42, verbose = TRUE) {
  design <- match.arg(design)
  p <- params
  msg <- function(...) if (verbose) message(...)
  if (!is.null(seed)) set.seed(seed)
  g0 <- prepare_occurrences(x, p)
  g0 <- g0[, !vapply(g0, is.list, logical(1))]
  # enregistrements de reference : coordonnees + texte + pays couvert, dedoublonnes
  ref <- g0[!is.na(g0$decimalLatitude) & !is.na(g0$decimalLongitude) &
              g0$countryCode %in% gazetteer$countries & g0$loc_txt0 != "", ]
  k <- paste(ref$species, round(ref$decimalLatitude, 3), round(ref$decimalLongitude, 3),
             ref$recordedBy, ref$recordNumber, ref$eventDate)
  ref <- ref[!duplicated(k), ]
  N <- nrow(ref)
  if (N < 2) stop("Not enough reference records (", N, ").")
  n_sample <- min(n_sample, floor(max_fraction * N))
  if (design == "partition") {
    if (n_sample * n_rep > N) {
      n_rep <- floor(N / n_sample)
      warning("Partition design: n_rep reduced to ", n_rep)
    }
    shuffled <- sample(ref$.rid)
    samples <- lapply(seq_len(n_rep), function(r) shuffled[((r - 1) * n_sample + 1):(r * n_sample)])
  } else {
    samples <- lapply(seq_len(n_rep), function(r) sample(ref$.rid, n_sample))
  }
  msg("Plan: ", n_rep, " replicates of ", n_sample, " masked records (", design, "); ",
      sprintf("%.1f%%", 100 * n_sample / N), " of the reference records masked per replicate")

  # verite : plausibilite et correction des coordonnees de reference
  tr_all <- g0[g0$.rid %in% unlist(samples), c(".rid", "countryCode", "decimalLatitude", "decimalLongitude")]
  names(tr_all) <- c(".rid", "countryCode", "vlat", "vlon")
  d0 <- dist_to_places(tr_all$vlat, tr_all$vlon, tr_all$countryCode, gazetteer)
  tr_all$statut_v <- ifelse(is.na(d0), "non_verifie", ifelse(d0 <= p$plausible_km, "ok", "suspecte"))
  tr_all$vlat_c <- ifelse(tr_all$statut_v == "ok", tr_all$vlat, NA_real_)
  tr_all$vlon_c <- ifelse(tr_all$statut_v == "ok", tr_all$vlon, NA_real_)
  for (i in which(tr_all$statut_v == "suspecte")) {
    r <- fix_coordinate(tr_all$vlat[i], tr_all$vlon[i], tr_all$countryCode[i], gazetteer, p$plausible_km)
    if (!is.null(r)) { tr_all$vlat_c[i] <- r$lat; tr_all$vlon_c[i] <- r$lon; tr_all$statut_v[i] <- "corrigee" }
  }

  dup_key <- NULL
  if (mask_duplicates) {
    dup_key <- g0$dup_key
  }
  raw <- x
  if (!is.data.frame(raw)) raw <- g0
  raw <- as.data.frame(raw)[, !vapply(as.data.frame(raw), is.list, logical(1)), drop = FALSE]

  evaluer <- function(res, tr, lab, r) {
    geo <- res$georeferenced
    if (is.null(geo) || !nrow(geo)) return(NULL)
    d <- dplyr::inner_join(geo[, c(".rid", "decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters",
                                   "geo_confiance", "geo_source", "geo_niveau", "date_source")], tr, by = ".rid")
    d$err_brut_km <- geosphere::distHaversine(cbind(d$decimalLongitude, d$decimalLatitude), cbind(d$vlon, d$vlat)) / 1000
    d$err_corr_km <- ifelse(is.na(d$vlat_c), NA_real_,
                            geosphere::distHaversine(cbind(d$decimalLongitude, d$decimalLatitude), cbind(d$vlon_c, d$vlat_c)) / 1000)
    d$variante <- rep(lab, nrow(d)); d$tirage <- rep(r, nrow(d))
    d
  }
  dd <- list(); t0 <- Sys.time()
  for (r in seq_len(n_rep)) {
    m <- seq_len(nrow(raw)) %in% samples[[r]]
    if (mask_duplicates) {
      cles <- stats::na.omit(dup_key[m])
      m <- m | (dup_key %in% cles)
    }
    gm <- raw
    gm$decimalLatitude[m] <- NA; gm$decimalLongitude[m] <- NA
    if ("coordinateUncertaintyInMeters" %in% names(gm)) gm$coordinateUncertaintyInMeters[m] <- NA
    # "sans collecteur" = texte seul (ni doublons d'herbier ni rapprochement par itineraire)
    rs <- georeference(gm, gazetteer, update_params(p, use_collector = FALSE, use_duplicates = FALSE), verbose = FALSE)
    ra <- georeference(gm, gazetteer, update_params(p, use_collector = TRUE), verbose = FALSE)
    tr <- tr_all[tr_all$.rid %in% samples[[r]], ]
    dd[[r]] <- dplyr::bind_rows(evaluer(rs, tr, "sans collecteur", r), evaluer(ra, tr, "avec collecteur", r))
    el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
    msg(sprintf("Replicate %d/%d done (%.1f min, ~%.1f min left)", r, n_rep, el, el / r * (n_rep - r)))
  }
  dd <- dplyr::bind_rows(dd)

  long <- dplyr::bind_rows(
    dplyr::mutate(dd, base = "brute", err_km = .data$err_brut_km),
    dplyr::mutate(dd[dd$statut_v == "ok", ], base = "plausible", err_km = .data$err_brut_km),
    dplyr::mutate(dd[dd$statut_v %in% c("ok", "corrigee"), ], base = "corrigee", err_km = .data$err_corr_km))
  long$dans_incert <- long$err_km * 1000 <= long$coordinateUncertaintyInMeters
  denom <- dplyr::bind_rows(lapply(seq_len(n_rep), function(r) {
    s <- tr_all$statut_v[tr_all$.rid %in% samples[[r]]]
    data.frame(tirage = r, base = c("brute", "plausible", "corrigee"),
               n_masques = c(length(s), sum(s == "ok"), sum(s %in% c("ok", "corrigee"))))
  }))
  metr <- function(z) dplyr::summarise(z,
    n_georef = dplyr::n(), mediane_km = stats::median(.data$err_km), moyenne_km = mean(.data$err_km),
    q75_km = unname(stats::quantile(.data$err_km, 0.75)), q90_km = unname(stats::quantile(.data$err_km, 0.90)),
    pct_2km = 100 * mean(.data$err_km <= 2), pct_5km = 100 * mean(.data$err_km <= 5),
    pct_10km = 100 * mean(.data$err_km <= 10), pct_25km = 100 * mean(.data$err_km <= 25),
    pct_dans_incertitude = 100 * mean(.data$dans_incert))
  grille <- expand.grid(tirage = seq_len(n_rep), base = c("brute", "plausible", "corrigee"),
                        variante = c("sans collecteur", "avec collecteur"), stringsAsFactors = FALSE)
  par_tirage <- long %>% dplyr::group_by(.data$tirage, .data$base, .data$variante) %>% metr() %>%
    dplyr::ungroup() %>% dplyr::right_join(grille, by = c("tirage", "base", "variante")) %>%
    dplyr::left_join(denom, by = c("tirage", "base"))
  par_tirage$n_georef <- dplyr::coalesce(par_tirage$n_georef, 0L)
  par_tirage$taux_georef_pct <- 100 * par_tirage$n_georef / par_tirage$n_masques
  p1 <- tapply(dd$geo_source[dd$variante == "avec collecteur"] == "Collecteur_palier1",
               dd$tirage[dd$variante == "avec collecteur"], sum)
  par_tirage$n_palier1 <- ifelse(par_tirage$variante == "avec collecteur",
                                 dplyr::coalesce(as.integer(p1[as.character(par_tirage$tirage)]), 0L), NA_integer_)
  par_tirage$pct_palier1 <- 100 * par_tirage$n_palier1 / par_tirage$n_masques
  pd <- tapply(dd$geo_source[dd$variante == "avec collecteur"] == "Doublon_herbier",
               dd$tirage[dd$variante == "avec collecteur"], sum)
  par_tirage$n_doublon <- ifelse(par_tirage$variante == "avec collecteur",
                                 dplyr::coalesce(as.integer(pd[as.character(par_tirage$tirage)]), 0L), NA_integer_)
  par_tirage$pct_doublon <- 100 * par_tirage$n_doublon / par_tirage$n_masques
  par_tirage <- as.data.frame(par_tirage[order(par_tirage$base, par_tirage$variante, par_tirage$tirage), ])

  mesures <- c("taux_georef_pct", "mediane_km", "moyenne_km", "q90_km", "pct_2km", "pct_5km",
               "pct_10km", "pct_25km", "pct_dans_incertitude", "pct_doublon", "pct_palier1")
  lng <- tidyr::pivot_longer(par_tirage, dplyr::any_of(mesures), names_to = "mesure", values_to = "v")
  lng <- lng[!is.na(lng$v), ]
  ic <- function(v) stats::qt(0.975, pmax(length(v) - 1, 1)) * stats::sd(v) / sqrt(length(v))
  resume <- lng %>% dplyr::group_by(.data$base, .data$variante, .data$mesure) %>%
    dplyr::summarise(n_tirages = dplyr::n(), moyenne = mean(.data$v), ecart_type = stats::sd(.data$v),
                     ic95_bas = mean(.data$v) - ic(.data$v), ic95_haut = mean(.data$v) + ic(.data$v),
                     tirage_q025 = unname(stats::quantile(.data$v, 0.025)),
                     tirage_q975 = unname(stats::quantile(.data$v, 0.975)), .groups = "drop") %>%
    dplyr::mutate(mesure = factor(.data$mesure, levels = mesures)) %>%
    dplyr::arrange(.data$base, .data$variante, .data$mesure) %>% as.data.frame()
  diff <- lng[, c("tirage", "base", "variante", "mesure", "v")] %>%
    tidyr::pivot_wider(names_from = "variante", values_from = "v")
  diff$d <- diff[["avec collecteur"]] - diff[["sans collecteur"]]
  diff <- diff[!is.na(diff$d), ] %>% dplyr::group_by(.data$base, .data$mesure) %>%
    dplyr::summarise(n_tirages = dplyr::n(), diff_moyenne = mean(.data$d), ecart_type = stats::sd(.data$d),
                     ic95_bas = mean(.data$d) - ic(.data$d), ic95_haut = mean(.data$d) + ic(.data$d),
                     tirages_hausse = sum(.data$d > 0), tirages_baisse = sum(.data$d < 0), .groups = "drop") %>%
    dplyr::mutate(mesure = factor(.data$mesure, levels = mesures)) %>%
    dplyr::arrange(.data$base, .data$mesure) %>% as.data.frame()
  lc <- long[long$base == "corrigee", ]
  out <- structure(list(
    by_replicate = par_tirage, summary = resume, difference = diff,
    by_source = as.data.frame(lc %>% dplyr::group_by(.data$variante, .data$geo_source) %>% metr()),
    by_confidence = as.data.frame(lc %>% dplyr::group_by(.data$variante, .data$geo_confiance) %>% metr()),
    by_date = as.data.frame(lc[lc$variante == "avec collecteur", ] %>%
                              dplyr::group_by(.data$geo_source, .data$date_source) %>% metr()),
    errors = as.data.frame(dd), truth = tr_all,
    design = list(n_sample = n_sample, n_rep = n_rep, design = design, n_reference = N,
                  mask_duplicates = mask_duplicates)), class = "georef_validation")
  if (verbose) print(out)
  out
}

#' @export
print.georef_validation <- function(x, ...) {
  d <- x$design
  cat("<georef_validation>", d$n_rep, "replicates of", d$n_sample, "masked records (", d$design, ")",
      if (d$mask_duplicates) "| duplicates masked" else "", "\n")
  s <- x$summary[x$summary$base == "corrigee", ]
  s$valeur <- sprintf("%.1f [%.1f ; %.1f]", s$moyenne, s$ic95_bas, s$ic95_haut)
  w <- tidyr::pivot_wider(s[, c("mesure", "variante", "valeur")], names_from = "variante", values_from = "valeur")
  df <- x$difference[x$difference$base == "corrigee", ]
  w$`avec - sans` <- sprintf("%+.1f [%.1f ; %.1f]", df$diff_moyenne, df$ic95_bas, df$ic95_haut)[match(w$mesure, df$mesure)]
  print(as.data.frame(w), row.names = FALSE)
  invisible(x)
}

#' Plot the error distribution of a validation
#'
#' Empirical cumulative distribution of the location error, with and without
#' the collector step: one thin line per replicate, a thick line for the pooled errors.
#'
#' @param x A `georef_validation`.
#' @param base Truth version: `"corrigee"`, `"plausible"` or `"brute"`.
#' @param xlim Range of the x axis (km).
#' @param ... Passed to [graphics::plot()].
#' @return Invisibly, `x`.
#' @export
plot.georef_validation <- function(x, base = "corrigee", xlim = c(0, 100), ...) {
  e <- x$errors
  e$err <- if (base == "brute" || base == "plausible") e$err_brut_km else e$err_corr_km
  if (base == "plausible") e <- e[e$statut_v == "ok", ]
  e <- e[!is.na(e$err), ]
  graphics::plot(NULL, xlim = xlim, ylim = c(0, 1), xlab = paste0("Erreur de localisation (km), v\u00e9rit\u00e9 ", base),
                 ylab = "Proportion cumul\u00e9e", ...)
  vs <- c("sans collecteur", "avec collecteur"); cols <- c("grey55", "black")
  for (k in seq_along(vs)) {
    xv <- e[e$variante == vs[k], ]
    for (r in unique(xv$tirage))
      graphics::lines(stats::ecdf(xv$err[xv$tirage == r]), col = grDevices::adjustcolor(cols[k], 0.25),
                      verticals = TRUE, do.points = FALSE, lwd = 0.7)
    if (nrow(xv)) graphics::lines(stats::ecdf(xv$err), col = cols[k], verticals = TRUE, do.points = FALSE, lwd = 2)
  }
  graphics::legend("bottomright", c(vs, "un tirage (trait fin)"), lty = 1, lwd = c(2, 2, 0.7),
                   col = c(cols, grDevices::adjustcolor("black", 0.3)), bty = "n")
  invisible(x)
}

#' Write the outputs of validate_georef()
#'
#' @param x A `georef_validation`.
#' @param dir Output folder.
#' @param prefix File name prefix.
#' @return Invisibly, the paths of the files written.
#' @export
write_validation <- function(x, dir = ".", prefix = "validation_") {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  parts <- c(par_tirage = "by_replicate", resume = "summary", difference = "difference",
             par_source = "by_source", par_confiance = "by_confidence", par_date = "by_date", erreurs = "errors")
  out <- character(0)
  for (n in names(parts)) {
    path <- file.path(dir, paste0(prefix, n, ".csv"))
    utils::write.csv(x[[parts[[n]]]], path, row.names = FALSE)
    out <- c(out, path)
  }
  png <- file.path(dir, paste0(prefix, "ecdf.png"))
  grDevices::png(png, width = 1800, height = 1100, res = 220)
  plot(x)
  grDevices::dev.off()
  invisible(c(out, png))
}
