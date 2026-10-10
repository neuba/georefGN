# ---- ancres du module collecteur (phase 1) ---------------------------------------

# recoltes georeferencees (ancres) et toponymes GeoNames rattaches a chacune
build_anchors <- function(occ, gazetteer, p) {
  ok_st <- c("ok", "corrigee", if (!p$check_coords) "non_verifie")
  refs <- occ[occ$coord_statut %in% ok_st & !is.na(occ$coll) & !is.na(occ$t) &
                occ$countryCode %in% gazetteer$countries, ]
  empty <- list(refs = refs, links = data.frame(), base = data.frame())
  if (!nrow(refs)) return(empty)
  refs <- data.frame(.rid = refs$.rid, species = refs$species, recordedBy = refs$recordedBy, coll = refs$coll,
                     coll_all = refs$coll_all,
                     countryCode = refs$countryCode, r_lat = refs$decimalLatitude, r_lon = refs$decimalLongitude,
                     t = refs$t, tprec = refs$tprec, tdemi = refs$tdemi, annee = refs$annee, mois = refs$mois,
                     incert = pmax(dplyr::coalesce(refs$coordinateUncertaintyInMeters, 0), p$anchor_min_uncertainty_m),
                     txt = norm_text(refs$loc_txt0), stringsAsFactors = FALSE)
  refs$key <- paste(refs$txt, refs$countryCode, sep = "||")
  tok_ref <- refs[refs$txt != "" & !is.na(refs$txt), c("key", "txt", "countryCode")]
  tok_ref <- tok_ref[!duplicated(tok_ref$key), ]
  cand_ref <- match_texts(tok_ref, gazetteer)
  if (!nrow(cand_ref)) return(empty)
  by_key <- split(cand_ref, cand_ref$key)
  rt <- refs[refs$key %in% names(by_key), ]
  links <- dplyr::bind_rows(lapply(seq_len(nrow(rt)), function(i) {
    d <- by_key[[rt$key[i]]]
    dplyr::bind_rows(lapply(retained_names(d$nom_n), function(n) {
      o <- d[d$nom_n == n, ]
      dk <- geosphere::distHaversine(c(rt$r_lon[i], rt$r_lat[i]), cbind(o$lon, o$lat)) / 1000
      j <- which.min(dk)
      if (dk[j] > p$anchor_link_km) return(NULL)
      data.frame(.rid = rt$.rid[i], nom_n = n, geonameid = o$geonameid[j],
                 g_lat = o$lat[j], g_lon = o$lon[j], dist_km = dk[j], stringsAsFactors = FALSE)
    }))
  }))
  if (!nrow(links)) return(empty)
  links <- dplyr::left_join(links, refs[, c(".rid", "species", "recordedBy", "coll", "coll_all", "countryCode", "r_lat", "r_lon",
                                             "t", "tprec", "tdemi", "annee", "mois", "incert")], by = ".rid")
  t0 <- ifelse(links$tprec == 0, links$t, NA_real_)
  base <- links %>% dplyr::mutate(t0 = t0) %>%
    dplyr::group_by(.data$coll, .data$countryCode, .data$annee, .data$mois, .data$nom_n, .data$geonameid) %>%
    dplyr::summarise(collecteur_exemple = dplyr::first(.data$recordedBy), n_enreg = dplyr::n(),
                     n_especes = dplyr::n_distinct(.data$species),
                     jour_min = if (any(!is.na(.data$t0))) as.character(as.Date(min(.data$t0, na.rm = TRUE), origin = "1970-01-01")) else NA_character_,
                     jour_max = if (any(!is.na(.data$t0))) as.character(as.Date(max(.data$t0, na.rm = TRUE), origin = "1970-01-01")) else NA_character_,
                     lat_mediane = stats::median(.data$r_lat), lon_mediane = stats::median(.data$r_lon),
                     lat_geonames = dplyr::first(.data$g_lat), lon_geonames = dplyr::first(.data$g_lon), .groups = "drop") %>%
    dplyr::arrange(.data$coll, .data$annee, .data$mois, .data$nom_n)
  list(refs = refs, links = links, base = as.data.frame(base))
}

#' Reuse the coordinates of herbarium duplicates
#'
#' A record without coordinates takes the median coordinates of its herbarium
#' duplicates (same collector key, same full collection number, see
#' [prepare_occurrences()]) whose coordinates are plausible. Dates must be
#' compatible (same year when known; at most one day apart when both are known to
#' the day) and the duplicates must lie within `duplicate_spread_km` of their median.
#'
#' @param occ Data frame from [prepare_occurrences()] after [check_coordinates()].
#' @param params Parameters from [georef_params()].
#' @return The records that received coordinates, with `geo_source = "Doublon_herbier"`.
#' @export
duplicate_step <- function(occ, params = georef_params()) {
  p <- params
  ok_st <- c("ok", "corrigee", if (!p$check_coords) "non_verifie")
  don <- occ[occ$coord_statut %in% ok_st & !is.na(occ$dup_key), ]
  rec <- occ[(is.na(occ$decimalLatitude) | is.na(occ$decimalLongitude)) & !is.na(occ$dup_key), ]
  if (!nrow(don) || !nrow(rec)) return(occ[0, ])
  don <- data.frame(dup_key = don$dup_key, d_lat = don$decimalLatitude, d_lon = don$decimalLongitude,
                    d_t = don$t, d_p = don$tprec, d_an = don$annee,
                    d_inc = pmax(dplyr::coalesce(don$coordinateUncertaintyInMeters, 0), p$anchor_min_uncertainty_m))
  pr <- dplyr::inner_join(rec[, c(".rid", "dup_key", "t", "tprec", "annee")], don, by = "dup_key",
                          relationship = "many-to-many")
  pr <- pr[(is.na(pr$annee) | is.na(pr$d_an) | pr$annee == pr$d_an) &
             !(pr$tprec %in% 0 & pr$d_p %in% 0 & abs(pr$t - pr$d_t) > 1), ]
  if (!nrow(pr)) return(occ[0, ])
  agg <- do.call(rbind, lapply(split(pr, pr$.rid), function(g) {
    la <- stats::median(g$d_lat); lo <- stats::median(g$d_lon)
    sp <- max(geosphere::distHaversine(cbind(g$d_lon, g$d_lat), c(lo, la))) / 1000
    data.frame(.rid = g$.rid[1], g_lat = la, g_lon = lo, sp = sp, inc = stats::median(g$d_inc), n = nrow(g))
  }))
  agg <- agg[agg$sp <= p$duplicate_spread_km, ]
  if (!nrow(agg)) return(occ[0, ])
  out <- occ[match(agg$.rid, occ$.rid), ]
  out$decimalLatitude <- agg$g_lat; out$decimalLongitude <- agg$g_lon
  out$coordinateUncertaintyInMeters <- pmax(agg$sp * 1000, agg$inc, p$anchor_min_uncertainty_m)
  out$geo_source <- "Doublon_herbier"; out$geo_confiance <- "haute"; out$geo_niveau <- "doublon"
  out$n_duplicates <- agg$n
  out
}

# palier 1 : meme collecteur, meme lieu GeoNames (geonameid), +/- fenetre
resolve_collector <- function(d, rt, t0, p0, has_off, h0, p) {
  if (has_off || is.null(rt) || !nrow(rt)) return(NULL)
  d <- d[d$nom_n %in% retained_names(d$nom_n), , drop = FALSE]
  r2 <- rt[rt$geonameid %in% unique(d$geonameid), , drop = FALSE]
  if (!nrow(r2)) return(NULL)
  r2$delta <- delta_days(t0, p0, r2$t, r2$tprec, h0, r2$tdemi)
  r2 <- r2[!is.na(r2$delta) & r2$delta <= p$collector_window_days, , drop = FALSE]
  if (!nrow(r2)) return(NULL)
  b <- r2[which.min(r2$delta), ]
  g1 <- r2[r2$geonameid == b$geonameid, , drop = FALSE]
  g1 <- g1[!duplicated(g1$.rid), , drop = FALSE]
  la <- stats::median(g1$r_lat); lo <- stats::median(g1$r_lon)
  sp <- if (nrow(g1) > 1) max(geosphere::distHaversine(cbind(g1$r_lon, g1$r_lat), c(lo, la))) / 1000 else 0
  if (sp > p$anchor_spread_km) return(NULL)
  list(lieux = d$nom_n[d$geonameid == b$geonameid][1], n_lieux = 1L, g_lat = la, g_lon = lo,
       incert_m = max(sp * 1000, stats::median(g1$incert), p$anchor_min_uncertainty_m),
       niveau = "collecteur_palier1", delta_j = b$delta, copie = TRUE, d_ancre_km = sp)
}

#' Georeference occurrence records
#'
#' Runs the whole workflow on records without coordinates:
#' \enumerate{
#'   \item preparation: dates with their precision, collector key, collection
#'     number; undated records are dated from their collection number
#'     ([infer_dates_from_numbers()]); original coordinates are checked
#'     ([check_coordinates()]);
#'   \item herbarium duplicates: a record takes the coordinates of a georeferenced
#'     duplicate (same collector, same collection number; [duplicate_step()]);
#'   \item phase 1 (collector): when a georeferenced record of the same collector
#'     and country is linked to the same GeoNames place within
#'     `collector_window_days`, the median coordinates of these anchors are reused
#'     (if their spread is below `anchor_spread_km`);
#'   \item phase 2 (text): place names of the locality text are matched to the
#'     gazetteer; homonyms located in the declared administrative unit
#'     (`stateProvince`, `county`; [match_admin()]) are preferred, then populated
#'     places, then the most populated; several names are resolved by the most
#'     compact combination; distance-direction expressions are applied; the
#'     uncertainty depends on the feature type (stream, forest reserve, range...);
#'   \item confidence levels, country check and herbarium-duplicate removal.
#' }
#'
#' @param x Occurrence records (see [prepare_occurrences()]).
#' @param gazetteer A `georef_gazetteer` from [read_gazetteer()].
#' @param params Parameters from [georef_params()].
#' @param verbose Print progress messages.
#' @return An object of class `georef_result`, a list with:
#'   \describe{
#'     \item{occ_all}{all records, with `geo_source` (`GBIF`, `Doublon_herbier`, `Collecteur_palier1`,
#'       `GeoNames_texte`, `GeoNames_texte_offset`, `GeoNames_flou`, `aucune`)
#'       and `geo_confiance`;}
#'     \item{georeferenced}{the records georeferenced by the method;}
#'     \item{occ_dedup}{records with coordinates, herbarium duplicates removed;}
#'     \item{conr_ready}{`ddlat`, `ddlon`, `tax` for 'ConR';}
#'     \item{anchors}{collector x year x month x place base;}
#'     \item{date_control}{leave-one-out control of the dates inferred from numbers;}
#'     \item{coordinate_checks}{corrected or suspect original coordinates;}
#'     \item{to_check}{a random sample of 100 georeferenced records to check by hand;}
#'     \item{params}{the parameters used.}
#'   }
#' @export
#' @examples
#' ex <- georef_example()
#' gaz <- read_gazetteer(ex$geonames_dir, countries = "CI", verbose = FALSE)
#' res <- georeference(ex$occurrences, gaz, georef_params(countries = "CI"), verbose = FALSE)
#' res
#' table(res$georeferenced$geo_source)
georeference <- function(x, gazetteer, params = georef_params(), verbose = TRUE) {
  p <- params
  msg <- function(...) if (verbose) message(...)
  occ <- prepare_occurrences(x, p)
  msg("Records: ", nrow(occ), " | with collector: ", sum(!is.na(occ$coll)),
      " | day-precise date: ", sum(occ$tprec %in% 0))

  date_ctrl <- data.frame()
  if (p$use_record_number) {
    r <- infer_dates_from_numbers(occ, p)
    occ <- r$data; date_ctrl <- r$control
    if (nrow(date_ctrl))
      msg(sprintf("Dates from collection numbers: %d records dated | control: exact %.1f%%, within 3 days %.1f%%",
                  attr(date_ctrl, "n_dated"), 100 * mean(date_ctrl$err_j == 0), 100 * mean(abs(date_ctrl$err_j) <= 3)))
  }
  occ <- check_coordinates(occ, gazetteer, p)
  if (p$check_coords) msg("Original coordinates: ", paste(names(table(occ$coord_statut)), table(occ$coord_statut),
                                                       sep = " = ", collapse = ", "))

  # ---- doublons d'herbier (meme collecteur, meme numero complet)
  geo_dup <- occ[0, ]
  if (p$use_duplicates) {
    geo_dup <- duplicate_step(occ, p)
    msg("Herbarium duplicates: coordinates reused for ", nrow(geo_dup), " records")
  }

  # ---- textes a georeferencer
  sans <- occ[(is.na(occ$decimalLatitude) | is.na(occ$decimalLongitude)) & occ$loc_txt0 != "" &
                !is.na(occ$countryCode) & occ$countryCode %in% gazetteer$countries & !occ$.rid %in% geo_dup$.rid, ]
  sans$loc_txt <- sans$loc_txt0
  sans$txt <- norm_text(sans$loc_txt)
  sans$adm1 <- rep(NA_character_, nrow(sans)); sans$adm2 <- rep(NA_character_, nrow(sans))
  if (p$use_admin && nrow(sans) && nrow(gazetteer$admin)) {
    am <- match_admin(sans$stateProvince, sans$county, sans$countryCode, gazetteer)
    sans$adm1 <- am$adm1; sans$adm2 <- am$adm2
    msg("Declared administrative unit recognised: ", sum(!is.na(sans$adm1)), " records")
  }
  sans$key <- paste(sans$txt, sans$countryCode, dplyr::coalesce(sans$adm1, ""), dplyr::coalesce(sans$adm2, ""), sep = "||")
  tok <- sans[!duplicated(sans$key), c("key", "txt", "loc_txt", "countryCode", "adm1", "adm2")]
  msg("Without coordinates (text + covered country): ", nrow(sans), " | unique texts: ", nrow(tok))
  cand <- match_texts(tok, gazetteer)
  if (p$fuzzy) cand <- add_fuzzy(cand, tok, gazetteer, p)
  msg("Texts with at least one recognised place name: ", length(unique(cand$key)))

  # ---- phase 2 : choix des lieux
  by_key0 <- split(cand, cand$key)
  res <- if (nrow(cand)) dplyr::bind_rows(lapply(names(by_key0), function(k) {
    i <- match(k, tok$key)
    cbind(key = k, resolve_text(by_key0[[k]], p, tok$adm1[i], tok$adm2[i]), stringsAsFactors = FALSE)
  })) else
    data.frame(key = character(0), lieux = character(0), n_lieux = integer(0), g_lat = numeric(0),
               g_lon = numeric(0), incert_m = numeric(0), etendue = logical(0), classe = character(0),
               adm_utilise = logical(0))
  res$incert_m <- pmax(dplyr::coalesce(res$incert_m, 0), p$min_uncertainty_m)
  fl <- tapply(cand$flou, cand$key, any)
  res$flou <- dplyr::coalesce(unname(fl[res$key]), FALSE)
  res$incert_m <- pmax(res$incert_m, ifelse(res$flou, p$fuzzy_min_uncertainty_m, 0))
  off <- cbind(key = tok$key, parse_offsets(tok$loc_txt))
  off_ok <- stats::setNames(!is.na(off$dist_m) & !is.na(off$azimuth), off$key)

  res_rec <- sans[, c(".rid", "key", "coll", "coll_all", "countryCode", "t", "tprec", "tdemi")] %>%
    dplyr::left_join(res, by = "key")
  nr <- nrow(res_rec)
  res_rec$niveau <- rep("texte", nr); res_rec$delta_j <- rep(NA_real_, nr)
  res_rec$copie <- rep(FALSE, nr); res_rec$d_ancre_km <- rep(NA_real_, nr)

  # ---- phase 1 : collecteur
  anchors <- list(base = data.frame())
  if (p$use_collector) {
    anchors <- build_anchors(occ, gazetteer, p)
    msg("Anchors (collector + date + plausible coordinates): ", nrow(anchors$refs),
        " | anchor-place links: ", nrow(anchors$links))
    if (nrow(anchors$links)) {
      by_key <- split(cand, cand$key)
      lk <- anchors$links
      lk$k <- if (p$team_anchors) lk$coll_all else lk$coll
      lk <- tidyr::separate_rows(lk, "k", sep = ";")
      lk <- as.data.frame(lk[!is.na(lk$k) & lk$k != "", ])
      rt_by <- split(lk, paste(lk$k, lk$countryCode, sep = "||"))
      out <- vector("list", nrow(res_rec))
      for (i in seq_len(nrow(res_rec))) {
        keys <- if (p$team_anchors && !is.na(res_rec$coll_all[i])) strsplit(res_rec$coll_all[i], ";")[[1]] else res_rec$coll[i]
        keys <- keys[!is.na(keys)]
        if (!length(keys) || is.na(res_rec$t[i])) next
        d <- by_key[[res_rec$key[i]]]; if (is.null(d)) next
        for (cl in keys) {
          rt <- rt_by[[paste(cl, res_rec$countryCode[i], sep = "||")]]; if (is.null(rt)) next
          r <- resolve_collector(d, rt, res_rec$t[i], res_rec$tprec[i], isTRUE(off_ok[res_rec$key[i]]),
                                 res_rec$tdemi[i], p)
          if (!is.null(r)) { out[[i]] <- c(list(idx = i), r); break }
        }
      }
      tmp <- dplyr::bind_rows(out)
      if (nrow(tmp)) {
        for (cn in c("lieux", "n_lieux", "g_lat", "g_lon", "incert_m", "niveau", "delta_j", "copie", "d_ancre_km"))
          res_rec[[cn]][tmp$idx] <- tmp[[cn]]
        res_rec$flou[tmp$idx] <- FALSE
      }
      msg("Records resolved by the collector step: ", nrow(tmp))
    }
  }

  # ---- distance + direction, confiance, source
  res_rec <- res_rec[!is.na(res_rec$lieux), ]
  res_rec <- dplyr::left_join(res_rec, off, by = "key")
  res_rec$applique <- !res_rec$copie & res_rec$n_lieux == 1 & !is.na(res_rec$dist_m) &
    !is.na(res_rec$azimuth) & res_rec$dist_m <= 100000
  if (any(res_rec$applique)) {
    w <- which(res_rec$applique)
    dp <- geosphere::destPoint(cbind(res_rec$g_lon[w], res_rec$g_lat[w]), res_rec$azimuth[w], res_rec$dist_m[w])
    res_rec$g_lon[w] <- dp[, 1]; res_rec$g_lat[w] <- dp[, 2]
    res_rec$incert_m[w] <- pmax(res_rec$incert_m[w], 0.2 * res_rec$dist_m[w],
                                ifelse(res_rec$flou[w], p$fuzzy_min_uncertainty_m, 0))
  }
  res_rec$geo_confiance <- dplyr::case_when(
    res_rec$niveau == "collecteur_palier1" ~ "haute",
    res_rec$flou & res_rec$n_lieux >= 2 ~ "moyenne (flou)",
    res_rec$flou ~ "basse (flou)",
    res_rec$n_lieux >= 2 & !res_rec$etendue ~ "haute",
    res_rec$n_lieux >= 2 ~ "moyenne",
    res_rec$applique & !res_rec$etendue ~ "moyenne (distance/direction)",
    res_rec$etendue ~ "basse (entite etendue)",
    TRUE ~ "moyenne")
  res_rec$geo_source <- dplyr::case_when(
    res_rec$niveau == "collecteur_palier1" ~ "Collecteur_palier1",
    res_rec$flou ~ "GeoNames_flou",
    res_rec$applique ~ "GeoNames_texte_offset",
    TRUE ~ "GeoNames_texte")
  res_rec <- res_rec[res_rec$incert_m <= p$max_uncertainty_m, ]
  if (!p$keep_medium_confidence) res_rec <- res_rec[res_rec$geo_confiance == "haute", ]
  if (!p$keep_fuzzy) res_rec <- res_rec[!res_rec$flou, ]

  # ---- recombinaison
  geo <- dplyr::inner_join(
    sans[, setdiff(names(sans), c("txt", "key"))],
    res_rec[, c(".rid", "lieux", "n_lieux", "g_lat", "g_lon", "incert_m", "geo_confiance", "geo_source",
                "niveau", "delta_j", "classe", "adm_utilise")], by = ".rid")
  geo$decimalLatitude <- geo$g_lat; geo$decimalLongitude <- geo$g_lon
  geo$coordinateUncertaintyInMeters <- geo$incert_m
  names(geo)[names(geo) == "niveau"] <- "geo_niveau"
  names(geo)[names(geo) == "delta_j"] <- "delta_j_ancre"
  names(geo)[names(geo) == "classe"] <- "feature_code"
  names(geo)[names(geo) == "adm_utilise"] <- "admin_unit_used"
  geo <- geo[, setdiff(names(geo), c("g_lat", "g_lon", "incert_m"))]

  if (p$country_check && nrow(geo) && requireNamespace("CoordinateCleaner", quietly = TRUE) &&
      requireNamespace("countrycode", quietly = TRUE)) {
    ok <- tryCatch({
      tmp <- geo
      tmp$iso3 <- countrycode::countrycode(tmp$countryCode, "iso2c", "iso3c")
      CoordinateCleaner::cc_coun(tmp, lon = "decimalLongitude", lat = "decimalLatitude",
                                 iso3 = "iso3", value = "flagged", verbose = FALSE)
    }, error = function(e) { message("cc_coun skipped: ", conditionMessage(e)); rep(TRUE, nrow(geo)) })
    msg("Reconstructed points outside the declared country (removed): ", sum(!ok))
    geo <- geo[ok, ]
  }
  if (nrow(geo_dup)) geo <- dplyr::bind_rows(geo_dup, geo)

  avec <- occ[!is.na(occ$decimalLatitude) & !is.na(occ$decimalLongitude), ]
  avec$geo_source <- rep("GBIF", nrow(avec))
  avec$geo_confiance <- as.character(ifelse(avec$coord_statut %in% "corrigee", "corrigee", "originale"))
  reste <- occ[!occ$.rid %in% c(avec$.rid, geo$.rid), ]
  reste$geo_source <- rep("aucune", nrow(reste)); reste$geo_confiance <- rep(NA_character_, nrow(reste))
  occ_all <- dplyr::bind_rows(avec, geo, reste)
  occ_all <- occ_all[order(occ_all$.rid), ]
  occ_coord <- occ_all[!is.na(occ_all$decimalLatitude) & !is.na(occ_all$decimalLongitude), ]
  k <- paste(occ_coord$species, round(occ_coord$decimalLatitude, 3), round(occ_coord$decimalLongitude, 3),
             occ_coord$recordedBy, occ_coord$recordNumber, occ_coord$eventDate)
  occ_dedup <- occ_coord[!duplicated(k), ]
  conr_ready <- data.frame(ddlat = occ_dedup$decimalLatitude, ddlon = occ_dedup$decimalLongitude,
                           tax = occ_dedup$species, stringsAsFactors = FALSE)
  conr_ready <- conr_ready[!is.na(conr_ready$tax), ]

  verif <- geo[, intersect(c("species", "countryCode", "recordedBy", "recordNumber", "date_source", "date_deduite",
                             "loc_txt", "lieux", "n_lieux", "geo_confiance", "geo_niveau", "decimalLatitude",
                             "decimalLongitude", "coordinateUncertaintyInMeters"), names(geo))]
  if (nrow(verif) > 100) verif <- verif[sample(nrow(verif), 100), ]
  coord_chk <- occ[occ$coord_statut %in% c("corrigee", "suspecte"),
                   c(".rid", "species", "countryCode", "recordedBy", "locality", "decimalLatitude_orig",
                     "decimalLongitude_orig", "decimalLatitude", "decimalLongitude", "coord_statut", "coord_correction")]

  out <- structure(list(occ_all = occ_all, georeferenced = geo, occ_coord = occ_coord, occ_dedup = occ_dedup,
                        conr_ready = conr_ready, anchors = anchors$base, date_control = date_ctrl,
                        coordinate_checks = coord_chk, to_check = verif, params = p,
                        n = c(records = nrow(occ), with_coordinates = nrow(avec), eligible = nrow(sans) + nrow(geo_dup),
                              georeferenced = nrow(geo), dedup = nrow(occ_dedup))),
                   class = "georef_result")
  if (verbose) print(out)
  out
}

#' @export
print.georef_result <- function(x, ...) {
  n <- x$n
  cat("<georef_result>\n")
  cat("  records:", n["records"], "| with coordinates:", n["with_coordinates"],
      "| eligible without coordinates:", n["eligible"], "\n")
  cat(sprintf("  georeferenced: %d (%.1f%% of eligible) | after duplicate removal: %d\n",
              n["georeferenced"], 100 * n["georeferenced"] / max(n["eligible"], 1), n["dedup"]))
  if (nrow(x$georeferenced)) {
    cat("  by source:\n"); print(table(x$georeferenced$geo_source))
    cat("  by confidence:\n"); print(table(x$georeferenced$geo_confiance))
  }
  invisible(x)
}

#' Write the outputs of georeference()
#'
#' @param x A `georef_result`.
#' @param dir Output folder.
#' @return Invisibly, the paths of the files written.
#' @export
write_georef <- function(x, dir = ".") {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  flat <- function(d) { for (n in names(d)) if (is.list(d[[n]]))
    d[[n]] <- vapply(d[[n]], function(z) paste(unlist(z), collapse = ";"), character(1)); d }
  f <- c(occ_all = "occ_all_geonames.csv", conr_ready = "occ_conr_ready.csv", to_check = "geonames_a_verifier.csv",
         anchors = "base_collecteur_toponyme_periode.csv", coordinate_checks = "coordonnees_corrigees_ou_suspectes.csv",
         date_control = "controle_dates_numero.csv")
  out <- character(0)
  for (n in names(f)) if (!is.null(x[[n]]) && nrow(as.data.frame(x[[n]]))) {
    path <- file.path(dir, f[[n]])
    utils::write.csv(flat(as.data.frame(x[[n]])), path, row.names = FALSE, na = "")
    out <- c(out, path)
  }
  invisible(out)
}
