# ---- appariement des textes avec le gazetteer (phase 2) -------------------------

# candidats : n-grammes des textes joints aux noms du gazetteer (meme pays)
match_texts <- function(tok, gazetteer) {
  if (!nrow(tok)) return(data.frame(key = character(0), countryCode = character(0), nom_n = character(0),
                                    geonameid = character(0), lat = numeric(0), lon = numeric(0),
                                    fclass = character(0), fcode = character(0), admin1 = character(0),
                                    admin2 = character(0), pop = numeric(0), flou = logical(0)))
  ng <- lapply(tok$txt, ngrams)
  long <- data.frame(key = rep(tok$key, lengths(ng)), countryCode = rep(tok$countryCode, lengths(ng)),
                     nom_n = unlist(ng), stringsAsFactors = FALSE)
  cand <- dplyr::inner_join(long, gazetteer$names, by = c("nom_n", "countryCode"), relationship = "many-to-many")
  cand$flou <- rep(FALSE, nrow(cand))
  cand
}

# ---- types d'entites GeoNames -------------------------------------------------
.class_priority <- c(P = 1, T = 2, L = 3, H = 4)
.stream_codes <- c("STM", "STMI", "STMS", "STMX", "STMA", "STMB", "STMD", "STMC", "CNL", "STMM", "STMQ", "STMSB")
.extended_relief <- c("MTS", "HLLS", "RDGE", "PLAT", "UPLD", "VAL", "VALS", "PLN", "PLNX", "MTSU", "RNGA", "HMDA")
feature_uncertainty <- function(fclass, fcode, p) {
  km <- ifelse(fcode %in% "PPLC", p$city_km,
        ifelse(fcode %in% c("PPLA", "PPLA2"), p$admin_seat_km,
        ifelse(fclass %in% "P", 0,
        ifelse(fcode %in% .stream_codes, p$stream_km,
        ifelse(fclass %in% "H", 5,
        ifelse(fcode %in% .extended_relief, p$extended_relief_km,
        ifelse(fclass %in% "L", p$area_km, 0)))))))
  pmax(km * 1000, p$min_uncertainty_m)
}
is_extended <- function(fclass, fcode) fclass %in% c("H", "L") | fcode %in% .extended_relief

# choix du lieu d'un texte :
# - homonymes situes dans l'unite administrative declaree preferes (si l'un d'eux s'y trouve) ;
# - pour un meme nom : localite (P), puis relief, zone, hydrographie, puis population ;
# - plusieurs toponymes : combinaison la plus compacte ; position = barycentre des localites (P)
#   de la combinaison ; incertitude au moins egale au rayon propre au type d'entite.
resolve_text <- function(d, p, a1 = NA, a2 = NA, max_opt = 5, max_comb = 5000) {
  noms <- retained_names(d$nom_n)
  adm_ok <- FALSE
  opts <- lapply(noms, function(n) {
    x <- d[d$nom_n == n, , drop = FALSE]
    if (!is.na(a2) && any(paste(x$admin1, x$admin2, sep = ".") == a2)) {
      x <- x[paste(x$admin1, x$admin2, sep = ".") == a2, , drop = FALSE]; adm_ok <<- TRUE
    } else if (!is.na(a1) && any(x$admin1 %in% a1)) {
      x <- x[x$admin1 %in% a1, , drop = FALSE]; adm_ok <<- TRUE
    }
    x <- x[order(.class_priority[x$fclass], -x$pop), , drop = FALSE]
    x <- x[!duplicated(x$geonameid), , drop = FALSE]
    utils::head(x, max_opt)
  })
  if (length(opts) == 1) {
    o <- opts[[1]][1, ]
    return(data.frame(lieux = noms, n_lieux = 1L, g_lat = o$lat, g_lon = o$lon,
                      incert_m = feature_uncertainty(o$fclass, o$fcode, p), etendue = is_extended(o$fclass, o$fcode),
                      classe = o$fcode, adm_utilise = adm_ok, stringsAsFactors = FALSE))
  }
  g <- do.call(expand.grid, lapply(opts, function(o) seq_len(nrow(o))))
  if (nrow(g) > max_comb) g <- g[sample(nrow(g), max_comb), , drop = FALSE]
  pick <- function(ix) do.call(rbind, Map(function(o, i) o[i, c("lat", "lon", "fclass", "fcode")], opts, ix))
  sc <- apply(g, 1, function(ix) {
    q <- pick(ix)
    max(geosphere::distHaversine(cbind(q$lon, q$lat), c(mean(q$lon), mean(q$lat))))
  })
  ch <- pick(unlist(g[which.min(sc), ]))
  pos <- if (any(ch$fclass == "P")) ch[ch$fclass == "P", , drop = FALSE] else ch
  la <- mean(pos$lat); lo <- mean(pos$lon)
  disp <- if (nrow(pos) > 1) max(geosphere::distHaversine(cbind(pos$lon, pos$lat), c(lo, la))) else 0
  data.frame(lieux = paste(noms, collapse = " | "), n_lieux = length(noms), g_lat = la, g_lon = lo,
             incert_m = max(disp, feature_uncertainty(pos$fclass, pos$fcode, p)),
             etendue = !any(ch$fclass == "P"), classe = paste(unique(ch$fcode), collapse = "+"),
             adm_utilise = adm_ok, stringsAsFactors = FALSE)
}

# cle phonetique adaptee aux transcriptions francaises / ouest-africaines
phon_key <- function(x) {
  x <- gsub(" ", "", norm_text(x), fixed = TRUE)
  x <- gsub("eau", "o", x); x <- gsub("au", "o", x)
  x <- gsub("ou", "u", x);  x <- gsub("w", "u", x)
  x <- gsub("ai|ei|eu", "e", x)
  x <- gsub("ph", "f", x);  x <- gsub("sh|ch", "s", x)
  x <- gsub("gn", "ny", x)
  x <- gsub("qu|q|ck", "k", x)
  x <- gsub("c(?=[ei])", "s", x, perl = TRUE); x <- gsub("c", "k", x)
  x <- gsub("y", "i", x);   x <- gsub("z", "s", x)
  x <- gsub("h", "", x)
  x <- gsub("en", "an", x)
  x <- gsub("(.)\\1+", "\\1", x)
  ifelse(nchar(x) > 4, sub("[sxtd]$", "", x), x)
}

# recherche approchee (facultative, deconseillee) : ajoute des candidats marques flou = TRUE
add_fuzzy <- function(cand, tok, gazetteer, p) {
  if (!requireNamespace("stringdist", quietly = TRUE)) {
    warning("Package 'stringdist' needed for fuzzy matching; step skipped.")
    return(cand)
  }
  gl <- gazetteer$names
  gn_nm <- unique(gl[, c("nom_n", "countryCode")])
  gn_nm$cle <- phon_key(gn_nm$nom_n); gn_nm$len <- nchar(gn_nm$nom_n)
  gn_idx <- split(seq_len(nrow(gn_nm)), paste(gn_nm$countryCode, substr(gn_nm$cle, 1, 1)))
  gl <- gl[order(-gl$pop), ]
  gn_best <- gl[!duplicated(paste(gl$nom_n, gl$countryCode)), ]
  gn_best_k <- paste(gn_best$nom_n, gn_best$countryCode, sep = "||")
  memo <- new.env()
  flou_un <- function(q, cc) {
    k <- paste(q, cc, sep = "||")
    if (!is.null(memo[[k]])) return(memo[[k]])
    out <- NA_character_; qc <- phon_key(q)
    ids <- gn_idx[[paste(cc, substr(qc, 1, 1))]]
    if (!is.null(ids)) {
      s <- gn_nm[ids, ]
      s <- s[abs(s$len - nchar(q)) <= 2 & s$nom_n != q, ]
      if (nrow(s) > 0) {
        jw <- 1 - stringdist::stringdist(q, s$nom_n, method = "jw", p = 0.1)
        osa <- stringdist::stringdist(q, s$nom_n, method = "osa")
        phon <- s$cle == qc
        maxd <- if (nchar(q) >= 9) 2 else 1
        ok <- (phon & osa <= maxd + 1) | (jw >= p$fuzzy_jw_min & osa <= maxd)
        if (any(ok)) {
          s <- s[ok, ]; s$score <- (jw + 0.1 * phon - 0.05 * osa)[ok]
          s <- s[order(-s$score), ]
          out <- s$nom_n[1]
          if (nrow(s) > 1 && s$score[1] - s$score[2] < 0.02) {
            i <- match(paste(s$nom_n[1:2], cc, sep = "||"), gn_best_k)
            dd <- geosphere::distHaversine(c(gn_best$lon[i[1]], gn_best$lat[i[1]]),
                                           c(gn_best$lon[i[2]], gn_best$lat[i[2]]))
            if (is.na(dd) || dd > 20000) out <- NA_character_
          }
        }
      }
    }
    memo[[k]] <- out
    out
  }
  extraire <- function(x) {
    caps <- stringr::str_extract_all(x,
      "\\p{Lu}[\\p{L}'-]+(?:\\s+(?:(?:de|du|des|la|le|les|el|al|d'|l')\\s*)?\\p{Lu}[\\p{L}'-]+)*")[[1]]
    q <- if (length(caps)) unlist(lapply(norm_text(caps), ngrams, nmax = 3)) else {
      w <- ngrams(norm_text(x), 2); w[nchar(w) >= 6] }
    q <- unique(q)
    q[nchar(q) >= p$fuzzy_min_nchar & !q %in% .stopwords]
  }
  by_key <- split(cand$nom_n, cand$key)
  cibles <- if (p$fuzzy_scope == "all") tok else tok[!tok$key %in% names(by_key), ]
  cf <- dplyr::bind_rows(lapply(seq_len(nrow(cibles)), function(i) {
    q <- extraire(cibles$loc_txt[i])
    ex <- by_key[[cibles$key[i]]]
    if (length(q) && length(ex))
      q <- q[!vapply(q, function(a) any(grepl(paste0(" ", a, " "), paste0(" ", ex, " "), fixed = TRUE)), logical(1))]
    if (!length(q)) return(NULL)
    m <- unique(stats::na.omit(unname(vapply(q, flou_un, character(1), cc = cibles$countryCode[i]))))
    if (!length(m)) return(NULL)
    data.frame(key = cibles$key[i], countryCode = cibles$countryCode[i], nom_n = m, stringsAsFactors = FALSE)
  }))
  if (!nrow(cf)) return(cand)
  cf <- dplyr::inner_join(cf, gazetteer$names, by = c("nom_n", "countryCode"), relationship = "many-to-many")
  cf$flou <- TRUE
  out <- dplyr::bind_rows(cand, cf)
  out[!duplicated(out[, c("key", "nom_n", "geonameid")]), ]
}
