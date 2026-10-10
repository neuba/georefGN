# ---- collecteurs, numeros de collecte et dates ----------------------------------

.coll_stop <- c("unknown", "anonymous", "anonyme", "inconnu", "collector", "collecteur", "sn", "none", "na",
                "nn", "indet", "anon")
.coll_titles <- c("dr", "prof", "mr", "mrs", "mme", "mlle", "sir", "rev", "fr", "pere", "abbe", "frere", "soeur", "jr", "sr")
.coll_particles <- c("de", "da", "du", "des", "van", "von", "der", "den", "le", "la", "del", "dos", "di", "ten", "ter",
                     "d", "l", "vd", "v")

split_collectors <- function(s) {
  s <- gsub("\\bet al\\.?|\\bet coll\\.?|\\bal\\.|\\bcoll\\.|[\\[\\]()]", " ", s, ignore.case = TRUE, perl = TRUE)
  s <- gsub("\\.(?=\\p{L})", ". ", s, perl = TRUE)
  s <- gsub("(?<=\\.)\\s*,\\s*(?=\\p{Lu})", ";", s, perl = TRUE)
  p <- strsplit(s, "\\s*(;|\\||&|/|\\s+et\\s+|\\s+and\\s+|\\s+with\\s+|\\s+y\\s+)\\s*", perl = TRUE)[[1]]
  p <- trimws(p); p <- p[nzchar(p)]
  unlist(lapply(p, function(x) {
    if (!grepl(",", x, fixed = TRUE)) return(x)
    apres <- trimws(sub("^[^,]*,", "", x))
    tk <- strsplit(apres, "\\s+")[[1]]
    if (!grepl(".", apres, fixed = TRUE) && sum(nchar(tk) >= 2) >= 2) trimws(strsplit(x, ",")[[1]]) else x
  }))
}
surname_key <- function(tk) {
  w <- strsplit(norm_text(paste(tk, collapse = " ")), " ", fixed = TRUE)[[1]]
  w <- w[nzchar(w) & !w %in% .coll_particles & !w %in% .coll_titles]
  if (!length(w)) NA_character_ else paste(w, collapse = "")
}
initial_of <- function(g) { x <- norm_text(g); if (is.na(x) || !nzchar(x)) NA_character_ else substr(x, 1, 1) }

parse_person <- function(p) {
  empty <- list(nom = NA_character_, init = NA_character_, ambigu = NULL)
  p <- trimws(gsub("\\s+", " ", p))
  if (is.na(p) || !nzchar(p)) return(empty)
  if (grepl(",", p, fixed = TRUE))
    return(list(nom = surname_key(strsplit(trimws(sub(",.*$", "", p)), " ")[[1]]),
                init = initial_of(sub("^[^,]*,", "", p)), ambigu = NULL))
  tk <- strsplit(p, " ")[[1]]
  tk <- tk[nzchar(norm_text(tk)) & !norm_text(tk) %in% .coll_titles]
  if (!length(tk)) return(empty)
  sp <- gsub("[.-]", "", tk)
  is_init <- (grepl(".", tk, fixed = TRUE) & nchar(sp) <= 4 & grepl("^\\p{Lu}", tk, perl = TRUE)) |
             (grepl("^\\p{Lu}+$", tk, perl = TRUE) &
                (nchar(tk) <= 2 | (nchar(tk) == 3 & !grepl("[AEIOUY]", stringi::stri_trans_general(tk, "Latin-ASCII")))))
  if (any(is_init) && any(!is_init))
    return(list(nom = surname_key(tk[!is_init]), init = initial_of(paste(tk[is_init], collapse = " ")), ambigu = NULL))
  if (all(is_init)) return(empty)
  caps <- grepl("^\\p{Lu}{2,}$", gsub("[-']", "", tk), perl = TRUE)
  if (any(caps) && any(!caps))
    return(list(nom = surname_key(tk[caps]), init = initial_of(paste(tk[!caps], collapse = " ")), ambigu = NULL))
  if (length(tk) == 1) return(list(nom = surname_key(tk), init = NA_character_, ambigu = NULL))
  list(nom = NA_character_, init = NA_character_, ambigu = tk)
}

#' Parse collector names
#'
#' Reduces each collector to a key made of the normalised family name (accents,
#' hyphens and particles removed) and the initial of the given name when known.
#' Recognised forms include `"Ake Assi, L."`, `"L. Ake Assi"`, `"Ake Assi LA"`,
#' `"Jongkind CCH"`, `"AKE ASSI Laurent"`. When the order is unknown
#' (`"Ake Assi Laurent"` or `"Laurent Ake Assi"`), the family name is chosen among
#' the names seen unambiguously in the same data, otherwise the last word is used.
#' When a family name is carried by a single initial in the data, mentions without
#' initial are attached to it; when it is carried by several (`"Kone M."`,
#' `"Kone D."`), mentions without initial are ambiguous and get `NA`.
#'
#' @param x Character vector (e.g. GBIF `recordedBy`). The key of a name depends
#'   on the other names of `x`, so pass the whole data set.
#' @return Data frame with `coll` (key of the first collector) and `coll_all`
#'   (keys of all team members, separated by `";"`).
#' @export
#' @examples
#' parse_collectors(c("Ake Assi, L.", "Ake Assi LA", "Laurent Ake Assi",
#'                    "Jongkind CCH", "Jongkind & Abbiw", "Kone M.", "Kone D.", "Kone"))
parse_collectors <- function(x) {
  x <- as.character(x)
  u <- unique(x[!is.na(x)])
  out0 <- data.frame(coll = rep(NA_character_, length(x)), coll_all = rep(NA_character_, length(x)),
                     stringsAsFactors = FALSE)
  if (!length(u)) return(out0)
  pers <- lapply(u, function(s) lapply(split_collectors(s), parse_person))
  dico <- table(unlist(lapply(pers, function(pp) vapply(pp, function(z) z$nom, ""))), useNA = "no")
  resolve_order <- function(tk) {
    n <- length(tk); cands <- list()
    for (k in seq_len(n - 1)) {
      cands[[length(cands) + 1]] <- list(nom = surname_key(tk[1:k]), giv = tk[(k + 1):n])
      cands[[length(cands) + 1]] <- list(nom = surname_key(tk[(n - k + 1):n]), giv = tk[1:(n - k)])
    }
    f <- vapply(cands, function(cc) if (!is.na(cc$nom) && cc$nom %in% names(dico)) as.numeric(dico[[cc$nom]]) else 0, 0)
    b <- if (any(f > 0)) cands[[which.max(f)]] else list(nom = surname_key(tk[n]), giv = tk[-n])
    list(nom = b$nom, init = initial_of(paste(b$giv, collapse = " ")), ambigu = NULL)
  }
  pers <- lapply(pers, function(pp) lapply(pp, function(z) if (is.null(z$ambigu)) z else resolve_order(z$ambigu)))
  tab <- do.call(rbind, lapply(pers, function(pp) do.call(rbind, lapply(pp, function(z)
    data.frame(nom = z$nom, init = z$init, stringsAsFactors = FALSE)))))
  tab <- tab[!is.na(tab$nom), , drop = FALSE]
  inits <- tapply(tab$init, tab$nom, function(v) unique(v[!is.na(v)]), simplify = FALSE)
  key <- function(z) {
    if (is.na(z$nom) || z$nom %in% .coll_stop || nchar(z$nom) < 2) return(NA_character_)
    ii <- inits[[z$nom]]
    if (!is.na(z$init)) return(paste0(z$nom, "_", z$init))
    if (length(ii) == 1) return(paste0(z$nom, "_", ii))
    if (length(ii) == 0) return(z$nom)
    NA_character_
  }
  keys <- lapply(pers, function(pp) vapply(pp, key, ""))
  first <- vapply(keys, function(k) if (length(k)) k[1] else NA_character_, "")
  all_k <- vapply(keys, function(k) { k <- unique(k[!is.na(k)]); if (length(k)) paste(k, collapse = ";") else NA_character_ }, "")
  i <- match(x, u)
  data.frame(coll = unname(first[i]), coll_all = unname(all_k[i]), stringsAsFactors = FALSE)
}

#' Collector key
#'
#' Key of the first collector, see [parse_collectors()].
#'
#' @param x Character vector (e.g. the GBIF `recordedBy` field).
#' @return Character vector of keys (`NA` for unknown or ambiguous collectors).
#' @export
#' @examples
#' collector_key(c("Ake Assi, L.", "L. Ake Assi", "Chevalier, A.J.B.; Perrot", "s.n."))
collector_key <- function(x) parse_collectors(x)$coll

#' Parse collection numbers
#'
#' Extracts the collection number and its series prefix: `"LAA 1234"` gives
#' prefix `"LAA"` and number 1234; `"1234b"` gives 1234. Numbers written as
#' year-number (`"1975-123"`, `"2005/45"`) give prefix `"1975-"` and number 123
#' when the year is plausible and, if the collection year is known, equal to it.
#' `full` (upper-case letters and digits) identifies herbarium duplicates.
#'
#' @param x Character vector (e.g. GBIF `recordNumber`).
#' @param year Collection year (optional, recycled).
#' @return Data frame with `prefix`, `number` and `full`.
#' @export
#' @examples
#' parse_record_number(c("1234", "1234b", "LAA 1234", "1975-123", "s.n.", NA))
parse_record_number <- function(x, year = NA) {
  x0 <- toupper(trimws(as.character(x)))
  ymax <- as.integer(format(Sys.Date(), "%Y"))
  m1 <- stringr::str_match(x0, "^([A-Z]{0,6})[\\s.#:-]*((?:17|18|19|20)\\d{2})\\s*[-/]\\s*(\\d{1,6})(?!\\d)")
  m2 <- stringr::str_match(x0, "^([A-Z]{0,6})[\\s.#:-]*(\\d{1,7})")
  yr <- suppressWarnings(as.integer(m1[, 3]))
  ok1 <- !is.na(yr) & yr >= 1750 & yr <= ymax & (is.na(year) | yr == year)
  full <- gsub("[^A-Z0-9]", "", x0); full[is.na(full) | !grepl("[0-9]", full)] <- NA
  data.frame(prefix = ifelse(ok1, paste0(dplyr::coalesce(m1[, 2], ""), yr, "-"), dplyr::coalesce(m2[, 2], "")),
             number = suppressWarnings(as.numeric(ifelse(ok1, m1[, 4], m2[, 3]))),
             full = full, stringsAsFactors = FALSE)
}

# temps en jours depuis 1970 ; precision 0 = jour, 1 = mois, 2 = annee, 3 = jour deduit du numero
time_num <- function(jour, annee, mois) {
  t <- ifelse(!is.na(jour), as.numeric(jour),
       ifelse(!is.na(annee) & !is.na(mois), (annee - 1970) * 365.25 + (mois - 0.5) * 30.4375,
       ifelse(!is.na(annee), (annee - 1970) * 365.25 + 182.6, NA_real_)))
  p <- ifelse(!is.na(jour), 0L, ifelse(!is.na(annee) & !is.na(mois), 1L, ifelse(!is.na(annee), 2L, NA_integer_)))
  list(t = t, p = as.integer(p))
}
.floor_prec <- c(0, 15, 183, 0)
# ecart en jours entre deux dates, compte tenu de leur precision et de leur demi-largeur
delta_days <- function(t1, p1, t2, p2, h1 = 0, h2 = 0)
  pmax(abs(t1 - t2) + h1 + h2, pmax(.floor_prec[p1 + 1], .floor_prec[p2 + 1]))

#' Infer collection dates from collection numbers
#'
#' Collection numbers of a collector increase over time. For a record without a
#' day-precise date: (i) a dated duplicate (same collector, same number) gives
#' the exact date; (ii) otherwise the two dated numbers that bracket it
#' (\eqn{N_1 < N < N_2}, dates \eqn{d_1 \le d_2}) give an interval. Assuming that
#' a collector assigns at most `max_numbers_per_day` numbers a day, the record
#' cannot be earlier than \eqn{d_1 + \lceil (N-N_1)/100 \rceil - 1} nor later than
#' \eqn{d_2 - (\lceil (N_2-N)/100 \rceil - 1)}. The inferred date is the first
#' possible date (\eqn{d_1} when \eqn{N - N_1 \le 100}).
#'
#' A leave-one-out control re-dates every dated number from its neighbours,
#' ignoring its own date, to test the assumption on the data.
#'
#' @param occ A data frame prepared by [prepare_occurrences()].
#' @param params Parameters from [georef_params()].
#' @return A list with `data` (the updated data frame: columns `t`, `tprec`,
#'   `tdemi`, `date_source`, `date_deduite`, `date_deduite_max`) and `control`
#'   (one row per re-dated number, with the error in days).
#' @export
#' @examples
#' ex <- georef_example()
#' occ <- prepare_occurrences(ex$occurrences)
#' r <- infer_dates_from_numbers(occ)
#' table(r$data$date_source, useNA = "ifany")
#' mean(r$control$err_j == 0)
infer_dates_from_numbers <- function(occ, params = georef_params()) {
  p <- params
  ctrl <- data.frame()
  deduire_serie <- function(d, N, loo = FALSE) {
    n <- length(N); lo <- hi <- rep(NA_real_, n); type <- rep(NA_character_, n)
    if (!nrow(d) || !n) return(data.frame(lo = lo, hi = hi, type = type, stringsAsFactors = FALSE))
    i <- findInterval(N, d$num_coll)
    exact <- i > 0 & d$num_coll[pmax(i, 1)] == N
    if (!loo) { lo[exact] <- d$t[i[exact]]; hi[exact] <- lo[exact]; type[exact] <- "numero_doublon" }
    i1 <- ifelse(exact, i - 1, i); i2 <- ifelse(exact, i + 1, i + 1)
    ok <- (loo | !exact) & i1 >= 1 & i2 <= nrow(d)
    if (any(ok)) {
      w <- which(ok)
      N1 <- d$num_coll[i1[w]]; d1 <- d$t[i1[w]]; N2 <- d$num_coll[i2[w]]; d2 <- d$t[i2[w]]; Nw <- N[w]
      l <- d1 + pmax(0, ceiling((Nw - N1) / p$max_numbers_per_day) - 1)
      h <- d2 - pmax(0, ceiling((N2 - Nw) / p$max_numbers_per_day) - 1)
      c_ok <- d1 <= d2 & (Nw - N1) <= p$max_number_gap & (N2 - Nw) <= p$max_number_gap &
              (d2 - d1) <= p$max_bracket_days & l <= h
      lo[w[c_ok]] <- l[c_ok]; hi[w[c_ok]] <- h[c_ok]; type[w[c_ok]] <- "numero_encadre"
    }
    data.frame(lo = lo, hi = hi, type = type, stringsAsFactors = FALSE)
  }

  dat <- occ[!is.na(occ$serie) & occ$tprec %in% 0, c("serie", "num_coll", "t")]
  if (!nrow(dat)) return(list(data = occ, control = ctrl))
  dat <- dat %>% dplyr::group_by(.data$serie, .data$num_coll) %>%
    dplyr::summarise(t = if (dplyr::n_distinct(.data$t) == 1) dplyr::first(.data$t) else NA_real_, .groups = "drop")
  n_conflit <- sum(is.na(dat$t))
  dat <- dat[!is.na(dat$t), ]
  dat <- dat[order(dat$serie, dat$num_coll), ]
  by_s <- split(as.data.frame(dat[, c("num_coll", "t")]), dat$serie)

  ctrl <- dplyr::bind_rows(lapply(names(by_s), function(s) {
    d <- by_s[[s]]
    data.frame(serie = s, num_coll = d$num_coll, t_vrai = d$t, deduire_serie(d, d$num_coll, loo = TRUE),
               stringsAsFactors = FALSE)
  }))
  if (nrow(ctrl)) ctrl <- ctrl[!is.na(ctrl$lo), ]
  if (nrow(ctrl)) {
    ctrl$err_j <- ctrl$t_vrai - ctrl$lo
    ctrl$dans_intervalle <- ctrl$t_vrai >= ctrl$lo & ctrl$t_vrai <= ctrl$hi
  }

  cib <- which(!is.na(occ$serie) & !(occ$tprec %in% 0) & occ$serie %in% names(by_s))
  n_ok <- 0L
  if (length(cib)) {
    r <- dplyr::bind_rows(lapply(split(cib, occ$serie[cib]), function(ix)
      cbind(idx = ix, deduire_serie(by_s[[occ$serie[ix[1]]]], occ$num_coll[ix]))))
    r <- r[!is.na(r$lo), ]
    if (nrow(r)) {
      ym <- function(t) { x <- as.Date(t, origin = "1970-01-01")
        as.integer(format(x, "%Y")) * 12 + as.integer(format(x, "%m")) - 1 }
      an <- occ$annee[r$idx]; mo <- occ$mois[r$idx]
      compat <- (is.na(an) | (an >= floor(ym(r$lo) / 12) & an <= floor(ym(r$hi) / 12))) &
                (is.na(an) | is.na(mo) | (an * 12 + mo - 1 >= ym(r$lo) & an * 12 + mo - 1 <= ym(r$hi)))
      r <- r[compat, ]
      n_ok <- nrow(r)
      if (n_ok) {
        larg <- r$hi - r$lo
        occ$t[r$idx]     <- if (p$cautious_number_dates) r$lo + larg / 2 else r$lo
        occ$tdemi[r$idx] <- if (p$cautious_number_dates) larg / 2 else 0
        occ$tprec[r$idx] <- 3L
        occ$date_source[r$idx] <- r$type
        occ$date_deduite[r$idx] <- as.Date(r$lo, origin = "1970-01-01")
        occ$date_deduite_max[r$idx] <- as.Date(r$hi, origin = "1970-01-01")
      }
    }
  }
  attr(ctrl, "n_conflicts") <- n_conflit
  attr(ctrl, "n_dated") <- n_ok
  list(data = occ, control = ctrl)
}
