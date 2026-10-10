#' Download GeoNames country files
#'
#' Downloads the GeoNames country dumps (`XX.zip`) and, optionally, the full
#' per-country alternate-name files (`alternatenames/XX.zip`, which contain
#' spelling variants and historical names). GeoNames data are licensed under
#' CC BY 4.0 (<https://www.geonames.org>).
#'
#' @param countries ISO 3166-1 alpha-2 country codes.
#' @param dir Destination folder.
#' @param altnames Also download the alternate-name files.
#' @param overwrite Download again files already present.
#' @return Invisibly, the paths of the downloaded files.
#' @export
#' @examples
#' \dontrun{
#' download_geonames(c("CI", "GN", "LR"), dir = "geonames")
#' }
download_geonames <- function(countries, dir = "geonames", altnames = TRUE, overwrite = FALSE) {
  base <- "https://download.geonames.org/export/dump/"
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  out <- character(0)
  get1 <- function(url, dest) {
    if (file.exists(dest) && !overwrite) return(dest)
    utils::download.file(url, dest, mode = "wb", quiet = TRUE)
    dest
  }
  for (cc in toupper(countries)) {
    out <- c(out, get1(paste0(base, cc, ".zip"), file.path(dir, paste0(cc, ".zip"))))
    if (altnames) {
      d2 <- file.path(dir, "alternatenames"); dir.create(d2, showWarnings = FALSE)
      out <- c(out, get1(paste0(base, "alternatenames/", cc, ".zip"), file.path(d2, paste0(cc, ".zip"))))
    }
  }
  invisible(out)
}

#' Read the GeoNames gazetteer
#'
#' Reads the GeoNames country files and builds the table of normalised names
#' used for matching. Each place contributes its main name, ASCII name, the
#' `alternatenames` column and, when the files are present, the full
#' alternate-name files (variants, local-language and historical names). All
#' names of a place point to its `geonameid`, which is used to compare places
#' independently of their spelling.
#'
#' Names shorter than four characters are dropped, except three-character names
#' of places with more than 5 000 inhabitants ("Man"), as well as the common
#' words of [georef_stopwords()].
#'
#' @param dir Folder containing `XX.txt` or `XX.zip` files (see [download_geonames()]).
#' @param countries ISO 3166-1 alpha-2 codes.
#' @param fclasses Feature classes kept (see [georef_params()]).
#' @param altnames_dir Folder of the full alternate-name files (they have the
#'   same names as the main files, so they must be in a sub-folder).
#'   `NULL` to ignore them.
#' @param exclude_codes Pseudo-language codes of alternate names that are not
#'   names (links, postal codes, airport codes, abbreviations).
#' @param keep_historic Keep historical names (useful for old specimens).
#' @param keep_colloquial Keep colloquial names.
#' @param verbose Print a summary.
#' @return An object of class `georef_gazetteer`: a list with `names` (one row
#'   per normalised name and place) and `places` (populated places used to check
#'   coordinates).
#' @export
#' @examples
#' ex <- georef_example()
#' gaz <- read_gazetteer(ex$geonames_dir, countries = "CI")
#' gaz
read_gazetteer <- function(dir = "geonames",
                           countries = georef_params()$countries,
                           fclasses = georef_params()$fclasses,
                           altnames_dir = file.path(dir, "alternatenames"),
                           exclude_codes = c("link", "wkdt", "post", "iata", "icao", "faac",
                                             "tcid", "unlc", "abbr", "fr_1793"),
                           keep_historic = TRUE, keep_colloquial = FALSE, verbose = TRUE) {
  cols <- c("geonameid", "name", "asciiname", "alternatenames", "lat", "lon", "fclass", "fcode",
            "countryCode", "cc2", "admin1", "admin2", "admin3", "admin4", "pop", "elev", "dem", "tz", "mod")
  cls <- c(rep("character", 4), "numeric", "numeric", rep("character", 8), "numeric", rep("character", 4))
  lire <- function(d, p, alt = FALSE) {
    f <- file.path(d, paste0(p, ".txt")); z <- file.path(d, paste0(p, ".zip"))
    if (!file.exists(f) && file.exists(z)) utils::unzip(z, files = paste0(p, ".txt"), exdir = d)
    if (!file.exists(f) || file.size(f) == 0) return(NULL)
    if (!alt) return(utils::read.delim(f, header = FALSE, quote = "", comment.char = "", col.names = cols,
                                       colClasses = cls, encoding = "UTF-8", na.strings = "", fill = TRUE))
    x <- utils::read.delim(f, header = FALSE, quote = "", comment.char = "", colClasses = "character",
                           encoding = "UTF-8", na.strings = "", fill = TRUE)
    x <- x[, seq_len(min(10, ncol(x))), drop = FALSE]
    names(x) <- c("altid", "geonameid", "lang", "nom", "pref", "short", "colloq", "hist", "from", "to")[seq_len(ncol(x))]
    for (v in c("colloq", "hist", "lang")) if (!v %in% names(x)) x[[v]] <- NA_character_
    x
  }
  countries <- toupper(countries)
  gn <- dplyr::bind_rows(lapply(countries, function(p) lire(dir, p)))
  if (!nrow(gn)) stop("No GeoNames file found in '", dir, "' for: ", paste(countries, collapse = ", "))
  manq <- setdiff(countries, unique(gn$countryCode))
  if (length(manq)) warning("No GeoNames entries for: ", paste(manq, collapse = ", "))
  gn$pop[is.na(gn$pop)] <- 0
  gn_f <- gn[gn$fclass %in% fclasses, ]
  keep <- c("geonameid", "nom", "countryCode", "lat", "lon", "fclass", "fcode", "admin1", "admin2", "pop")

  noms_base <- gn_f %>%
    dplyr::mutate(nom = paste(.data$name, .data$asciiname, dplyr::coalesce(.data$alternatenames, ""), sep = ",")) %>%
    tidyr::separate_rows("nom", sep = ",") %>%
    dplyr::select(dplyr::all_of(keep))
  noms_alt <- NULL; n_alt <- 0L
  if (!is.null(altnames_dir) && dir.exists(altnames_dir)) {
    alt <- dplyr::bind_rows(lapply(countries, function(p) lire(altnames_dir, p, alt = TRUE)))
    if (nrow(alt)) {
      alt <- alt[!is.na(alt$nom) & !dplyr::coalesce(alt$lang, "") %in% exclude_codes & !grepl("^https?://", alt$nom), ]
      if (!keep_historic)   alt <- alt[!dplyr::coalesce(alt$hist, "") %in% "1", ]
      if (!keep_colloquial) alt <- alt[!dplyr::coalesce(alt$colloq, "") %in% "1", ]
      noms_alt <- alt[, c("geonameid", "nom")] %>%
        dplyr::inner_join(gn_f[, c("geonameid", "countryCode", "lat", "lon", "fclass", "fcode", "admin1", "admin2", "pop")],
                          by = "geonameid") %>%
        dplyr::select(dplyr::all_of(keep))
      n_alt <- nrow(alt)
    }
  }
  nettoyer <- function(x) x %>%
    dplyr::mutate(nom_n = norm_text(.data$nom)) %>%
    dplyr::filter(.data$nom_n != "", !.data$nom_n %in% .stopwords,
                  nchar(.data$nom_n) >= 4 | (nchar(.data$nom_n) == 3 & .data$pop >= 5000)) %>%
    dplyr::select("nom_n", "countryCode", "geonameid", "lat", "lon", "fclass", "fcode", "admin1", "admin2", "pop") %>%
    dplyr::distinct()
  names_tab <- nettoyer(dplyr::bind_rows(noms_base, noms_alt))

  places <- gn[gn$fclass == "P" & !is.na(gn$lat) & !is.na(gn$lon), c("countryCode", "lat", "lon")]
  idx <- split(seq_len(nrow(places)), paste(places$countryCode, floor(places$lat / 0.5), floor(places$lon / 0.5)))
  # unites administratives ADM1 / ADM2 (pour stateProvince et county)
  adm <- gn[gn$fcode %in% c("ADM1", "ADM2"), , drop = FALSE]
  admin <- if (nrow(adm)) {
    a <- data.frame(countryCode = adm$countryCode, level = adm$fcode, admin1 = adm$admin1,
                    admin2 = ifelse(adm$fcode == "ADM2", paste(adm$admin1, adm$admin2, sep = "."), NA_character_),
                    nom = paste(adm$name, adm$asciiname, dplyr::coalesce(adm$alternatenames, ""), sep = ","),
                    stringsAsFactors = FALSE)
    a <- tidyr::separate_rows(a, "nom", sep = ",")
    a$nom_a <- clean_admin(a$nom)
    a <- a[nchar(a$nom_a) >= 3, c("countryCode", "level", "admin1", "admin2", "nom_a")]
    as.data.frame(unique(a))
  } else data.frame(countryCode = character(0), level = character(0), admin1 = character(0),
                    admin2 = character(0), nom_a = character(0))
  out <- structure(list(names = as.data.frame(names_tab), places = places, index = idx, admin = admin,
                        countries = countries, fclasses = fclasses, n_entries = nrow(gn),
                        n_altnames = n_alt), class = "georef_gazetteer")
  if (verbose) print(out)
  out
}

#' @export
print.georef_gazetteer <- function(x, ...) {
  cat("<georef_gazetteer>", x$n_entries, "GeoNames entries ;", nrow(x$names), "name-place pairs ;",
      length(unique(x$names$nom_n)), "distinct normalised names\n")
  cat("  countries:", paste(x$countries, collapse = ", "), "| classes:", paste(x$fclasses, collapse = ", "),
      "| full alternate names read:", x$n_altnames, "\n")
  invisible(x)
}

# distance (km) au lieu peuple GeoNames le plus proche du pays declare (Inf si aucun a ~1)
dist_to_places <- function(lat, lon, cc, gaz) {
  out <- rep(NA_real_, length(lat))
  for (i in seq_along(lat)) {
    if (is.na(lat[i]) || is.na(lon[i]) || is.na(cc[i]) || abs(lat[i]) > 90 || abs(lon[i]) > 180) next
    ci <- floor(lat[i] / 0.5); cj <- floor(lon[i] / 0.5)
    ks <- paste(cc[i], rep(ci + (-1:1), each = 3), rep(cj + (-1:1), times = 3))
    ids <- unlist(gaz$index[ks], use.names = FALSE)
    out[i] <- if (length(ids)) min(geosphere::distHaversine(c(lon[i], lat[i]),
                                   cbind(gaz$places$lon[ids], gaz$places$lat[ids]))) / 1000 else Inf
  }
  out
}

# ---- unites administratives -----------------------------------------------------
.admin_generic <- c("region", "regions", "prefecture", "prefectures", "sous", "district", "districts", "departement",
  "departements", "province", "state", "county", "division", "cercle", "circle", "commune", "autonome", "autonomous",
  "of", "de", "du", "des", "la", "le", "les", "l", "d", "et", "the", "zone", "gouvernorat", "municipality", "urban",
  "urbaine", "rural", "rurale", "ville", "city", "capital", "capitale")
clean_admin <- function(x) vapply(strsplit(norm_text(x), " ", fixed = TRUE), function(v)
  paste(v[nzchar(v) & !v %in% .admin_generic], collapse = " "), "")

admin_units <- function(txt, cc, admin) {
  a <- admin[admin$countryCode == cc, , drop = FALSE]
  s <- clean_admin(txt)
  if (!nrow(a) || is.na(txt) || is.na(s) || !nzchar(s)) return(a[0, ])
  m <- a[a$nom_a == s, , drop = FALSE]
  if (!nrow(m)) m <- a[nchar(a$nom_a) >= 4 & vapply(a$nom_a, function(n)
    grepl(paste0(" ", n, " "), paste0(" ", s, " "), fixed = TRUE), logical(1)), , drop = FALSE]
  m
}

#' Match declared administrative units to GeoNames
#'
#' Matches `stateProvince` and `county` to the ADM1 / ADM2 units of the
#' gazetteer (exact match after removing generic words such as "region" or
#' "prefecture", otherwise a unit name contained in the text). A unit is returned
#' only when it is unambiguous.
#'
#' @param state,county Character vectors (GBIF `stateProvince`, `county`).
#' @param country ISO2 country codes.
#' @param gazetteer A `georef_gazetteer`.
#' @return Data frame with `adm1` (GeoNames admin1 code) and `adm2`
#'   (`"admin1.admin2"`), `NA` when unknown.
#' @export
match_admin <- function(state, county, country, gazetteer) {
  n <- max(length(state), length(county), length(country))
  state <- rep_len(as.character(state), n); county <- rep_len(as.character(county), n)
  country <- rep_len(as.character(country), n)
  key <- paste(state, county, country, sep = "||")
  u <- which(!duplicated(key))
  one <- function(i) {
    m1 <- admin_units(state[i], country[i], gazetteer$admin); m2 <- admin_units(county[i], country[i], gazetteer$admin)
    a1_1 <- unique(m1$admin1); a1_2 <- unique(m2$admin1)
    a1 <- unique(if (length(a1_1) && length(a1_2)) intersect(a1_1, a1_2) else c(a1_1, a1_2))
    a2 <- unique(stats::na.omit(c(m1$admin2, m2$admin2)))
    a2 <- a2[sub("\\..*$", "", a2) %in% (if (length(a1)) a1 else sub("\\..*$", "", a2))]
    c(if (length(a1) == 1) a1 else NA_character_, if (length(a2) == 1) a2 else NA_character_)
  }
  r <- if (length(u)) t(vapply(u, one, character(2))) else matrix(character(0), 0, 2)
  j <- match(key, key[u])
  data.frame(adm1 = r[j, 1], adm2 = r[j, 2], stringsAsFactors = FALSE)
}
