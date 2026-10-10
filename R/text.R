# ---- normalisation des textes et des toponymes --------------------------------

#' Normalise a text for place-name matching
#'
#' Transliterates to ASCII, lower-cases, replaces punctuation by spaces and
#' squeezes white space.
#'
#' @param x Character vector.
#' @return Character vector.
#' @export
#' @examples
#' norm_text("For\\u00eat class\\u00e9e de Ta\\u00ef, pr\\u00e8s d'Abidjan")
norm_text <- function(x) {
  x <- stringi::stri_trans_general(as.character(x), "Latin-ASCII")
  x <- tolower(gsub("[^A-Za-z0-9 ]", " ", x))
  trimws(gsub("\\s+", " ", x))
}

# n-grammes de 1 a nmax mots d'un texte normalise
ngrams <- function(txt, nmax = 4) {
  w <- strsplit(txt, " ", fixed = TRUE)[[1]]
  w <- w[nzchar(w)]
  if (length(w) == 0) return(character(0))
  unique(unlist(lapply(seq_len(min(nmax, length(w))), function(n)
    vapply(seq_len(length(w) - n + 1),
           function(i) paste(w[i:(i + n - 1)], collapse = " "), ""))))
}

# noms retenus dans un texte : on retire les noms inclus dans un autre nom, 5 noms au plus
retained_names <- function(noms, max_names = 5) {
  noms <- unique(noms)
  inclus <- vapply(noms, function(a) any(noms != a &
            grepl(paste0(" ", a, " "), paste0(" ", noms, " "), fixed = TRUE)), logical(1))
  noms <- noms[!inclus]
  if (length(noms) > max_names) noms <- noms[order(-nchar(noms))][seq_len(max_names)]
  noms
}

# texte de localite : locality + verbatimLocality + municipality + county
loc_text <- function(df) {
  f <- function(v) if (v %in% names(df)) dplyr::coalesce(as.character(df[[v]]), "") else rep("", nrow(df))
  stringr::str_squish(paste(f("locality"), f("verbatimLocality"), f("municipality"), f("county")))
}

#' Common words ignored as stand-alone place names
#'
#' French and English landscape and administrative words that are also
#' GeoNames place names ("foret", "river", "village"...). They are ignored when
#' they occur alone.
#'
#' @return Character vector of normalised words.
#' @export
#' @examples
#' head(georef_stopwords())
georef_stopwords <- function() .stopwords

.stopwords <- unique(c(
  "foret", "forets", "route", "village", "riviere", "fleuve", "mont", "monts", "montagne",
  "station", "parc", "reserve", "centre", "nord", "sud", "est", "ouest", "pres", "entre", "environs",
  "ville", "camp", "pont", "marigot", "bas", "haut", "grand", "petit", "bois", "savane", "plateau",
  "vallee", "lac", "ile", "colline", "rocher", "chute", "cascade", "sommet", "falaise", "sentier",
  "piste", "bord", "classee", "classe", "region", "departement", "prefecture", "district", "zone",
  "plantation", "mission", "ecole", "hopital", "marche", "port", "cap", "pointe", "baie", "source",
  "lisiere", "versant", "flanc", "derriere", "devant", "apres", "avant", "environ",
  "kilometre", "kilometres", "localite", "inconnue", "unknown", "sans", "precision", "pays", "cote", "ivoire",
  "guinee", "liberia", "ghana", "mali", "togo", "benin", "burkina", "faso", "sierra", "leone", "afrique",
  "forest", "forests", "road", "river", "stream", "creek", "mountain", "mountains", "mount", "park",
  "center", "north", "south", "east", "west", "near", "between", "vicinity", "surroundings", "town", "city",
  "bridge", "upper", "lower", "great", "little", "small", "wood", "savanna", "valley", "lake", "island", "hill",
  "rock", "waterfall", "falls", "summit", "cliff", "trail", "path", "track", "edge", "classified", "school",
  "hospital", "market", "cape", "point", "bay", "border", "side", "behind", "front", "after", "before", "about",
  "kilometer", "kilometers", "mile", "miles", "locality", "without", "country", "africa"))

# ---- distance + direction ("10 km N de Man") -----------------------------------
.azimuths <- c(nord = 0, n = 0, north = 0, nordest = 45, ne = 45, northeast = 45,
               est = 90, e = 90, east = 90, sudest = 135, se = 135, southeast = 135,
               sud = 180, s = 180, south = 180, sudouest = 225, so = 225, sw = 225, southwest = 225,
               ouest = 270, o = 270, w = 270, west = 270, nordouest = 315, no = 315, nw = 315, northwest = 315)

#' Parse distance-direction expressions
#'
#' Finds expressions such as "10 km N de Man" or "5 miles west of Kumasi"
#' (eight directions, French or English, km or miles; the unit is mandatory).
#'
#' @param x Character vector of locality texts.
#' @return A data frame with the distance in metres (`dist_m`) and the azimuth
#'   in degrees (`azimuth`), `NA` when no expression is found.
#' @export
#' @examples
#' parse_offsets(c("10 km N de Man", "5 miles west of Kumasi", "Man"))
parse_offsets <- function(x) {
  raw <- gsub("(\\d),(\\d)", "\\1.\\2", tolower(stringi::stri_trans_general(as.character(x), "Latin-ASCII")))
  pat <- paste0("(\\d+(?:\\.\\d+)?)\\s*(km|kilometers?|kilometres?|miles?|mi)\\s*",
                "(?:to\\s+the\\s+|towards?\\s+|au\\s+|a\\s+|vers\\s+|de\\s+|du\\s+|of\\s+|from\\s+)?(?:l')?\\s*",
                "(nord[- ]?est|nord[- ]?ouest|sud[- ]?est|sud[- ]?ouest|nord|sud|est|ouest|",
                "north[- ]?east|north[- ]?west|south[- ]?east|south[- ]?west|north|south|east|west|",
                "ne|nw|no|se|sw|so|n|s|e|w|o)(?=[\\s,.;)]|$)")
  m <- stringr::str_match(raw, pat)
  mult <- ifelse(!is.na(m[, 3]) & m[, 3] %in% c("mi", "mile", "miles"), 1609.34, 1000)
  data.frame(dist_m = as.numeric(m[, 2]) * mult,
             azimuth = unname(.azimuths[gsub("[- ]", "", m[, 4])]),
             stringsAsFactors = FALSE)
}
