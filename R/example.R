#' Synthetic example data
#'
#' Writes a small fictitious GeoNames file (`CI.txt`, 30 invented villages,
#' two of them homonyms, plus one historical alternate name) to a temporary
#' folder and generates fictitious occurrence records of three collectors with
#' sequential collection numbers. Part of the records have no coordinates and
#' part have no date. The data are invented and only serve for examples and tests.
#'
#' @param n_days Number of collecting days per collector.
#' @param seed Random seed (the global random state is restored afterwards).
#' @return A list with `occurrences` (data frame in GBIF format), `truth`
#'   (true coordinates and dates) and `geonames_dir` (folder to pass to
#'   [read_gazetteer()]).
#' @export
#' @examples
#' ex <- georef_example()
#' head(ex$occurrences)
georef_example <- function(n_days = 60, seed = 1) {
  if (exists(".Random.seed", envir = globalenv())) {
    old <- get(".Random.seed", envir = globalenv())
    on.exit(assign(".Random.seed", old, envir = globalenv()), add = TRUE)
  }
  set.seed(seed)
  dir <- file.path(tempdir(), paste0("georefGN_example_", seed))
  dir.create(file.path(dir, "alternatenames"), recursive = TRUE, showWarnings = FALSE)
  np <- 30
  syl <- c("ba", "ko", "di", "gue", "lo", "ma", "ni", "ou", "sa", "te", "zou", "bli", "dou", "fla", "kro")
  noms <- vapply(seq_len(np), function(i) paste0(toupper(substr(s <- paste(sample(syl, 3), collapse = ""), 1, 1)),
                                                 substr(s, 2, nchar(s))), "")
  noms <- make.unique(noms, sep = "")
  noms[c(5, 6)] <- "Kouibli"
  pl <- data.frame(id = 1000 + seq_len(np), name = noms, lat = stats::runif(np, 5, 10), lon = stats::runif(np, -8, -3),
                   pop = round(stats::rlnorm(np, 7, 1.5)), stringsAsFactors = FALSE)
  L <- function(id, name, lat, lon, pop) paste(id, name, stringi::stri_trans_general(name, "Latin-ASCII"), "",
                                               round(lat, 5), round(lon, 5), "P", "PPL", "CI", "", "01", "", "", "",
                                               pop, "", "", "Africa/Abidjan", "2020-01-01", sep = "\t")
  writeLines(mapply(L, pl$id, pl$name, pl$lat, pl$lon, pl$pop), file.path(dir, "CI.txt"), useBytes = TRUE)
  writeLines(paste(1, pl$id[7], "fr", paste0("Ancien", pl$name[7]), "", "", "", "1", "", "", sep = "\t"),
             file.path(dir, "alternatenames", "CI.txt"))

  colls <- c("Ak\u00e9-Assi, L.", "Chevalier, A.J.B.", "Adjanohoun, E.")
  rows <- list()
  for (cl in seq_along(colls)) {
    jours <- as.Date("1960-01-01") + (cl - 1) * 900 + sort(sample(0:700, n_days))
    num <- 100 * cl
    for (k in seq_along(jours)) {
      nj <- sample(3:25, 1); p <- sample(np, 1)
      rows[[length(rows) + 1]] <- data.frame(num = num:(num + nj - 1), jour = jours[k], p = p, coll = colls[cl])
      num <- num + nj
    }
  }
  x <- do.call(rbind, rows); n <- nrow(x)
  lat <- pl$lat[x$p] + stats::rnorm(n, 0, 0.02); lon <- pl$lon[x$p] + stats::rnorm(n, 0, 0.02)
  loc <- pl$name[x$p]
  loc[x$p == 7 & stats::runif(n) < 0.5] <- paste0("Ancien", pl$name[7])
  occ <- data.frame(species = paste("Species", sample(LETTERS[1:15], n, TRUE)), countryCode = "CI",
                    decimalLatitude = lat, decimalLongitude = lon, coordinateUncertaintyInMeters = NA_real_,
                    locality = paste0(sample(c("", "pr\u00e8s de ", "for\u00eat de "), n, TRUE), loc),
                    eventDate = as.character(x$jour), recordedBy = x$coll, recordNumber = as.character(x$num),
                    stringsAsFactors = FALSE)
  truth <- data.frame(row = seq_len(n), lat = lat, lon = lon, date = x$jour)
  sc <- sample(n, round(0.25 * n))
  occ$decimalLatitude[sc] <- NA; occ$decimalLongitude[sc] <- NA
  nd <- sample(sc, round(0.4 * length(sc))); occ$eventDate[nd] <- NA
  sw <- sample(setdiff(seq_len(n), sc), 5)
  tmp <- occ$decimalLatitude[sw]; occ$decimalLatitude[sw] <- occ$decimalLongitude[sw]; occ$decimalLongitude[sw] <- tmp
  list(occurrences = occ, truth = truth, geonames_dir = dir)
}
