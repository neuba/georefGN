# petit gazetteer : 2 regions, homonymes, village et fleuve du meme nom, foret classee
make_gaz <- function() {
  dir <- file.path(tempdir(), "gaz_v5"); dir.create(dir, showWarnings = FALSE)
  G <- function(id, name, lat, lon, fc, fcode, a1, a2 = "", pop = 0, alt = "")
    paste(id, name, name, alt, lat, lon, fc, fcode, "CI", "", a1, a2, "", "", pop, "", "", "Africa/Abidjan", "2020-01-01", sep = "\t")
  writeLines(c(G(1, "Region des Lagunes", 5.6, -4.3, "A", "ADM1", "01", alt = "Lagunes"),
               G(2, "Region des Savanes", 9.4, -5.6, "A", "ADM1", "02", alt = "Savanes"),
               G(3, "Prefecture de Dabou", 5.4, -4.4, "A", "ADM2", "01", "011"),
               G(201, "Kouibli", 5.50, -4.50, "P", "PPL", "01", "011", 100),
               G(202, "Kouibli", 9.30, -5.70, "P", "PPL", "02", "021", 5000),
               G(203, "Bandama", 6.80, -5.30, "P", "PPL", "01", "", 0),
               G(204, "Bandama", 8.90, -5.00, "H", "STM", "02", "", 0),
               G(205, "Yapo", 5.75, -4.10, "L", "RESF", "01", "", 0),
               G(206, "Daloa", 6.88, -6.45, "P", "PPL", "01", "", 200000)),
             file.path(dir, "CI.txt"))
  read_gazetteer(dir, countries = "CI", verbose = FALSE)
}
gaz <- make_gaz()
p <- georef_params(countries = "CI", use_collector = FALSE)

test_that("declared administrative units are matched", {
  a <- match_admin(c("Lagunes", "Region des Savanes", "Inconnue", NA), c(NA, NA, NA, "Dabou"), "CI", gaz)
  expect_equal(a$adm1, c("01", "02", NA, "01"))
  expect_equal(a$adm2[4], "01.011")
})

test_that("stateProvince selects the right homonym; populated places come before streams", {
  occ <- data.frame(species = "Sp a", countryCode = "CI", locality = c("Kouibli", "Kouibli", "Bandama"),
                    stateProvince = c("Lagunes", NA, NA), recordedBy = c("A", "B", "C"),
                    stringsAsFactors = FALSE)
  r <- georeference(occ, gaz, p, verbose = FALSE)$georeferenced
  r <- r[order(r$.rid), ]
  expect_equal(r$decimalLatitude[1], 5.50)     # Lagunes homonym, not the more populated one
  expect_equal(r$decimalLatitude[2], 9.30)     # no unit: most populated
  expect_true(r$admin_unit_used[1])
  expect_equal(r$decimalLatitude[3], 6.80)     # village Bandama, not the river
})

test_that("extended features get a larger uncertainty and low confidence", {
  occ <- data.frame(species = "Sp a", countryCode = "CI", locality = "Yapo", recordedBy = "A", stringsAsFactors = FALSE)
  r <- georeference(occ, gaz, p, verbose = FALSE)$georeferenced
  expect_equal(r$coordinateUncertaintyInMeters, 15000)
  expect_equal(r$geo_confiance, "basse (entite etendue)")
})

test_that("herbarium duplicates reuse coordinates even without locality text", {
  occ <- data.frame(species = "Sp a", countryCode = "CI",
                    decimalLatitude = c(6.88, NA, NA), decimalLongitude = c(-6.45, NA, NA),
                    locality = c("Daloa", "", ""), recordedBy = c("Ake Assi, L.", "L. Ake Assi", "Ake Assi LA"),
                    recordNumber = c("1234", "1234", "1235"), eventDate = c("1970-01-02", NA, NA),
                    stringsAsFactors = FALSE)
  res <- georeference(occ, gaz, georef_params(countries = "CI"), verbose = FALSE)
  g <- res$georeferenced
  expect_equal(nrow(g), 1)
  expect_equal(g$geo_source, "Doublon_herbier")
  expect_equal(g$decimalLatitude, 6.88)
})
