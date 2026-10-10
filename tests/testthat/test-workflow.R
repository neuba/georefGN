ex <- georef_example(n_days = 30)
gaz <- read_gazetteer(ex$geonames_dir, countries = "CI", verbose = FALSE)
p <- georef_params(countries = "CI")

test_that("the gazetteer includes full alternate names", {
  expect_s3_class(gaz, "georef_gazetteer")
  expect_true(any(grepl("^ancien", gaz$names$nom_n)))
})

test_that("swapped coordinates are corrected", {
  occ <- check_coordinates(prepare_occurrences(ex$occurrences), gaz, p)
  expect_true(sum(occ$coord_statut == "corrigee") >= 4)
  fixed <- occ[occ$coord_statut == "corrigee", ]
  tr <- ex$truth[fixed$.rid, ]
  expect_true(all(abs(fixed$decimalLatitude - tr$lat) < 1e-6))
})

test_that("georeference() reconstructs points close to the truth", {
  res <- georeference(ex$occurrences, gaz, p, verbose = FALSE)
  expect_s3_class(res, "georef_result")
  g <- res$georeferenced
  expect_gt(nrow(g), 0.8 * res$n[["eligible"]])
  e <- geosphere::distHaversine(cbind(g$decimalLongitude, g$decimalLatitude),
                                cbind(ex$truth$lon[g$.rid], ex$truth$lat[g$.rid])) / 1000
  expect_lt(stats::median(e[g$geo_source == "Collecteur_palier1"]), 10)
  expect_true(all(c("ddlat", "ddlon", "tax") %in% names(res$conr_ready)))
  f <- write_georef(res, file.path(tempdir(), "out_georef"))
  expect_true(all(file.exists(f)))
})

test_that("without the collector step, only phase 2 sources are produced", {
  res <- georeference(ex$occurrences, gaz, georef_params(countries = "CI", use_collector = FALSE), verbose = FALSE)
  expect_false("Collecteur_palier1" %in% res$georeferenced$geo_source)
})

test_that("validate_georef() returns paired summaries", {
  v <- validate_georef(ex$occurrences, gaz, p, n_sample = 20, n_rep = 2, verbose = FALSE)
  expect_s3_class(v, "georef_validation")
  expect_true(all(c("taux_georef_pct", "mediane_km") %in% as.character(v$summary$mesure)))
  expect_equal(nrow(v$by_replicate), 2 * 3 * 2)
})
