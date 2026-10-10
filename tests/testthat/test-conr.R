test_that("provisional_category applies criterion B thresholds and locations", {
  expect_equal(provisional_category(c(50, 3000, 1e5, 3000), c(8, 40, 300, 40), c(1, 4, 20, 12)),
               c("CR", "EN", "LC/NT", "LC/NT"))
  expect_true(is.na(provisional_category(NA, NA, NA)))
})

test_that("compare_reference counts agreements", {
  w <- data.frame(tax = c("a", "b", "c"), Cat_prov_A = c("EN", "VU", "LC/NT"),
                  Cat_prov_C = c("EN", "LC/NT", "LC/NT"))
  ref <- data.frame(tax = c("a", "b", "c"), Category_Final = c("EN", "VU", "VU"))
  r <- compare_reference(w, ref)
  expect_equal(r$accord, c(2, 1))
  expect_equal(r$moins_menacee, c(1, 2))
})

test_that("compare_conr runs when ConR is installed", {
  skip_if_not_installed("ConR")
  ex <- georef_example(n_days = 20)
  gaz <- read_gazetteer(ex$geonames_dir, countries = "CI", verbose = FALSE)
  res <- georeference(ex$occurrences, gaz, georef_params(countries = "CI"), verbose = FALSE)
  cmp <- compare_conr(res, verbose = FALSE)
  expect_s3_class(cmp, "georef_conr")
})
