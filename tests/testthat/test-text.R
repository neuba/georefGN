test_that("norm_text normalises accents, case and punctuation", {
  expect_equal(norm_text("Forêt  classée de Taï, près d'Abidjan"),
               "foret classee de tai pres d abidjan")
  expect_true(is.na(norm_text(NA)))
})

test_that("ngrams returns all 1- to 4-word sequences", {
  g <- georefGN:::ngrams("foret de tai sud")
  expect_true(all(c("foret", "foret de tai", "foret de tai sud", "tai sud") %in% g))
  expect_length(georefGN:::ngrams(""), 0)
})

test_that("retained_names drops names included in longer ones", {
  expect_equal(georefGN:::retained_names(c("tai", "foret de tai", "man")), c("foret de tai", "man"))
})

test_that("parse_offsets reads French and English expressions", {
  o <- parse_offsets(c("10 km N de Man", "5 miles west of Kumasi", "2,5 km au sud-est de Daloa", "Man", "parcelle 12"))
  expect_equal(o$dist_m[1:3], c(10000, 5 * 1609.34, 2500))
  expect_equal(o$azimuth[1:3], c(0, 270, 135))
  expect_true(all(is.na(o$dist_m[4:5])))
})
