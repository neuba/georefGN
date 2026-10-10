test_that("collector keys unify spellings and keep the given-name initial", {
  x <- c("Ak\u00e9 Assi, L.", "L. Ak\u00e9 Assi", "Ak\u00e9 Assi LA", "AKE ASSI Laurent", "Ak\u00e9 Assi Laurent",
         "Laurent Ak\u00e9 Assi", "Jongkind CCH", "Jongkind, C.C.H.", "Kon\u00e9 M.", "Kon\u00e9 Moussa",
         "Kon\u00e9 D.", "Kon\u00e9", "van der Burgt X.M.", "Unknown", NA)
  k <- collector_key(x)
  expect_equal(k[1:6], rep("akeassi_l", 6))
  expect_equal(k[7:8], rep("jongkind_c", 2))
  expect_equal(k[9:10], rep("kone_m", 2))
  expect_equal(k[11], "kone_d")
  expect_true(is.na(k[12]))                     # Kone without initial: ambiguous (M. or D.)
  expect_equal(k[13], "burgt_x")
  expect_true(all(is.na(k[14:15])))
})

test_that("team members are all kept", {
  r <- parse_collectors(c("Jongkind & Abbiw", "Abbiw; Jongkind, C.C.H.", "Ake Assi, L., Ouattara, D."))
  expect_equal(r$coll, c("jongkind_c", "abbiw", "akeassi_l"))   # single initial C. for Jongkind in the data
  expect_true(grepl("abbiw", r$coll_all[1]) && grepl("jongkind", r$coll_all[2]))
  expect_equal(r$coll_all[3], "akeassi_l;ouattara_d")
})

test_that("parse_record_number extracts prefix and number", {
  r <- parse_record_number(c("1234", "1234b", "LAA 1234", "s.n.", NA, "1975-123", "2005/45", "1999-2"),
                           year = c(NA, NA, NA, NA, NA, 1975, NA, 1960))
  expect_equal(r$number, c(1234, 1234, 1234, NA, NA, 123, 45, 1999))
  expect_equal(r$prefix, c("", "", "LAA", "", "", "1975-", "2005-", ""))
  expect_equal(r$full[c(2, 6)], c("1234B", "1975123"))
})

test_that("delta_days accounts for date precision", {
  expect_equal(georefGN:::delta_days(0, 0L, 10, 0L), 10)
  expect_equal(georefGN:::delta_days(0, 1L, 5, 0L), 15)     # month precision: floor of 15 days
  expect_equal(georefGN:::delta_days(0, 3L, 5, 0L, 2, 0), 7) # inferred date with half-width
})

test_that("dates are inferred from collection numbers", {
  occ <- data.frame(recordedBy = "Aké-Assi, L.",
                    recordNumber = c("100", "150", "199", "260", "120", "250", "1000"),
                    eventDate = c("1960-01-10", "1960-01-10", "1960-01-12", "1960-01-13", NA, NA, NA),
                    stringsAsFactors = FALSE)
  occ <- prepare_occurrences(occ)
  r <- infer_dates_from_numbers(occ)
  d <- r$data
  expect_equal(d$date_source[5], "numero_encadre")
  expect_equal(as.character(d$date_deduite[5]), "1960-01-10")   # between 100 and 150, same day
  expect_equal(as.character(d$date_deduite[6]), "1960-01-12")   # first possible date after 199
  expect_true(is.na(d$date_deduite[7]))                          # beyond the last dated number
  expect_true(nrow(r$control) > 0)
})

test_that("a dated duplicate gives the exact date", {
  occ <- prepare_occurrences(data.frame(recordedBy = "Chevalier", recordNumber = c("55", "55"),
                                        eventDate = c("1910-05-02", NA), stringsAsFactors = FALSE))
  d <- infer_dates_from_numbers(occ)$data
  expect_equal(d$date_source[2], "numero_doublon")
  expect_equal(as.character(d$date_deduite[2]), "1910-05-02")
})

test_that("an incompatible known year rejects the inferred date", {
  occ <- prepare_occurrences(data.frame(recordedBy = "X Y", recordNumber = c("1", "50", "20"),
                                        eventDate = c("1960-01-10", "1960-01-11", NA), year = c(NA, NA, 1990),
                                        stringsAsFactors = FALSE))
  d <- infer_dates_from_numbers(occ)$data
  expect_true(is.na(d$date_deduite[3]))
})
