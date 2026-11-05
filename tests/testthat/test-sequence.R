test_that("generate_sequence_data validates unit_length and subtype correctly", {
  invalid_dataset <- data.frame(
    user_id = "u1",
    start_date = as.Date("2023-01-01"),
    start_time = "08:00:00",
    end_time = "09:00:00",
    subtype = 0,
    subtype_decoded = "UNKNOWN"
  )

  expect_error(generate_sequence_data(invalid_dataset, unit_length = 7), "Please select a unit_length that divides evenly")
  expect_error(generate_sequence_data(invalid_dataset, unit_length = 5), "must be integer codes from 1 to 26")
})

test_that("generate_sequence_data creates expected long and wide sequences and id_key", {
  dataset <- data.frame(
    user_id = c("u1", "u1", "u2"),
    start_date = as.Date(c("2023-01-01", "2023-01-01", "2023-01-01")),
    start_time = c("00:00:00", "02:00:00", "00:00:00"),
    end_time = c("02:00:00", "04:00:00", "02:00:00"),
    subtype = c(1, 2, 3),
    subtype_decoded = c("HOME", "WORK", "SHOP"),
    stringsAsFactors = FALSE
  )

  res <- generate_sequence_data(dataset, unit_length = 60)

  expect_named(res, c("long_sequence_data", "wide_sequence_data", "id_key"))
  expect_equal(nrow(res$id_key), 3)
  expect_equal(res$id_key$letters_subtype, c("a", "b", "c"))

  wide <- res$wide_sequence_data
  expect_equal(nrow(wide), 2)
  expect_true("1" %in% names(wide))
  expect_true("3" %in% names(wide))

  u1_row <- wide[wide$user_id == "u1", ]
  expect_equal(as.character(u1_row[["1"]]), "a")
  expect_equal(as.character(u1_row[["3"]]), "b")

  u2_row <- wide[wide$user_id == "u2", ]
  expect_equal(as.character(u2_row[["1"]]), "c")
  expect_true(is.na(u2_row[["3"]]))
})

test_that("generate_sequence_data prioritizes longer duration on overlapping slots", {
  # Two items for u1 overlapping on slot loc 2 (00:05-00:10 with 5 min units):
  # Item 1: 00:00 to 00:30 (longer diff = 6) -> subtype 1 ('a')
  # Item 2: 00:05 to 00:10 (shorter diff = 1) -> subtype 2 ('b')
  # On slot loc 2, the longer item ('a') must win.
  dataset <- data.frame(
    user_id = c("u1", "u1"),
    start_date = as.Date(c("2023-01-01", "2023-01-01")),
    start_time = c("00:00:00", "00:05:00"),
    end_time = c("00:30:00", "00:10:00"),
    subtype = c(1, 2),
    subtype_decoded = c("HOME", "WORK"),
    stringsAsFactors = FALSE
  )

  res <- generate_sequence_data(dataset, unit_length = 5)
  wide <- res$wide_sequence_data
  expect_equal(as.character(wide[["2"]]), "a")
})
