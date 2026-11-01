test_that("rename_userids anonymizes user IDs correctly across dictionary tables", {
  csv_dict <- list(
    ucalitems = data.table::data.table(user_id = c("u1", "u2", "u1"), id = 1:3),
    ema_survey = data.table::data.table(user_id = c("u2", "u1"), value = 10:11),
    meta = data.table::data.table(info = "no_user_id")
  )

  result <- rename_userids(csv_dict, seed = 42)

  expect_true("id_dictionary" %in% names(result))
  expect_equal(nrow(result$id_dictionary), 2)
  expect_setequal(result$id_dictionary$orig_id, c("u1", "u2"))

  id_map <- stats::setNames(result$id_dictionary$new_id, result$id_dictionary$orig_id)

  expect_equal(result$ucalitems$user_id, as.character(id_map[c("u1", "u2", "u1")]))
  expect_equal(result$ema_survey$user_id, as.character(id_map[c("u2", "u1")]))
  expect_equal(result$meta$info, "no_user_id")
})
