# A small calendar item survey export with the quirks seen in real data:
# per-option timestamp lists, trailing spaces, inconsistent capitalization,
# commas inside option labels, repeated rating values, and a re-answered question.
make_calendar_item_survey <- function() {
  data.table::data.table(
    study_id = 1L,
    user_id = c("u1", "u1", "u1", "u1", "u2", "u2", "u2"),
    calendar_item_id = c(10L, 10L, 10L, 10L, 20L, 20L, 20L),
    question_id = c(1L, 2L, 5L, 5L, 8L, 5L, 7L),
    calendar_item_timestamp = c(1000, 1000, 1000, 1000, 2000, 2000, 2000),
    Q_Type = c(1L, 1L, 2L, 2L, 1L, 2L, 1L),
    response = c("Friends / acquaintances, Spouse / partner",
                 "Vigorous activity (e.g., running, swimming), None of the above",
                 "3",
                 "4, 4",
                 "Friends / Acquaintances",
                 "5",
                 "Tired , Stressful"),
    response_timestamp = c("1500, 1500", "1500, 1500", "1500", "1600, 1600", "2500", "2500", "2500, 2500"),
    question_text = c("Who was with you during this activity? ", "Physical activity?",
                      "How would you rate the overall experience of this activity?",
                      "How would you rate the overall experience of this activity?",
                      "Who was with you during this trip?",
                      "How would you rate the overall experience of this activity?",
                      "Negative emotions?")
  )
}

test_that("multi-select answers are split without breaking labels that contain commas", {
  tidy <- tidy_calendar_item_survey(make_calendar_item_survey())
  q2 <- tidy[question_id == 2]$response_options[[1]]
  expect_equal(q2, c("Vigorous activity (e.g., running, swimming)", "None of the above"))
})

test_that("option labels are trimmed and capitalization is made consistent", {
  tidy <- tidy_calendar_item_survey(make_calendar_item_survey())
  expect_equal(tidy[question_id == 7]$response_options[[1]], c("Tired", "Stressful"))
  friends <- unique(unlist(tidy[question_id %in% c(1, 8)]$response_options))
  expect_equal(sum(tolower(friends) == "friends / acquaintances"), 1)
})

test_that("repeated values collapse, ratings become numeric, and the latest answer is kept", {
  tidy <- tidy_calendar_item_survey(make_calendar_item_survey())
  q5 <- tidy[user_id == "u1" & question_id == 5]
  expect_equal(nrow(q5), 1)
  expect_equal(q5$response, "4")
  expect_equal(q5$response_value, 4)
  expect_equal(q5$response_timestamp, 1600)
  expect_true(all(is.na(tidy[question_type == "select_all"]$response_value)))
})

test_that("raw and survey_join_id() column names both work", {
  raw <- make_calendar_item_survey()
  renamed <- data.table::copy(raw)
  data.table::setnames(renamed, c("calendar_item_id", "calendar_item_timestamp"), c("cal_item_id", "start_timestamp"))
  expect_equal(tidy_calendar_item_survey(raw), tidy_calendar_item_survey(renamed))
  expect_true(all(c("cal_item_id", "start_timestamp") %in% names(tidy_calendar_item_survey(raw))))
})

test_that("missing columns give a clear error", {
  bad <- make_calendar_item_survey()[, !"response", with = FALSE]
  expect_error(tidy_calendar_item_survey(bad), "response")
})

test_that("process_surveys keeps every answer, with one wide row per item", {
  items <- data.table::data.table(user_id = c("u1", "u2", "u3"), cal_item_id = c(10L, 20L, 30L),
                                  type_decoded = c("ACTIVITY", "TRIP", "ACTIVITY"))
  out <- process_surveys(list(calendar_item_survey = make_calendar_item_survey(), ucalitems_temporal_plot = items))

  wide <- out$wide_calendar_item_survey_table
  expect_equal(nrow(wide), 2)
  expect_false(anyDuplicated(wide[, .(user_id, cal_item_id)]) > 0)
  expect_equal(wide[user_id == "u1"]$Q5, 4)

  joined <- out$ucal_items_temporal_plot_with_survey_data
  expect_equal(nrow(joined), 3)
  answered <- sum(!is.na(as.matrix(joined[, grep("^Q[0-9]+$", names(joined)), with = FALSE])))
  expect_equal(answered, nrow(out$calendar_item_survey_tidy))
  expect_true(is.na(joined[user_id == "u3"]$Q5))
})

test_that("process_surveys can run twice without duplicating tables", {
  items <- data.table::data.table(user_id = "u1", cal_item_id = 10L)
  once <- process_surveys(list(calendar_item_survey = make_calendar_item_survey(), ucalitems_temporal_plot = items))
  twice <- process_surveys(once)
  expect_equal(names(twice), names(once))
})

test_that("survey_codebook counts options and orders rating scales", {
  codebook <- survey_codebook(make_calendar_item_survey())
  q5 <- codebook[question_id == 5]
  expect_equal(q5$option, c("4", "5"))
  expect_equal(q5$question_responses, c(2L, 2L))
  expect_equal(q5$share, c(0.5, 0.5))
  expect_equal(unique(codebook[question_id == 1]$question_text), "Who was with you during this activity?")
})

test_that("EMA surveys tidy from both date-time and date-time text timestamps", {
  ema <- data.table::data.table(
    study_id = 1L, user_id = c("u1", "u1", "u1"), question_id = c(1L, 1L, 4L), ema_rule_id = 1L,
    ema_survey_date = as.Date(c("2022-11-12", "2022-11-13", "2022-11-12")),
    timestamp = c("2022-11-13 04:36:36", "2022-11-14 04:33:48", "2022-11-13 04:36:36"),
    Q_Type = c(2L, 2L, 1L), response = c("12+", "4", "No"),
    response_timestamp = c("2022-11-13 04:36:36", "2022-11-14 04:33:48", "2022-11-13 04:36:36"),
    question_text = c("Hours of work at home?", "Hours of work at home?", "Online purchase?")
  )
  tidy <- tidy_ema_survey(ema)
  expect_s3_class(tidy$response_timestamp, "POSIXct")
  expect_equal(tidy$start_date, as.Date(c("2022-11-12", "2022-11-12", "2022-11-13")))
  expect_true(is.na(tidy[response == "12+"]$response_value))
  expect_equal(tidy[response == "4"]$response_value, 4)

  ema_dt <- data.table::copy(ema)[, `:=`(timestamp = as.POSIXct(timestamp, tz = "UTC"),
                                        response_timestamp = as.POSIXct(response_timestamp, tz = "UTC"))]
  expect_equal(nrow(tidy_ema_survey(ema_dt)), 3)

  wide <- process_surveys(list(ema_survey = ema))$wide_ema_survey_table
  expect_equal(nrow(wide), 2)
  expect_type(wide$Q1, "character")
})
