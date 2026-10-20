# Items: u1 has an activity (10), a two-leg trip (11 walk + 12 bus, the bus leg
# longer) whose survey is attached to the walk leg, an unsurveyed activity
# (13), and item ID 10 reused for a different activity 40 days later.
hour <- 3.6e6
day <- 24 * hour
make_items <- function() {
  data.table::data.table(
    user_id = "u1",
    cal_item_id = c(10L, 11L, 12L, 13L, 10L),
    start_timestamp = c(0, 2 * hour, 2.2 * hour, 3 * hour, 40 * day),
    leg2tripid = c(1L, 2L, 2L, 3L, 4L),
    type_decoded = c("ACTIVITY", "TRIP", "TRIP", "ACTIVITY", "ACTIVITY"),
    subtype_decoded = c("HOME", "WALK", "BUS", "WORKPLACE", "SHOPPING ERRANDS"),
    trip_purpose = c("HOME", "WORKPLACE", "WORKPLACE", "WORKPLACE", "SHOPPING ERRANDS"),
    distance_after_split = c(0, 200, 5000, 0, 0),
    duration_after_split = c(2, 0.2, 0.5, 4, 1),
    IsWeekend = "Weekday"
  )
}

make_survey <- function() {
  data.table::data.table(
    user_id = "u1",
    calendar_item_id = c(10L, 10L, 11L, 10L, 10L),
    calendar_item_timestamp = c(0.1 * hour, 0.1 * hour, 2 * hour, 40 * day, 40 * day),
    question_id = c(5L, 1L, 14L, 5L, 1L),
    Q_Type = c(2L, 1L, 2L, 2L, 1L),
    response = c("4", "No one", "5", "2", "Friends"),
    response_timestamp = c(hour, hour, 3 * hour, 40 * day + hour, 40 * day + hour),
    question_text = c("Experience?", "Who was with you?", "How safe?", "Experience?", "Who was with you?")
  )
}

test_that("a reused item ID is kept as a separate item", {
  tidy <- tidy_calendar_item_survey(make_survey())
  expect_equal(nrow(tidy), 5)

  linked <- link_survey_to_items(make_survey(), make_items())
  expect_equal(linked[question_id == 5][order(start_timestamp)]$subtype_decoded, c("HOME", "SHOPPING ERRANDS"))
})

test_that("trip answers are linked to the trip's main mode", {
  linked <- link_survey_to_items(make_survey(), make_items())
  trip <- linked[question_id == 14]
  expect_equal(trip$subtype_decoded, "WALK")
  expect_equal(trip$trip_mode, "BUS")
  expect_true(all(is.na(linked[type_decoded == "ACTIVITY"]$trip_mode)))
})

test_that("summarize_survey_by gives means and option shares by group", {
  means <- summarize_survey_by(make_survey(), make_items(), by = "subtype_decoded", measure = "mean", questions = 5)
  expect_equal(means$subtype_decoded, c("HOME", "SHOPPING ERRANDS"))
  expect_equal(means$mean, c(4, 2))

  shares <- summarize_survey_by(make_survey(), make_items(), by = "IsWeekend", questions = 1)
  expect_equal(shares$responses, c(2L, 2L))
  expect_equal(shares$share, c(0.5, 0.5))

  expect_equal(nrow(summarize_survey_by(make_survey(), make_items(), measure = "mean", min_responses = 2)), 0)
  expect_error(summarize_survey_by(make_survey(), make_items(), by = "not_a_column"), "not_a_column")
})

test_that("response rates count complete trips and measure response delay", {
  rates <- survey_response_rates(make_items(), make_survey())
  expect_equal(rates$type_decoded, c("ACTIVITY", "TRIP"))
  expect_equal(rates$units, c(3L, 1L))
  expect_equal(rates$surveyed, c(2L, 1L))

  units <- survey_response_rates(make_items(), make_survey(), level = "unit")
  expect_equal(units[type_decoded == "TRIP"]$response_delay_hours, 1)
  expect_equal(sort(units[type_decoded == "ACTIVITY" & surveyed]$response_delay_hours), c(1, 1))
})

test_that("process_surveys keeps answers for reused item IDs on the right item", {
  out <- process_surveys(list(calendar_item_survey = make_survey(), ucalitems_temporal_plot = make_items()))
  expect_equal(nrow(out$wide_calendar_item_survey_table), 3)
  joined <- out$ucal_items_temporal_plot_with_survey_data
  expect_equal(nrow(joined), 5)
  expect_equal(joined[subtype_decoded == "HOME"]$Q5, 4)
  expect_equal(joined[subtype_decoded == "SHOPPING ERRANDS"]$Q5, 2)
  expect_true(is.na(joined[subtype_decoded == "WORKPLACE"]$Q5))
})

test_that("join_ema_to_days uses the latest submission per day", {
  ema <- data.table::data.table(
    user_id = "u1", question_id = 4L, Q_Type = 1L, question_text = "Online purchase?",
    ema_survey_date = as.Date(c("2022-11-12", "2022-11-12", "2022-11-13")),
    timestamp = as.POSIXct(c("2022-11-13 01:00", "2022-11-13 03:00", "2022-11-14 01:00"), tz = "UTC"),
    response = c("No", "Yes", "No"),
    response_timestamp = as.POSIXct(c("2022-11-13 01:00", "2022-11-13 03:00", "2022-11-14 01:00"), tz = "UTC")
  )
  days <- data.table::data.table(user_id = "u1", start_date = as.Date(c("2022-11-12", "2022-11-13", "2022-11-14")), trip_count = 1:3)

  joined <- join_ema_to_days(ema, days)
  expect_equal(nrow(joined), 2)
  expect_equal(joined$Q4, c("Yes", "No"))
  expect_equal(joined$ema_submissions, c(2L, 1L))
  expect_equal(nrow(join_ema_to_days(ema, days, keep_all_days = TRUE)), 3)
})

test_that("survey_codebook orders top-coded numeric scales numerically", {
  ema <- data.table::data.table(
    user_id = paste0("u", 1:5), question_id = 1L, Q_Type = 2L, question_text = "Hours?",
    ema_survey_date = as.Date("2022-11-12"), timestamp = as.POSIXct("2022-11-13 01:00", tz = "UTC"),
    response = c("12+", "2", "10", "2", "12"), response_timestamp = as.POSIXct("2022-11-13 01:00", tz = "UTC")
  )
  expect_equal(survey_codebook(ema)$option, c("2", "10", "12", "12+"))
})

test_that("run_survey_report needs survey data", {
  expect_error(run_survey_report(list(ucalitems_temporal_plot = make_items()), output_dir = tempfile()), "survey")
})
