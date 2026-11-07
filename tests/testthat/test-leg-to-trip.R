make_sample_legs <- function() {
  data.table::data.table(
    id = c(101L, 102L, 103L, 104L),
    user_id = c("u1", "u1", "u1", "u2"),
    start_date = as.Date(c("2023-01-01", "2023-01-01", "2023-01-01", "2023-01-01")),
    leg2tripid = c(1L, 1L, 2L, 1L),
    type_decoded = c("TRIP", "TRIP", "ACTIVITY", "TRIP"),
    subtype_decoded = c("WALK", "BUS", "HOME", "CAR - DRIVER"),
    trip_purpose = c("WORKPLACE", "WORKPLACE", "HOME", "SHOPPING ERRANDS"),
    distance_after_split = c(500, 3000, 0, 1500),
    duration_after_split = c(0.2, 0.1, 8.0, 0.5),
    start_timestamp = c(1000, 2000, 3000, 4000),
    end_timestamp = c(1500, 2500, 3500, 4500),
    start_dt = as.POSIXct(c("2023-01-01 08:00:00", "2023-01-01 08:15:00", "2023-01-01 09:00:00", "2023-01-01 10:00:00"), tz = "UTC"),
    end_dt = as.POSIXct(c("2023-01-01 08:12:00", "2023-01-01 08:21:00", "2023-01-01 17:00:00", "2023-01-01 10:30:00"), tz = "UTC"),
    end_date = as.Date(c("2023-01-01", "2023-01-01", "2023-01-01", "2023-01-01")),
    dow = c("Sunday", "Sunday", "Sunday", "Sunday"),
    IsWeekend = c("Weekend", "Weekend", "Weekend", "Weekend"),
    start_time = c("08:00:00", "08:15:00", "09:00:00", "10:00:00"),
    end_time = c("08:12:00", "08:21:00", "17:00:00", "10:30:00"),
    survey_not_null = c(FALSE, TRUE, FALSE, TRUE)
  )
}

test_that("leg2trip consolidates multi-leg trips using distance criterion", {
  dt <- make_sample_legs()
  res <- leg2trip(dt, criterion = "distance")

  # 3 consolidated trips/activities: u1-leg1, u1-leg2, u2-leg1
  expect_equal(nrow(res), 3)

  # For u1 leg2tripid 1: BUS had distance 3000 vs WALK 500
  u1_trip1 <- res[user_id == "u1" & leg2tripid == 1]
  expect_equal(u1_trip1$subtype_decoded, "BUS")
  expect_equal(u1_trip1$distance_after_split, 3500)
  expect_equal(round(u1_trip1$duration_after_split, 2), 0.3)
  expect_equal(u1_trip1$start_timestamp, 1000)
  expect_equal(u1_trip1$end_timestamp, 2500)
  expect_true(as.logical(u1_trip1$survey_not_null))
  expect_equal(u1_trip1$segment_subtype, "WALK_BUS")
  expect_equal(u1_trip1$segment_id, "101_102")
})

test_that("leg2trip consolidates multi-leg trips using duration criterion", {
  dt <- make_sample_legs()
  res <- leg2trip(dt, criterion = "duration")

  # For u1 leg2tripid 1: WALK had duration 0.2 vs BUS 0.1
  u1_trip1 <- res[user_id == "u1" & leg2tripid == 1]
  expect_equal(u1_trip1$subtype_decoded, "WALK")
  expect_equal(u1_trip1$distance_after_split, 3500)
  expect_equal(round(u1_trip1$duration_after_split, 2), 0.3)
  expect_equal(u1_trip1$segment_subtype, "WALK_BUS")
  expect_equal(u1_trip1$segment_id, "101_102")
})
