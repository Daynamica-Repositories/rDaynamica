#' Link Survey Answers to Calendar Items
#'
#' Adds activity and trip attributes to each calendar item survey answer, so
#' answers can be summarized by activity type, travel mode, trip purpose, day
#' of week, and so on. Each answer is linked to the item with its ID whose
#' start time is closest to the start time recorded with the survey (item IDs
#' are occasionally reused). Trip surveys describe a complete trip but are
#' attached to one of its legs, so `trip_mode` gives the trip's main mode (the
#' leg with the longest distance, then the longest duration).
#'
#' @param survey Calendar item survey table, raw or tidied with `tidy_calendar_item_survey()`.
#' @param items Calendar items table, such as `ucalitems_temporal_plot` from `filter_valid_days()`.
#' @return The tidy survey table with `type_decoded`, `subtype_decoded`,
#'   `trip_mode`, `trip_purpose`, `leg2tripid`, `start_date`, `dow`, and
#'   `IsWeekend` added where available. Answers for items not in `items` are dropped.
#' @export
link_survey_to_items <- function(survey, items) {
  tidy <- as_tidy_calendar_item_survey(survey)
  item_attrs <- survey_item_attributes(items)
  matched <- match_item_instance(tidy, item_attrs[, .(user_id, cal_item_id, item_instance, instance_start = item_start_timestamp)])

  linked <- merge(tidy, matched, by = c("user_id", "cal_item_id", "start_timestamp"))
  keep <- setdiff(names(item_attrs), c("item_start_timestamp", "item_distance", "item_duration"))
  linked <- merge(linked, item_attrs[, keep, with = FALSE], by = c("user_id", "cal_item_id", "item_instance"))
  linked[, item_instance := NULL]
  data.table::setorder(linked, user_id, cal_item_id, start_timestamp, question_id)
  linked[]
}

#' Summarize Survey Answers by Activity or Trip Attributes
#'
#' Summarizes calendar item survey answers for groups of activities or trips.
#' With `measure = "share"`, each row is a question, group, and answer option,
#' with the share of the group's responses that chose the option (shares for
#' select-all questions can add up to more than 1). With `measure = "mean"`,
#' each row is a question and group, with summary statistics of the numeric
#' answers (rating questions only).
#'
#' @param survey Calendar item survey table, raw or tidied.
#' @param items Calendar items table, such as `ucalitems_temporal_plot`.
#' @param by Column(s) to group by, from the columns added by
#'   `link_survey_to_items()` (e.g. "subtype_decoded", "trip_mode",
#'   "trip_purpose", "IsWeekend", "dow").
#' @param measure "share" or "mean".
#' @param questions Optional vector of question IDs to include.
#' @param min_responses Groups with fewer responses than this are dropped.
#' @return Data table of survey summaries.
#' @export
summarize_survey_by <- function(survey, items, by = "subtype_decoded", measure = c("share", "mean"),
                                questions = NULL, min_responses = 1) {
  measure <- match.arg(measure)
  linked <- link_survey_to_items(survey, items)
  missing <- setdiff(by, names(linked))
  if (length(missing) > 0) {
    cli::cli_abort("Can't group by {.field {missing}}; available columns are {.field {names(linked)}}.")
  }
  if (!is.null(questions)) linked <- linked[question_id %in% questions]
  linked <- linked[lengths(response_options) > 0]

  question_cols <- c("question_id", "question_text", "question_type")
  if (measure == "mean") {
    result <- linked[!is.na(response_value), .(
      responses = .N,
      mean = mean(response_value),
      sd = stats::sd(response_value),
      median = stats::median(response_value)
    ), by = c(question_cols, by)]
    result <- result[responses >= min_responses]
    data.table::setorderv(result, c("question_id", by))
  } else {
    responses <- linked[, .(responses = .N), by = c(question_cols, by)]
    options <- linked[, .(option = unlist(response_options)), by = c("question_id", by)][, .(n = .N), by = c("question_id", by, "option")]
    result <- merge(responses, options, by = c("question_id", by))
    result[, share := n / responses]
    data.table::setcolorder(result, c(question_cols, by, "option", "n", "responses", "share"))
    result <- result[responses >= min_responses]
    data.table::setorderv(result, c("question_id", by, "n"), order = c(rep(1L, length(by) + 1), -1L))
  }
  result[]
}

#' Survey Response Rates
#'
#' Calculates how often activities and trips were surveyed, and how long after
#' they started participants answered. Trips are counted as complete trips
#' (all legs with the same `leg2tripid`), since a trip's survey is attached to
#' one of its legs. Participants sometimes adjust an item's times after
#' answering, so a few response delays are slightly negative.
#'
#' @param items Calendar items table, such as `ucalitems_temporal_plot`.
#' @param survey Calendar item survey table, raw or tidied.
#' @param by Column(s) to group by, e.g. "type_decoded" or c("type_decoded", "user_id").
#' @param level "summary" for one row per group, or "unit" for one row per
#'   activity or trip with `surveyed` and `response_delay_hours`.
#' @return Data table. At the summary level: `units`, `surveyed`,
#'   `response_rate`, and the median and 90th percentile of `response_delay_hours`.
#' @export
survey_response_rates <- function(items, survey, by = "type_decoded", level = c("summary", "unit")) {
  level <- match.arg(level)
  tidy <- as_tidy_calendar_item_survey(survey)
  item_attrs <- survey_item_attributes(items)

  answered <- tidy[!is.na(response), .(response_timestamp = min(response_timestamp)), by = c("user_id", "cal_item_id", "start_timestamp")]
  matched <- match_item_instance(answered, item_attrs[, .(user_id, cal_item_id, item_instance, instance_start = item_start_timestamp)])
  answered <- merge(answered, matched, by = c("user_id", "cal_item_id", "start_timestamp"))[
    !is.na(item_instance), .(response_timestamp = min(response_timestamp)), by = c("user_id", "cal_item_id", "item_instance")]

  item_attrs <- merge(item_attrs[type_decoded %in% c("ACTIVITY", "TRIP")], answered,
                      by = c("user_id", "cal_item_id", "item_instance"), all.x = TRUE)

  unit_cols <- if ("leg2tripid" %in% names(item_attrs)) c("user_id", "leg2tripid") else c("user_id", "cal_item_id", "item_instance")
  missing <- setdiff(by, names(item_attrs))
  if (length(missing) > 0) {
    cli::cli_abort("Can't group by {.field {missing}}; available columns are {.field {names(item_attrs)}}.")
  }
  attr_cols <- setdiff(unique(c("type_decoded", by)), unit_cols)

  units <- item_attrs[, c(
    lapply(.SD, `[`, 1),
    list(surveyed = any(!is.na(response_timestamp)),
         unit_start = min(item_start_timestamp),
         first_response = suppressWarnings(min(response_timestamp, na.rm = TRUE)))
  ), by = unit_cols, .SDcols = attr_cols]
  units[, response_delay_hours := data.table::fifelse(surveyed, (first_response - unit_start) / 3.6e6, NA_real_)]
  units[, c("unit_start", "first_response") := NULL]

  if (level == "unit") return(units[])

  result <- units[, .(
    units = .N,
    surveyed = sum(surveyed),
    response_rate = mean(surveyed),
    median_delay_hours = stats::median(response_delay_hours, na.rm = TRUE),
    p90_delay_hours = stats::quantile(response_delay_hours, 0.9, na.rm = TRUE, names = FALSE)
  ), by = by]
  data.table::setorderv(result, by)
  result[]
}

#' Join End-of-Day (EMA) Survey Answers to Days
#'
#' Adds each day's EMA answers to the per-day summary, so self-reports can be
#' compared with recorded behavior (for example, reported online shopping
#' against shopping trips). If a participant submitted more than one EMA survey
#' on a day, the latest submission is used.
#'
#' @param ema_survey EMA survey table, raw or tidied with `tidy_ema_survey()`.
#' @param day_summary Per-day summary from `get_per_day_duration()` or `filter_valid_days()`.
#' @param keep_all_days If `TRUE`, days without an EMA survey are kept with missing answers.
#' @return `day_summary` with `ema_submissions` and one column per EMA question (`Q1`, `Q2`, ...).
#' @export
join_ema_to_days <- function(ema_survey, day_summary, keep_all_days = FALSE) {
  tidy <- data.table::as.data.table(ema_survey)
  if (!"response_options" %in% names(tidy)) tidy <- tidy_ema_survey(tidy)

  submissions <- tidy[, .(ema_submissions = data.table::uniqueN(timestamp)), by = c("user_id", "start_date")]
  latest <- tidy[, .SD[timestamp == max(timestamp)], by = c("user_id", "start_date")]
  wide <- merge(submissions, widen_survey(latest, c("user_id", "start_date")), by = c("user_id", "start_date"))

  days <- data.table::copy(data.table::as.data.table(day_summary))
  days[, start_date := as.Date(start_date)]
  merge(days, wide, by = c("user_id", "start_date"), all.x = keep_all_days)
}

# Survey Summary Internal Helper Functions

as_tidy_calendar_item_survey <- function(survey) {
  dt <- data.table::as.data.table(survey)
  if ("response_options" %in% names(dt)) dt else tidy_calendar_item_survey(dt)
}

# One row per item instance (segments of items split across midnight are
# combined), with the attributes used for grouping and the trip's main mode
survey_item_attributes <- function(items) {
  it <- data.table::as.data.table(items)
  missing <- setdiff(c("user_id", "cal_item_id", "type_decoded", "start_timestamp"), names(it))
  if (length(missing) > 0) {
    cli::cli_abort("{.arg items} is missing column{?s} {.field {missing}}.")
  }
  it <- item_row_instances(it)

  attr_cols <- intersect(c("leg2tripid", "type_decoded", "subtype_decoded", "trip_purpose", "start_date", "dow", "IsWeekend"),
                         names(it))
  item_attrs <- it[, c(lapply(.SD, `[`, 1),
                       list(item_start_timestamp = min(start_timestamp),
                            item_distance = if ("distance_after_split" %in% names(it)) sum(distance_after_split, na.rm = TRUE) else 0,
                            item_duration = if ("duration_after_split" %in% names(it)) sum(duration_after_split, na.rm = TRUE) else 0)),
                   by = c("user_id", "cal_item_id", "item_instance"), .SDcols = attr_cols]

  if (all(c("leg2tripid", "subtype_decoded") %in% names(item_attrs))) {
    main_mode <- item_attrs[type_decoded == "TRIP"][order(-item_distance, -item_duration)][
      , .(trip_mode = subtype_decoded[1]), by = c("user_id", "leg2tripid")]
    item_attrs <- merge(item_attrs, main_mode, by = c("user_id", "leg2tripid"), all.x = TRUE)
    item_attrs[type_decoded != "TRIP", trip_mode := NA_character_]
  }
  item_attrs[]
}
