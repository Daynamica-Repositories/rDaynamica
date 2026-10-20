#' Tidy Calendar Item Survey Responses
#'
#' Cleans the raw calendar item survey export into one row per surveyed
#' calendar item and question. Calendar item IDs are occasionally reused for a
#' later item; answers whose item start times are more than 3 days apart are
#' treated as belonging to different items. Multi-select answers are split into a list of
#' options (commas inside parentheses are kept as part of an option label),
#' option labels are trimmed and given consistent capitalization, repeated
#' values such as "2, 2" are collapsed, single-choice answers that are numbers
#' are converted to numeric, and the submission time is parsed from the
#' export's comma-separated timestamp lists. If a question was answered more
#' than once for the same item, the latest answer is kept.
#'
#' @param calendar_item_survey Calendar item survey table, either as exported
#'   (`calendar_item_id`, `calendar_item_timestamp`) or after `survey_join_id()`
#'   (`cal_item_id`, `start_timestamp`).
#' @return Data table with one row per user, calendar item, and question:
#'   `user_id`, `cal_item_id`, `start_timestamp`, `question_id`, `question_text`,
#'   `Q_Type`, `question_type` ("select_all" or "single"), `response` (cleaned
#'   text), `response_options` (list of selected options), `response_value`
#'   (numeric for single-choice numeric answers, otherwise `NA`; top-coded
#'   answers such as "12+" are `NA`, so check `response` for those), and
#'   `response_timestamp` (milliseconds since the epoch), plus `study_id` when present.
#' @export
tidy_calendar_item_survey <- function(calendar_item_survey) {
  dt <- data.table::copy(data.table::as.data.table(calendar_item_survey))
  data.table::setnames(dt,
                       old = c("calendar_item_id", "calendar_item_timestamp"),
                       new = c("cal_item_id", "start_timestamp"),
                       skip_absent = TRUE)
  check_survey_columns(dt, c("user_id", "cal_item_id", "start_timestamp"), "calendar_item_survey")

  dt <- tidy_survey_core(dt, id_cols = c("study_id", "user_id", "cal_item_id", "start_timestamp"))

  # Item IDs are occasionally reused for a later item, so only answers for the
  # same item instance count as repeats
  add_item_instance(dt, "start_timestamp")
  data.table::setorder(dt, user_id, cal_item_id, item_instance, question_id, -response_timestamp)
  dt <- unique(dt, by = c("user_id", "cal_item_id", "item_instance", "question_id"))
  dt[, item_instance := NULL]
  data.table::setorder(dt, user_id, cal_item_id, start_timestamp, question_id)
  dt
}

#' Tidy EMA Survey Responses
#'
#' Cleans the raw end-of-day (EMA) survey export into one row per survey
#' submission and question, using the same cleaning as
#' `tidy_calendar_item_survey()`. Participants can submit more than one EMA
#' survey on the same day; each submission is kept and identified by `timestamp`.
#'
#' @param ema_survey EMA survey table, as exported or after `filter_valid_days()`.
#' @return Data table with one row per user, submission, and question:
#'   `user_id`, `start_date`, `timestamp`, `question_id`, `question_text`,
#'   `Q_Type`, `question_type`, `response`, `response_options`,
#'   `response_value`, and `response_timestamp`, plus `study_id` when present.
#' @export
tidy_ema_survey <- function(ema_survey) {
  dt <- data.table::copy(data.table::as.data.table(ema_survey))
  if (!"start_date" %in% names(dt) && "ema_survey_date" %in% names(dt)) {
    dt[, start_date := as.Date(ema_survey_date)]
  }
  check_survey_columns(dt, c("user_id", "start_date", "timestamp"), "ema_survey")

  dt <- tidy_survey_core(dt, id_cols = c("study_id", "user_id", "start_date", "timestamp"))

  data.table::setorder(dt, user_id, timestamp, question_id, -response_timestamp)
  dt <- unique(dt, by = c("user_id", "timestamp", "question_id"))
  data.table::setorder(dt, user_id, timestamp, question_id)
  dt
}

#' Survey Codebook
#'
#' Summarizes each survey question and the answer options participants chose.
#' For select-all questions, `share` is the share of responses that selected
#' the option, so shares can add up to more than 1.
#'
#' @param survey A calendar item or EMA survey table, either raw or tidied with
#'   `tidy_calendar_item_survey()` / `tidy_ema_survey()`.
#' @return Data table with one row per question and option: `question_id`,
#'   `question_text`, `question_type`, `option`, `n` (responses selecting the
#'   option), `share`, `question_responses` (responses to the question), and
#'   `participants` (participants who answered the question).
#' @export
survey_codebook <- function(survey) {
  dt <- data.table::as.data.table(survey)
  if (!"response_options" %in% names(dt)) {
    dt <- if (any(c("cal_item_id", "calendar_item_id") %in% names(dt))) {
      tidy_calendar_item_survey(dt)
    } else {
      tidy_ema_survey(dt)
    }
  }

  answered <- dt[lengths(response_options) > 0]
  questions <- answered[, .(question_text = question_text[1],
                            question_type = question_type[1],
                            question_responses = .N,
                            participants = data.table::uniqueN(user_id)),
                        by = question_id]

  options <- answered[, .(option = unlist(response_options)), by = question_id][, .(n = .N), by = .(question_id, option)]
  codebook <- merge(questions, options, by = "question_id")
  codebook[, share := n / question_responses]

  # Numeric scales (including top-coded answers like "12+") read in order;
  # other options from most to least common
  codebook[, numeric_option := suppressWarnings(as.numeric(sub("^\\s*(-?[0-9]+(\\.[0-9]+)?).*$", "\\1", option)))]
  codebook[, sort_key := if (all(!is.na(numeric_option))) numeric_option else -as.numeric(n), by = question_id]
  data.table::setorder(codebook, question_id, sort_key, option)

  codebook[, .(question_id, question_text, question_type, option, n, share, question_responses, participants)]
}

#' Process In-App Survey Data into Wide Format
#'
#' Builds tidy and wide versions of the calendar item and EMA survey tables and
#' joins calendar item survey answers to `ucalitems_temporal_plot`. The wide
#' calendar item table has one row per surveyed item, with one column per
#' question (`Q1`, `Q2`, ...); single-choice questions whose answers are all
#' numbers are numeric columns.
#'
#' @param dict_tables Dictionary list of data tables.
#' @return Updated `dict_tables` with formatted wide survey tables and dictionaries.
#' @export
process_surveys <- function(dict_tables) {
  if (!is.null(dict_tables[['calendar_item_survey']])) {
    tidy <- tidy_calendar_item_survey(dict_tables$calendar_item_survey)

    calendar_item_question_dict <- survey_question_dict(tidy)

    # One wide row per surveyed item instance
    tidy_instances <- data.table::copy(tidy)
    add_item_instance(tidy_instances, "start_timestamp")
    item_cols <- intersect(c("study_id", "user_id", "cal_item_id", "item_instance"), names(tidy_instances))
    item_times <- tidy_instances[, .(survey_timestamp = min(start_timestamp),
                                     survey_response_timestamp = max(response_timestamp)),
                                 by = c("user_id", "cal_item_id", "item_instance")]
    wide <- merge(item_times, widen_survey(tidy_instances, item_cols),
                  by = c("user_id", "cal_item_id", "item_instance"))

    ucal_items_temporal_plot_with_survey_data <- join_wide_survey_to_items(
      dict_tables$ucalitems_temporal_plot,
      wide[, setdiff(names(wide), "study_id"), with = FALSE]
    )
    wide[, item_instance := NULL]
    wide_calendar_item_survey_table <- wide

    dict_tables[c("calendar_item_survey_tidy",
                  "ucal_items_temporal_plot_with_survey_data",
                  "wide_calendar_item_survey_table",
                  "calendar_item_question_dict")] <- list(
      tidy,
      ucal_items_temporal_plot_with_survey_data,
      wide_calendar_item_survey_table,
      calendar_item_question_dict
    )
  } else {
    cli::cli_inform("Skipping Calendar Item Surveys because no 'calendar_item_survey' table was found.")
  }

  if (!is.null(dict_tables[['ema_survey']])) {
    tidy <- tidy_ema_survey(dict_tables$ema_survey)

    submission_cols <- intersect(c("study_id", "user_id", "start_date", "timestamp"), names(tidy))
    submission_times <- tidy[, .(response_timestamp = max(response_timestamp)), by = c("user_id", "timestamp")]
    wide_ema_survey_table <- merge(
      submission_times,
      widen_survey(tidy, submission_cols),
      by = c("user_id", "timestamp")
    )
    data.table::setcolorder(wide_ema_survey_table, c(submission_cols, "response_timestamp"))

    dict_tables[c("ema_survey_tidy", "wide_ema_survey_table", "ema_survey_dict")] <- list(
      tidy,
      wide_ema_survey_table,
      survey_question_dict(tidy)
    )
  } else {
    cli::cli_inform("Skipping EMA surveys because no 'ema_survey' table was found.")
  }

  return(dict_tables)
}

# Survey Internal Helper Functions

check_survey_columns <- function(dt, id_cols, table_name) {
  required <- c(id_cols, "question_id", "question_text", "Q_Type", "response", "response_timestamp")
  missing <- setdiff(required, names(dt))
  if (length(missing) > 0) {
    cli::cli_abort("{.arg {table_name}} is missing column{?s} {.field {missing}}.")
  }
}

# Cleaning shared by calendar item and EMA surveys
tidy_survey_core <- function(dt, id_cols) {
  id_cols <- intersect(id_cols, names(dt))
  dt <- dt[, c(id_cols, "question_id", "question_text", "Q_Type", "response", "response_timestamp"), with = FALSE]

  dt[, question_text := gsub("\\s+", " ", trimws(question_text))]
  dt[, question_type := data.table::fifelse(Q_Type == 2, "single", "select_all")]

  dt[, response_options := harmonize_option_case(split_survey_options(response))]
  dt[, response := vapply(response_options, function(x) if (length(x)) paste(x, collapse = ", ") else NA_character_, character(1))]
  dt[, response_value := data.table::fifelse(
    question_type == "single" & lengths(response_options) == 1,
    suppressWarnings(as.numeric(response)),
    NA_real_
  )]
  dt[, response_timestamp := first_survey_timestamp(response_timestamp)]
  dt
}

# Split answers into options. Commas inside parentheses belong to the option
# label, e.g. "Vigorous activity (e.g. running, swimming)".
split_survey_options <- function(response) {
  parts <- strsplit(as.character(response), ",(?![^()]*\\))", perl = TRUE)
  lapply(parts, function(x) {
    x <- gsub("\\s+", " ", trimws(x))
    unique(x[!is.na(x) & x != ""])
  })
}

# Use the most common capitalization of each option everywhere, so that e.g.
# "Friends / acquaintances" and "Friends / Acquaintances" are one option
harmonize_option_case <- function(options) {
  all_options <- unlist(options)
  if (length(all_options) == 0) return(options)

  counts <- data.table::data.table(option = all_options)[, .N, by = option]
  counts[, key := tolower(option)]
  data.table::setorder(counts, key, -N)
  canonical <- counts[!duplicated(key)]
  lookup <- stats::setNames(canonical$option, canonical$key)

  lapply(options, function(x) unique(unname(lookup[tolower(x)])))
}

# Exports store one timestamp per selected option ("1668314407699, 1668314407699");
# they are identical, so keep the first. Millisecond values stay numeric;
# date-times (or date-time text, as read by path2dict()) become POSIXct.
first_survey_timestamp <- function(x) {
  if (inherits(x, "POSIXt")) return(x)
  first <- trimws(sub(",.*$", "", as.character(x)))
  millis <- suppressWarnings(as.numeric(first))
  if (all(is.na(first) | !is.na(millis))) return(millis)
  as.POSIXct(first, tz = "UTC", tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%d %H:%M", "%Y-%m-%d"))
}

# Calendar item IDs are occasionally reused for a later item. Rows of the same
# item, including the segments of an item split across midnight, are never
# more than this far apart.
item_instance_gap_ms <- 72 * 3.6e6

# Number the distinct items sharing a user and item ID, by time (modifies dt)
add_item_instance <- function(dt, time_col) {
  data.table::setorderv(dt, c("user_id", "cal_item_id", time_col))
  dt[, item_instance := cumsum(c(TRUE, diff(get(time_col)) > item_instance_gap_ms)), by = c("user_id", "cal_item_id")]
  invisible(dt)
}

# For each key (user_id, cal_item_id, start_timestamp from the survey), the
# item instance with the nearest start time
match_item_instance <- function(keys, instances) {
  inst <- data.table::copy(instances)[, .(user_id, cal_item_id, instance = item_instance, match_time = instance_start)]
  k <- unique(data.table::copy(keys)[, .(user_id, cal_item_id, start_timestamp)])[, match_time := start_timestamp]
  matched <- inst[k, on = c("user_id", "cal_item_id", "match_time"), roll = "nearest"]
  matched[, .(user_id, cal_item_id, start_timestamp, item_instance = instance)]
}

# Item instances in a calendar items table, with each row's instance
item_row_instances <- function(items) {
  it <- data.table::copy(data.table::as.data.table(items))
  it[, .item_row := .I]
  add_item_instance(it, "start_timestamp")
  it[, instance_start := min(start_timestamp), by = c("user_id", "cal_item_id", "item_instance")]
  it
}

# Join one-row-per-item survey answers to every row of a calendar items table
join_wide_survey_to_items <- function(items, wide) {
  items_dt <- data.table::as.data.table(items)
  if (!"start_timestamp" %in% names(items_dt)) {
    wide <- wide[, setdiff(names(wide), "item_instance"), with = FALSE]
    return(merge(items_dt, wide, by = c("user_id", "cal_item_id"), all.x = TRUE, sort = FALSE))
  }

  it <- item_row_instances(items_dt)
  instances <- unique(it[, .(user_id, cal_item_id, item_instance, instance_start)])
  matched <- match_item_instance(wide[, .(user_id, cal_item_id, start_timestamp = survey_timestamp)], instances)
  wide <- merge(wide[, setdiff(names(wide), "item_instance"), with = FALSE],
                matched, by.x = c("user_id", "cal_item_id", "survey_timestamp"),
                by.y = c("user_id", "cal_item_id", "start_timestamp"))
  # If two surveyed instances matched the same item, keep the closer one
  wide <- merge(wide[!is.na(item_instance)], instances, by = c("user_id", "cal_item_id", "item_instance"))
  wide[, .distance := abs(survey_timestamp - instance_start)]
  data.table::setorder(wide, user_id, cal_item_id, item_instance, .distance)
  wide <- unique(wide, by = c("user_id", "cal_item_id", "item_instance"))
  wide[, c(".distance", "instance_start") := NULL]

  joined <- merge(it, wide, by = c("user_id", "cal_item_id", "item_instance"), all.x = TRUE)
  data.table::setorder(joined, .item_row)
  joined[, c(".item_row", "item_instance", "instance_start") := NULL]
  joined[]
}

survey_question_dict <- function(tidy) {
  dict <- tidy[, .(question_text = question_text[1], Q_Type = Q_Type[1]), by = question_id]
  dict[, question_id := paste0("Q", question_id)]
  dict[]
}

# One column per question, in question order. Single-choice questions with
# only numeric answers become numeric columns.
widen_survey <- function(tidy, id_cols) {
  wide <- data.table::dcast(
    tidy,
    stats::as.formula(paste(paste(id_cols, collapse = " + "), "~ question_id")),
    value.var = "response"
  )
  question_ids <- sort(unique(tidy$question_id))
  data.table::setnames(wide, as.character(question_ids), paste0("Q", question_ids))

  for (q in question_ids) {
    col <- paste0("Q", q)
    is_single <- all(tidy[question_id == q, question_type] == "single")
    values <- wide[[col]][!is.na(wide[[col]])]
    if (is_single && length(values) > 0 && !anyNA(suppressWarnings(as.numeric(values)))) {
      data.table::set(wide, j = col, value = as.numeric(wide[[col]]))
    }
  }
  wide
}
