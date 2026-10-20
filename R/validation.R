#' Count Valid Days by Interaction Thresholds
#'
#' @param day_summary Data table output from `get_per_day_duration`.
#' @param numerator_col Column name to evaluate against threshold (e.g. 'with_subtype').
#' @param denominator_filter Expression string to filter active days.
#' @return Data frame summarizing total and valid days across thresholds.
#' @export
count_valid_per_days <- function(day_summary,
                                 numerator_col = 'with_subtype',
                                 denominator_filter = 'interact_with_app > 0') {

  day_summary_sub <- day_summary[eval(parse(text = denominator_filter))]

  threshold_list <- c(24, 20, 16, 12, 8) - 0.01

  count_days <- function(dt, day = NA) {
    total_days <- nrow(dt)
    if (!is.na(day)) {
      result <- data.table::data.table('Day of the Week' = 'Total', 'Total # of Days' = total_days)
    } else {
      result <- data.table::data.table('Day of the Week' = dt$dow[1], 'Total # of Days' = total_days)
    }

    for (threshold in threshold_list) {
      sub_days <- nrow(dt[eval(parse(text = paste0(numerator_col, '>=', threshold)))])
      if (total_days == 0) {
        sub_days_per <- NA
      } else {
        sub_days_per <- sub_days / total_days * 100
        result <- cbind(result, sprintf("%d (%.1f%%)", sub_days, sub_days_per))
      }
    }
    return(result)
  }

  dow_order <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')
  day_summary_sub[, dow := factor(dow, levels = dow_order)]

  summary_list <- suppressMessages(lapply(split(day_summary_sub, by = 'dow', sorted = TRUE), count_days))
  summary <- dplyr::bind_rows(summary_list)
  summary <- dplyr::bind_rows(summary, count_days(day_summary_sub, day = 'Total'))

  data.table::setnames(summary, c('Day of the Week', 'Total # of Days', sprintf("%.0f hours", threshold_list)))
  return(summary)
}

#' Query Valid Days from Dictionary Table
#'
#' @param csv_dict Dictionary list of data tables.
#' @param tb Name of table in `csv_dict` to filter.
#' @param query_text Expression string defining valid days.
#' @param csv_dict_ref Reference dictionary list.
#' @param tb_ref Reference table name (default 'day_summary').
#' @param cols Key columns to match (default c('user_id', 'start_date')).
#' @return Filtered data frame.
#' @export
query_valid_days_func <- function(csv_dict, tb,
                                  query_text,
                                  csv_dict_ref = csv_dict, tb_ref = 'day_summary',
                                  cols = c('user_id', 'start_date')) {

  valid_keys <- csv_dict_ref[[tb_ref]] %>%
    dplyr::filter(eval(parse(text = query_text))) %>%
    dplyr::select(dplyr::all_of(cols)) %>%
    unique()

  df <- dplyr::inner_join(csv_dict[[tb]], valid_keys, by = cols)
  return(df)
}

#' Filter Valid Days Across Dataset Dictionary
#'
#' @param csv_dict Dictionary list of data tables.
#' @param query_text Expression string defining valid days criteria.
#' @return Filtered dictionary list `csv_dict_sub`.
#' @export
filter_valid_days <- function(csv_dict, query_text) {
  csv_dict_sub <- list()

  csv_dict_sub[['day_summary']] <- csv_dict[['day_summary']] %>%
    dplyr::filter(eval(parse(text = query_text)))

  cli::cli_inform("# days before filtering: {nrow(csv_dict[['day_summary']])} | after filtering: {nrow(csv_dict_sub[['day_summary']])}")

  csv_dict_sub[['ucalitems_ljoin_ucisurvey_split']] <- data.table::copy(query_valid_days_func(csv_dict, 'ucalitems_ljoin_ucisurvey_split', query_text))
  csv_dict_sub[['ucalitems_ljoin_ucisurvey_split']][, IsWeekend := data.table::fifelse(weekdays(start_date) %in% c('Saturday', 'Sunday'), 'Weekend', 'Weekday')]

  cli::cli_inform("ucalitems_ljoin_ucisurvey_split items before filtering: {nrow(csv_dict[['ucalitems_ljoin_ucisurvey_split']])} | after: {nrow(csv_dict_sub[['ucalitems_ljoin_ucisurvey_split']])}")

  csv_dict_sub[['ucalitems_temporal_plot']] <- data.table::copy(csv_dict_sub[['ucalitems_ljoin_ucisurvey_split']])
  csv_dict_sub[['ucalitems_temporal_plot']][, `:=`(
    IsWeekend = data.table::fifelse(weekdays(start_date) %in% c('Saturday', 'Sunday'), 'Weekend', 'Weekday'),
    start_time = format(start_dt, format = "%H:%M:%S"),
    end_time = format(end_dt, format = "%H:%M:%S"),
    type_decoded = data.table::fifelse(type_decoded %in% c('DATA COLLECTION STARTED', 'INACC', 'OFF'), 'DEVICE OFF', type_decoded),
    subtype_decoded = data.table::fcase(
      subtype_decoded == 'ACTIVITY', 'OTHER ACTIVITIES',
      subtype_decoded == 'TRIP', 'OTHER TRIPS',
      subtype_decoded == 'WORK', 'WORKPLACE',
      type_decoded == 'ACTIVITY' & subtype_decoded == 'OTHER', 'OTHER ACTIVITIES',
      type_decoded == 'TRIP' & subtype_decoded == 'OTHER', 'OTHER TRIPS',
      rep_len(TRUE, nrow(csv_dict_sub[['ucalitems_temporal_plot']])), subtype_decoded
    ),
    trip_purpose = data.table::fcase(
      trip_purpose == 'ACTIVITY', 'OTHER ACTIVITIES',
      trip_purpose == 'TRIP', 'OTHER TRIPS',
      trip_purpose == 'WORK', 'WORKPLACE',
      trip_purpose == 'OTHER', 'OTHER ACTIVITIES',
      rep_len(TRUE, nrow(csv_dict_sub[['ucalitems_temporal_plot']])), trip_purpose
    )
  )]

  csv_dict_sub[['ucalitems_activity']] <- data.table::copy(query_valid_days_func(csv_dict, 'ucalitems_ljoin_ucisurvey_split', query_text)) %>%
    dplyr::filter(type_decoded == 'ACTIVITY')

  if (!is.null(csv_dict[['ema_survey']])) {
    csv_dict[['ema_survey']] <- data.table::copy(data.table::as.data.table(csv_dict[['ema_survey']]))
    if ('ema_survey_date' %in% names(csv_dict[['ema_survey']])) {
      csv_dict[['ema_survey']][, start_date := as.Date(ema_survey_date)]
    }
    csv_dict_sub[['ema_survey']] <- data.table::copy(query_valid_days_func(csv_dict, 'ema_survey', query_text))
    cli::cli_inform("ema_survey items before filtering: {nrow(csv_dict[['ema_survey']])} | after: {nrow(csv_dict_sub[['ema_survey']])}")
  }

  if (!is.null(csv_dict[['calendar_item_survey']])) {
    # Raw exports name these columns calendar_item_id / calendar_item_timestamp
    csv_dict[['calendar_item_survey']] <- data.table::copy(data.table::as.data.table(csv_dict[['calendar_item_survey']]))
    data.table::setnames(csv_dict[['calendar_item_survey']],
                         old = c("calendar_item_id", "calendar_item_timestamp"),
                         new = c("cal_item_id", "start_timestamp"),
                         skip_absent = TRUE)

    csv_dict_sub[['calendar_item_survey']] <- data.table::copy(query_valid_days_func(
      csv_dict, 'calendar_item_survey',
      query_text = '!is.na(id)',
      csv_dict_sub, 'ucalitems_ljoin_ucisurvey_split',
      cols = c('user_id', 'cal_item_id')
    ))
    cli::cli_inform("calendar_item_survey items before filtering: {nrow(csv_dict[['calendar_item_survey']])} | after: {nrow(csv_dict_sub[['calendar_item_survey']])}")
  }

  return(csv_dict_sub)
}
