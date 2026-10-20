#' Update Calendar Item Dataset Versions and Timestamps
#'
#' Ensures `subtype_decoded` and `type_decoded` columns exist in `ucalitems`, and converts string timestamps to numeric milliseconds if necessary.
#'
#' @param csv_dict Dictionary list of Daynamica tables.
#' @return Updated `csv_dict`.
#' @export
ucal_items_version_updater <- function(csv_dict) {
  ucal_items <- csv_dict$ucalitems

  if (!("subtype_decoded" %in% names(ucal_items))) {
    ucal_items$subtype_decoded <- ucal_items$subtype
  }
  if (!("type_decoded" %in% names(ucal_items))) {
    ucal_items$type_decoded <- ucal_items$type
  }

  if (!is.numeric(ucal_items$start_timestamp)) {
    if (length(strsplit(as.character(ucal_items$start_timestamp[1]), ":")[[1]]) == 2) {
      ucal_items$start_timestamp <- as.numeric(as.POSIXct(lubridate::mdy_hm(ucal_items$start_timestamp))) * 1000
      ucal_items$end_timestamp <- as.numeric(as.POSIXct(lubridate::mdy_hm(ucal_items$end_timestamp))) * 1000
    } else if (length(strsplit(as.character(ucal_items$start_timestamp[1]), ":")[[1]]) == 3) {
      ucal_items$start_timestamp <- as.numeric(as.POSIXct(lubridate::mdy_hms(ucal_items$start_timestamp))) * 1000
      ucal_items$end_timestamp <- as.numeric(as.POSIXct(lubridate::mdy_hms(ucal_items$end_timestamp))) * 1000
    }
  }

  csv_dict[["ucalitems"]] <- ucal_items
  return(csv_dict)
}

#' Assign Trip Purpose Based on Subsequent Activity
#'
#' @param ucalitems Data table of calendar items.
#' @return Updated `ucalitems` data table with `trip_purpose` column.
#' @export
assign_trip_purpose <- function(ucalitems) {
  ucalitems <- data.table::copy(ucalitems)
  data.table::setkey(ucalitems, user_id, start_timestamp)

  ucalitems[, type_decoded_pre := data.table::shift(type_decoded), by = .(user_id)]
  ucalitems[, type_decoded_next := data.table::shift(type_decoded, n = -1), by = .(user_id)]

  ucalitems[, flag := (type_decoded != 'TRIP' | type_decoded_pre != 'TRIP' | is.na(type_decoded_pre))]
  ucalitems[, leg2tripid := cumsum(flag)]

  ucalitems[, trip_purpose := data.table::fifelse(type_decoded == 'TRIP',
                                                   data.table::shift(subtype_decoded, n = -1),
                                                   subtype_decoded), by = .(user_id)]

  ucalitems[, trip_purpose := data.table::fifelse(type_decoded_next == 'TRIP' & type_decoded == 'TRIP',
                                                   data.table::last(trip_purpose),
                                                   trip_purpose), by = .(user_id, leg2tripid)]

  columns_to_remove <- c("type_decoded_pre", "type_decoded_next", "flag")
  ucalitems[, (columns_to_remove) := NULL]

  ucalitems[, trip_purpose := data.table::fifelse(is.na(trip_purpose), 'UNKNOWN', trip_purpose)]

  return(ucalitems)
}

#' Join Trip ID to Survey Table
#'
#' @param ucalitems_survey Survey data table.
#' @param ucalitems Calendar items data table.
#' @return Merged survey data table with `leg2tripid`.
#' @export
survey_join_id <- function(ucalitems_survey, ucalitems) {
  if ("calendar_item_id" %in% names(ucalitems_survey)) {
    data.table::setnames(ucalitems_survey,
                         old = c("calendar_item_id", "calendar_item_timestamp"),
                         new = c("cal_item_id", "start_timestamp"))
  }

  result <- merge(ucalitems_survey,
                  ucalitems[, c("user_id", "cal_item_id", "start_timestamp", "leg2tripid")],
                  by = c("user_id", "cal_item_id", "start_timestamp"),
                  all.x = TRUE)

  return(result)
}

#' Left Join Calendar Items Table with Survey Data
#'
#' @param ucalitems_survey Merged survey table with leg2tripid.
#' @param ucalitems Calendar items data table.
#' @return Combined data table.
#' @export
ucalitems_ljoin_ucisurvey <- function(ucalitems_survey, ucalitems) {
  ucalitems_survey_agg <- ucalitems_survey[!is.na(response)]
  ucalitems_survey_agg <- ucalitems_survey_agg[, .(valid_survey = .N),
                                                by = .(user_id, cal_item_id, start_timestamp)]
  ucalitems_survey_agg[, survey_not_null := TRUE]

  result <- merge(ucalitems,
                  ucalitems_survey_agg[, !"start_timestamp", with = FALSE],
                  by = c("user_id", "cal_item_id"),
                  all.x = TRUE)

  result[is.na(survey_not_null), survey_not_null := FALSE]
  result[, survey_not_null := max(survey_not_null), by = .(leg2tripid)]

  cli::cli_inform("ucalitems with survey: {table(result$survey_not_null)[2]} | without survey: {table(result$survey_not_null)[1]}")
  return(result)
}

#' Split Multiday Calendar Items Across Midnight / Day Start Boundaries
#'
#' @param ucalitems_ljoin_ucisurvey Combined calendar item and survey dataset.
#' @param local_timezone Timezone string (default 'US/Central').
#' @param day_start_hour Integer hour [0,6] defining day start.
#' @param min_time_stamp Minimum timestamp integer.
#' @return Data table with split records.
#' @export
split_ucalitems <- function(ucalitems_ljoin_ucisurvey,
                            local_timezone = 'US/Central',
                            day_start_hour = 0,
                            min_time_stamp = 0) {

  if (day_start_hour != round(day_start_hour) || day_start_hour < 0 || day_start_hour > 6) {
    cli::cli_abort("Please set day_start_hour as integer [0,6]!")
  }

  if (!is.numeric(ucalitems_ljoin_ucisurvey$start_timestamp)) {
    ucalitems_ljoin_ucisurvey[, `:=`(start_timestamp = as.numeric(as.POSIXct(lubridate::mdy_hm(start_timestamp))) * 1000,
                                     end_timestamp = as.numeric(as.POSIXct(lubridate::mdy_hm(end_timestamp))) * 1000)]
  }

  ucalitems_ljoin_ucisurvey[, `:=`(
    start_dt_ori = as.POSIXct(start_timestamp / 1000, origin = "1970-01-01", tz = "UTC"),
    end_dt_ori = as.POSIXct(end_timestamp / 1000, origin = "1970-01-01", tz = "UTC")
  )][, `:=`(
    start_dt = start_dt_ori - day_start_hour * 3600,
    end_dt = end_dt_ori - day_start_hour * 3600
  )][, `:=`(
    start_date = as.Date(start_dt, tz = local_timezone),
    end_date = as.Date(end_dt, tz = local_timezone)
  )][,
    days := as.numeric(difftime(end_date, start_date, units = "days")) + 1
  ]

  ucalitems_ljoin_ucisurvey <- ucalitems_ljoin_ucisurvey[days > 0][order(user_id, start_dt)][, `:=`(
    id = seq_len(.N),
    duration_before_split = as.numeric(difftime(end_dt, start_dt, units = "hours"))
  )]

  split_days <- data.table::rbindlist(lapply(split(ucalitems_ljoin_ucisurvey, by = 'id'), function(episode) {
    start_date_seq <- seq.Date(episode$start_date, episode$end_date, by = 'days')
    id <- rep(episode$id, episode$days)
    data.table::data.table(start_date = start_date_seq, id = id)
  }))

  split_days <- split_days[order(id, start_date)]

  result <- merge(split_days, ucalitems_ljoin_ucisurvey,
                  by = "id", all.x = TRUE, suffixes = c("", "_raw"))

  result[, start_dt := data.table::fifelse(
    start_date != start_date_raw,
    as.POSIXct(paste(start_date, "00:00:00")),
    start_dt
  )]

  result[, end_dt := data.table::fifelse(
    start_date != end_date,
    as.POSIXct(paste(start_date + 1, "00:00:00")),
    end_dt
  )]

  result[, `:=`(
    duration_after_split = as.numeric(difftime(end_dt, start_dt, units = "hours")),
    start_date = as.Date(start_dt, tz = local_timezone),
    end_date = as.Date(end_dt, tz = local_timezone),
    dow = weekdays(start_date)
  )]

  result[, distance_after_split := duration_after_split / duration_before_split * distance]
  result[, distance_after_split := data.table::fifelse(is.na(distance_after_split), 0, distance_after_split)]

  result <- result[, !names(result) %in% c("days", "start_date_raw"), with = FALSE]

  cli::cli_inform("# rows original: {nrow(ucalitems_ljoin_ucisurvey)} -> after splitting: {nrow(result)}")
  cli::cli_inform("# hours original: {round(sum(ucalitems_ljoin_ucisurvey$duration_before_split), 2)} -> after splitting: {round(sum(result$duration_after_split), 2)}")

  result[, `:=`(
    interact_with_app = (confirm_timestamp > min_time_stamp) | (edit_timestamp > min_time_stamp),
    interact_by_confirm = confirm_timestamp > min_time_stamp,
    interact_by_edit = edit_timestamp > min_time_stamp
  )]

  return(result)
}

#' Generate Daily Data Summary Metrics
#'
#' @param table Split calendar items data table.
#' @return Per-day duration and interaction metrics data table.
#' @export
get_per_day_duration <- function(table) {
  mask_dict <- list(
    total = !is.na(table$user_id),
    no_off = table$type_decoded %in% c('ACTIVITY', 'TRIP'),
    interact_with_app = table$interact_with_app,
    interact_by_confirm = table$interact_by_confirm,
    interact_by_edit = table$interact_by_edit,
    with_subtype = (!table$subtype_decoded %in% c('ACTIVITY', 'TRIP')) & table$type_decoded %in% c('ACTIVITY', 'TRIP'),
    with_survey = table$survey_not_null | table$subtype_decoded == 'HOME',
    trip_count = table$type_decoded == 'TRIP',
    trip_duration = table$type_decoded == 'TRIP',
    activity_count = table$type_decoded == 'ACTIVITY',
    activity_duration = table$type_decoded == 'ACTIVITY'
  )

  per_day <- data.table::rbindlist(mapply(function(ifvalid, key) {
    table_sub <- table[ifvalid]
    if (key %in% c('interact_with_app', 'interact_by_confirm', 'interact_by_edit', 'trip_count', 'activity_count')) {
      per_day_duration <- table_sub[, .(hours = .N), by = c('user_id', 'dow', 'start_date')]
    } else {
      per_day_duration <- table_sub[, .(hours = sum(duration_after_split)), by = c('user_id', 'dow', 'start_date')]
    }
    per_day_duration[, stat_type := key]
  }, mask_dict, names(mask_dict), SIMPLIFY = FALSE))

  data.table::setkey(per_day, user_id, dow, start_date)

  per_day <- data.table::dcast(per_day, user_id + dow + start_date ~ stat_type,
                                value.var = 'hours', fill = 0)

  per_day[, IsWeekend := data.table::fifelse(weekdays(start_date) %in% c('Saturday', 'Sunday'), 'Weekend', 'Weekday')]

  return(per_day)
}

#' Exclude Specific User IDs from Dictionary
#'
#' @param csv_dict Dictionary list of data tables.
#' @param exclude_id Keyword or vector of user IDs to exclude.
#' @return Filtered dictionary list.
#' @export
exclude_user <- function(csv_dict, exclude_id) {
  userids <- unique(csv_dict[['ucalitems']][, user_id])
  userids <- userids[!apply(sapply(exclude_id, function(id) grepl(id, userids)), 1, any)]

  csv_dict <- select_userids(csv_dict, userids)

  cli::cli_inform("Total number of users remaining: {length(userids)}")
  for (dict_name in names(csv_dict)) {
    cli::cli_inform("Table: {dict_name} | # rows: {nrow(csv_dict[[dict_name]])} | # cols: {ncol(csv_dict[[dict_name]])}")
  }

  return(csv_dict)
}

#' Select Specific User IDs Across Dictionary
#'
#' @param csv_dict Dictionary list of data tables.
#' @param userids Vector of user IDs to retain.
#' @return Subsetted dictionary list.
#' @export
select_userids <- function(csv_dict, userids) {
  csv_dict_sub <- list()
  for (key in names(csv_dict)) {
    if ("user_id" %in% names(csv_dict[[key]])) {
      csv_dict_sub[[key]] <- dplyr::filter(csv_dict[[key]], user_id %in% userids)
    } else {
      csv_dict_sub[[key]] <- csv_dict[[key]]
    }
  }
  return(csv_dict_sub)
}

#' Anonymize User IDs
#'
#' @param csv_dict Dictionary list of data tables.
#' @param seed Optional random seed for reproducible anonymization.
#' @return Updated `csv_dict` with anonymized IDs and an added `id_dictionary` mapping table.
#' @export
rename_userids <- function(csv_dict, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  study_size <- 10^c(1:6)

  unique_list_of_ids <- unique(c(csv_dict$calendar_item_survey$user_id,
                                 csv_dict$ema_survey$user_id,
                                 csv_dict$ucalitems$user_id))
  unique_list_of_ids <- unique_list_of_ids[!is.na(unique_list_of_ids)]

  max_val <- study_size[min(which(study_size > length(unique_list_of_ids)))]
  new_ids <- paste0("user_id_", sample(1:max_val, length(unique_list_of_ids), replace = FALSE))

  id_dict <- data.frame(orig_id = unique_list_of_ids, new_id = new_ids, stringsAsFactors = FALSE)

  for (item in seq_along(csv_dict)) {
    temp_data <- csv_dict[[item]]
    if (!("user_id" %in% names(temp_data))) {
      next
    }
    for (i in seq_len(nrow(id_dict))) {
      temp_data$user_id <- gsub(id_dict$orig_id[i], id_dict$new_id[i], temp_data$user_id)
    }
    csv_dict[[item]] <- temp_data
  }
  csv_dict[["id_dictionary"]] <- id_dict
  return(csv_dict)
}

#' Recategorize Calendar Item Subtypes
#'
#' @param csv_dict Dictionary list of data tables.
#' @param orig_cats Vector of original subtype category names.
#' @param new_cats Vector of new target category names.
#' @return Updated `csv_dict`.
#' @export
recategorize_calendar_items <- function(csv_dict, orig_cats, new_cats) {
  if (length(orig_cats) != length(new_cats)) {
    stop("Length of the original and new subtype category labels must be the same.")
  }

  tables_with_subtype_decoded_columns <- names(csv_dict)[sapply(csv_dict, function(x) "subtype_decoded" %in% names(x))]

  if (length(tables_with_subtype_decoded_columns) == 0) {
    stop("Please make sure that the subtype column is properly labeled as 'subtype_decoded'.")
  }
  cli::cli_inform("Modifying subtypes in tables: {paste(tables_with_subtype_decoded_columns, collapse = ', ')}")

  unique_levels <- c()
  for (tbl in tables_with_subtype_decoded_columns) {
    unique_levels <- c(unique_levels, unique(csv_dict[[tbl]]$subtype_decoded))
  }
  unique_levels <- unique(unique_levels)

  test_for_mislabels <- which(!(orig_cats %in% unique_levels))
  if (length(test_for_mislabels) > 0) {
    warning(paste0("Labels not found in dataset: ", paste(orig_cats[test_for_mislabels], collapse = ", ")))
  }

  subtype_cats <- data.frame(orig_labels = unique_levels, new_labels = unique_levels, new = FALSE, stringsAsFactors = FALSE)

  for (i in seq_along(orig_cats)) {
    idx <- which(subtype_cats$orig_labels == orig_cats[i])
    if (length(idx) > 0) {
      subtype_cats$new_labels[idx] <- new_cats[i]
      subtype_cats$new[idx] <- TRUE
    }
  }

  subtype_cats <- subtype_cats %>%
    dplyr::arrange(new_labels) %>%
    dplyr::group_by(new_labels) %>%
    dplyr::mutate(temp = dplyr::row_number(), test = ifelse(temp == 1, 1, 0)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(subtype = cumsum(test)) %>%
    dplyr::select(-temp, -test)

  for (j in seq_along(tables_with_subtype_decoded_columns)) {
    tbl_name <- tables_with_subtype_decoded_columns[j]
    csv_dict[[tbl_name]]$orig_subtype_decoded <- csv_dict[[tbl_name]]$subtype_decoded
    csv_dict[[tbl_name]]$orig_subtype <- csv_dict[[tbl_name]]$subtype

    for (i in seq_len(nrow(subtype_cats))) {
      if (subtype_cats$new[i]) {
        csv_dict[[tbl_name]]$subtype_decoded <- ifelse(
          csv_dict[[tbl_name]]$subtype_decoded == subtype_cats$orig_labels[i],
          subtype_cats$new_labels[i],
          csv_dict[[tbl_name]]$subtype_decoded
        )
      }
    }
    csv_dict[[tbl_name]]$subtype <- as.numeric(as.character(factor(
      csv_dict[[tbl_name]]$subtype_decoded,
      levels = subtype_cats$new_labels,
      labels = subtype_cats$subtype
    )))
  }

  return(csv_dict)
}
