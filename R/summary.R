#' Default Activity Color Map for Summary Statistics
#' @export
activity_color_discrete_map <- c(
  'HOME' = '#f6e8c3',
  'WORKPLACE' = '#bebada',
  'EDUCATION' = '#ccebc5',
  'FOOD & MEAL' = '#b3de69',
  'MEDICAL & FITNESS' = '#80b1d3',
  'FUN & LEISURE' = '#fdb462',
  'COMMUNITY & CULTURAL' = '#ffed6f',
  'RELIGIOUS & SPIRITUAL' = '#ffffb3',
  'CARE GIVING' = '#fb8072',
  'SHOPPING ERRANDS' = '#bc80bd',
  'CIVIL ERRANDS' = '#e31a1c',
  'OTHER ACTIVITIES' = '#d9d9d9',
  'TRIP' = '#b15928',
  'DEVICE OFF' = '#f0f0f0'
)

#' Default Trip Color Map for Summary Statistics
#' @export
trip_color_discrete_map <- c(
  'CAR - DRIVER' = '#fdbf6f',
  'CAR - PASSENGER' = '#ff7f00',
  'VEHICLE' = '#b15928',
  'TAXI/UBER/LYFT' = '#fb9a99',
  'RAIL' = '#1f78b4',
  'BUS' = '#a6cee3',
  'BIKE' = '#33a02c',
  'WALK' = '#b2df8a',
  'WAIT' = '#6a3d9a',
  'OTHER TRIPS' = '#d9d9d9'
)

#' Compute Dataset Overview Statistics
#'
#' @param csv_dict Dictionary list of data tables.
#' @param stat_group_cols Grouping columns vector (e.g., c('IsWeekend', 'Statistics')).
#' @return Data table summarizing median, mean, SD, min, and max across metrics.
#' @export
overview_statistics <- function(csv_dict,
                                stat_group_cols = c('IsWeekend', 'Statistics')) {
  per_day <- list()
  group_cols <- c('user_id', 'start_date')

  valid_duration <- csv_dict$day_summary[, .(user_id, start_date, value = no_off * 60)]
  valid_duration$Statistics <- '1_Recorded Data per Day (Minutes)'
  per_day[[length(per_day) + 1]] <- valid_duration

  if (!is.null(csv_dict[["convex_hull"]])) {
    agg_space_list <- list(
      c('convex_hull', 'area_mile', '8_Convex Hull Area (Square Miles)'),
      c('sde_ellipse', 'area_mile', '90_Ellipse Area (Square Miles)'),
      c('sde_ellipse', 'sx_mile', '91_Ellipse semi-major Axis (Miles)'),
      c('sde_ellipse', 'sy_mile', '92_Ellipse semi-minor Axis (Miles)')
    )
    for (item in agg_space_list) {
      if (item[1] %in% names(csv_dict)) {
        activity_space <- csv_dict[[item[1]]][, c('user_id', 'start_date', item[2]), with = FALSE]
        data.table::setnames(activity_space, item[2], 'value')
        activity_space$Statistics <- item[3]
        per_day[[length(per_day) + 1]] <- activity_space
      }
    }
  }

  agg_list <- list(
    c('ACTIVITY', 'duration_after_split', 'sum', 60, '2_Total Activity Duration per Day (Minutes)', 'ucalitems_ljoin_ucisurvey_split'),
    c('TRIP', 'duration_after_split', 'sum', 60, '3_Total Trip Duration per Day (Minutes)', 'ucalitems_ljoin_ucisurvey_split'),
    c('TRIP', 'distance_after_split', 'sum', 0.000621371, '5_Total Trip Distance per Day (Miles)', 'ucalitems_ljoin_ucisurvey_split'),
    c('ACTIVITY', 'id', 'count', 1, '6_Activity Count per Day', 'ucalitems_ljoin_ucisurvey_split'),
    c('TRIP', 'id', 'count', 1, '70_Trip (Segment) Count per Day', 'ucalitems_ljoin_ucisurvey_split'),
    c('TRIP', 'id', 'count', 1, '71_Trip (Complete) Count per Day', 'leg2trip')
  )

  for (item in agg_list) {
    types <- item[1]
    tb <- item[length(item)]
    if (tb %in% names(csv_dict)) {
      df_sub <- csv_dict[[tb]] %>% dplyr::filter(type_decoded %in% types)
      if (nrow(df_sub) > 0) {
        per_day_item <- df_sub[, .(value = ifelse(item[3] == 'sum', sum(get(item[2])), .N)), by = group_cols]
        per_day_item[, value := value * as.numeric(item[4])]
        per_day_item <- merge(per_day_item, valid_duration[, .(user_id, start_date)], by = group_cols, all = TRUE)
        per_day_item$value[is.na(per_day_item$value)] <- 0
        per_day_item$Statistics <- item[[5]]
        per_day[[length(per_day) + 1]] <- per_day_item
      }
    }
  }

  per_day_df <- data.table::rbindlist(per_day, fill = TRUE)
  per_day_df$start_date <- as.Date(per_day_df$start_date)
  per_day_df$dow <- weekdays(per_day_df$start_date)
  per_day_df$IsWeekend <- (per_day_df$dow == 'Saturday') | (per_day_df$dow == 'Sunday')

  result <- per_day_df[, .(
    Median = stats::median(value, na.rm = TRUE),
    Mean = mean(value, na.rm = TRUE),
    SD = stats::sd(value, na.rm = TRUE),
    Min = min(value, na.rm = TRUE),
    Max = max(value, na.rm = TRUE)
  ), by = stat_group_cols]

  data.table::setorder(result, Statistics)
  result$Statistics <- gsub(".*_", "", result$Statistics)

  return(result)
}

#' Count Valid Days
#'
#' @param day_summary Per day duration data table.
#' @param trip_num Minimum trip count filter.
#' @return Named list of valid day counts.
#' @export
get_valid_days <- function(day_summary, trip_num = -1) {
  day_summary <- day_summary[trip_count > trip_num, ]
  valid_days_count <- list(
    'All Days' = nrow(day_summary),
    'Weekend' = nrow(subset(day_summary, IsWeekend == "Weekend")),
    'Weekday' = nrow(subset(day_summary, IsWeekend == "Weekday"))
  )
  return(valid_days_count)
}

#' Get Summary Statistics Table for Activities/Trips
#'
#' @param df Input data table.
#' @param mytype 'ACTIVITY' or 'TRIP'.
#' @param valid_days_count Named list from `get_valid_days`.
#' @param by_field Subtype grouping column name.
#' @return Data table with mean daily values.
#' @export
get_summary <- function(df, mytype, valid_days_count, by_field) {
  unit_convert <- 1609.34
  df <- df %>% dplyr::filter(type_decoded == mytype)

  if (mytype == 'ACTIVITY') {
    df[, duration_after_split := duration_after_split]
  } else {
    df[, duration_after_split := duration_after_split * 60]
  }

  df[, distance_after_split := distance_after_split / unit_convert]

  col_dict_final <- list(
    'ACTIVITY' = data.table::data.table(old = c('id', 'duration_after_split'), new = c('Activity Counts', 'Activity Duration in Hours')),
    'TRIP' = data.table::data.table(old = c('id', 'duration_after_split', 'distance_after_split'), new = c('Trip Counts', 'Trip Duration in Minutes', 'Trip Distance in Miles'))
  )

  agg_dict <- list(
    'ACTIVITY' = list(id = function(x) length(x), duration_after_split = function(x) sum(x)),
    'TRIP' = list(id = function(x) length(x), duration_after_split = function(x) sum(x), distance_after_split = function(x) sum(x))
  )

  cols <- names(agg_dict[[mytype]])

  for (col in cols) {
    fun <- agg_dict[[mytype]][[col]]
    df[, (col) := fun(get(col)), by = c('user_id', 'IsWeekend', 'start_date', by_field)]
  }

  df <- unique(df[, c(cols, 'user_id', 'IsWeekend', 'start_date', by_field), with = FALSE])

  result <- list()
  for (key in names(valid_days_count)) {
    df_sub <- data.table::copy(df)
    if (key != 'All Days') {
      df_sub <- df_sub[IsWeekend == key]
    }
    val <- valid_days_count[[key]]
    for (col in cols) {
      df_sub[, (col) := as.numeric(get(col))]
      df_sub[, (col) := sum(get(col) / val), by = c(by_field)]
    }

    df_sub <- unique(df_sub[, c(cols, by_field), with = FALSE])

    agg_re <- data.table::melt(df_sub, id.vars = c(by_field), variable.name = 'variable', value.name = 'value')
    agg_re[, day_type := key]
    result[[key]] <- agg_re
  }

  result_df <- data.table::rbindlist(result, fill = TRUE)
  result_df[, variable := factor(variable)]
  result_df[, variable := plyr::mapvalues(variable, from = col_dict_final[[mytype]]$old, to = col_dict_final[[mytype]]$new)]
  data.table::setorder(result_df, variable, day_type)

  return(result_df)
}

#' Generate Activity or Trip Summary Table
#'
#' @param ucalitems Calendar items data table.
#' @param day_summary Per day duration data table.
#' @param mytype 'ACTIVITY' or 'TRIP'.
#' @param by_field Subtype grouping column name.
#' @param trip_num Minimum trip threshold.
#' @return Pivoted summary data table.
#' @export
activity_trip <- function(ucalitems, day_summary, mytype,
                          by_field = 'subtype_decoded', trip_num = -1) {
  valid_days_count <- get_valid_days(day_summary, trip_num)
  result_df <- get_summary(ucalitems, mytype, valid_days_count, by_field)

  total_row <- cbind(by_field = 'Total', result_df[, .(value = sum(value)), by = .(variable, day_type)])
  colnames(total_row)[1] <- by_field
  result_df <- rbind(result_df, total_row)

  result_df <- data.table::dcast(result_df, formula = stats::formula(paste(by_field, '~ variable + day_type')), value.var = 'value')
  result_df[is.na(result_df)] <- 0

  return(result_df)
}

#' Plot Summary Bar Chart for Activities/Trips
#'
#' @param ucalitems Calendar items data table.
#' @param tb Table name identifier string.
#' @param day_summary Per day duration data table.
#' @param mytype 'ACTIVITY' or 'TRIP'.
#' @param by_field Subtype grouping column name.
#' @param directory Directory for saving widget HTML.
#' @param agg_col Aggregation variable name.
#' @param save_results Logical indicating whether to save widget file.
#' @return Plotly widget object.
#' @export
activity_trip_figure <- function(ucalitems, tb, day_summary, mytype,
                                 by_field = 'subtype_decoded', directory = NULL, agg_col,
                                 save_results = FALSE) {
  valid_days_count <- get_valid_days(day_summary)
  df <- data.table::copy(ucalitems)

  if (mytype == 'ACTIVITY' && agg_col == 'duration_after_split' && by_field == 'subtype_decoded') {
    df[type_decoded == 'TRIP', subtype_decoded := 'TRIP']
    df[type_decoded == 'TRIP', type_decoded := 'ACTIVITY']
    df[type_decoded == 'DEVICE OFF', subtype_decoded := 'DEVICE OFF']
    df[type_decoded == 'DEVICE OFF', type_decoded := 'ACTIVITY']
  } else if (mytype == 'TRIP' && by_field == 'trip_purpose') {
    levels(df$trip_purpose) <- c(levels(df$trip_purpose), 'DEVICE OFF')
    df[, trip_purpose := replace(trip_purpose, trip_purpose == 'UNKNOWN', 'DEVICE OFF')]
  }

  col_dict_final <- list(
    'ACTIVITY' = data.table::data.table(old = c('id', 'duration_after_split'), new = c('Activity Counts', 'Activity Duration in Hours')),
    'TRIP' = data.table::data.table(old = c('id', 'duration_after_split', 'distance_after_split'), new = c('Trip Counts', 'Trip Duration in Minutes', 'Trip Distance in Miles'))
  )

  new_agg_col <- col_dict_final[[mytype]][old == agg_col, new][[1]]
  result_df <- get_summary(df, mytype, valid_days_count, by_field)
  result_df <- result_df[variable == new_agg_col]

  result_df[, (by_field) := factor(get(by_field))]
  legend_order <- rev(levels(result_df[[by_field]]))

  fig <- plotly::plot_ly(data = result_df) %>%
    plotly::add_trace(x = ~value, y = ~day_type, type = "bar", color = stats::as.formula(paste0("~", by_field))) %>%
    plotly::layout(
      barmode = 'stack',
      yaxis = list(title = list(text = ''), tickfont = list(size = 16)),
      xaxis = list(title = list(text = new_agg_col, font = list(size = 18)), tickfont = list(size = 16)),
      legend = list(title = list(text = by_field), traceorder = 'normal', items = list(order = legend_order), font = list(size = 16)),
      width = 1000, height = 450
    )

  if (save_results && !is.null(directory)) {
    if (!dir.exists(directory)) dir.create(directory, recursive = TRUE)
    if (tb == 'ucalitems_temporal_plot' && mytype == 'TRIP') {
      filename <- file.path(directory, paste0(mytype, "_segment_", new_agg_col, "_", by_field, ".html"))
    } else if (tb == 'leg2trip') {
      filename <- file.path(directory, paste0(mytype, "_complete_", new_agg_col, "_", by_field, ".html"))
    } else {
      filename <- file.path(directory, paste0(mytype, "_", new_agg_col, "_", by_field, ".html"))
    }
    htmlwidgets::saveWidget(fig, file = filename)
  }

  return(fig)
}
