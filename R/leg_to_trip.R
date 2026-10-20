#' Combine Trip Legs into Complete Trips
#'
#' Aggregates multiple leg segments into complete trips based on longest distance or duration leg.
#'
#' @param ucalitems_temporal_plot Data table prepared for temporal plotting.
#' @param criterion Criterion to select main mode ('distance' or 'duration').
#' @return Data table of consolidated trips.
#' @export
leg2trip <- function(ucalitems_temporal_plot, criterion = 'distance') {
  duration_sum <- ucalitems_temporal_plot[, .(distance_after_split = sum(distance_after_split),
                                               duration_after_split = sum(duration_after_split)),
                                          by = .(user_id, start_date, leg2tripid, subtype_decoded)]

  if (criterion == 'duration') {
    leg2trip_index <- duration_sum[, .I[which.max(duration_after_split)], by = .(user_id, start_date, leg2tripid)]
  } else {
    leg2trip_index <- duration_sum[, .I[which.max(distance_after_split)], by = .(user_id, start_date, leg2tripid)]
  }
  longest_type <- duration_sum[leg2trip_index$V1, .(user_id, start_date, leg2tripid, subtype_decoded)]

  other_attributes <- ucalitems_temporal_plot[, .(
    distance_after_split = sum(distance_after_split),
    duration_after_split = sum(duration_after_split),
    start_timestamp = data.table::first(start_timestamp),
    end_timestamp = data.table::last(end_timestamp),
    type_decoded = data.table::first(type_decoded),
    trip_purpose = data.table::first(trip_purpose),
    survey_not_null = max(survey_not_null),
    start_dt = data.table::first(start_dt),
    end_dt = data.table::last(end_dt),
    end_date = data.table::last(end_date),
    dow = data.table::first(dow),
    IsWeekend = data.table::first(IsWeekend),
    start_time = data.table::first(start_time),
    end_time = data.table::last(end_time),
    id = data.table::first(id)
  ), by = .(user_id, start_date, leg2tripid)]

  segment_temp <- ucalitems_temporal_plot[, .(id, user_id, start_date, leg2tripid,
                                              subtype_decoded, distance_after_split, duration_after_split)]

  segment_temp[, subtype_decoded := as.character(subtype_decoded)]
  segment_temp[, id := as.character(id)]

  segment_attributes <- lapply(c("subtype_decoded", "duration_after_split", "distance_after_split", "id"), function(col) {
    segment_temp[, .(col = paste(.SD[[1]], collapse = "_")), by = .(user_id, start_date, leg2tripid), .SDcols = col]
  })

  segment_attributes_df <- data.table::data.table(
    user_id = segment_attributes[[1]]$user_id,
    start_date = segment_attributes[[1]]$start_date,
    leg2tripid = segment_attributes[[1]]$leg2tripid,
    segment_subtype = segment_attributes[[1]]$col,
    segment_duration_hour = segment_attributes[[2]]$col,
    segment_distance_meter = segment_attributes[[3]]$col,
    segment_id = segment_attributes[[4]]$col
  )

  result <- Reduce(function(x, y) merge(x, y, by = c("user_id", "start_date", "leg2tripid"), all = TRUE),
                   list(longest_type, other_attributes, segment_attributes_df))

  return(result)
}
