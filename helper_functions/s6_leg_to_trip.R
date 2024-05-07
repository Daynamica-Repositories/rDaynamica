library(data.table)
library(dplyr)
library(stringr)

# Define the leg2trip function
leg2trip <- function(ucalitems_temporal_plot, criterion = 'distance') {
  # Calculate the sum of distance_after_split for each subtype in a complete trip
  duration_sum <- ucalitems_temporal_plot[, .(distance_after_split = 
                                                sum(distance_after_split)),
                                          by = .(user_id, start_date, 
                                                 leg2tripid, subtype_decoded)]
  
  # Find the longest leg in each complete trip
  if(criterion == 'duration'){
    leg2trip_index <- duration_sum[, .I[which.max(duration_after_split)], 
                                   by = .(user_id, start_date, leg2tripid)]
    longest_type <- duration_sum[leg2trip_index$V1,.(user_id,start_date,
                                                     leg2tripid, subtype_decoded)]
    
  } else{
    leg2trip_index <- duration_sum[, .I[which.max(distance_after_split)], 
                                   by = .(user_id, start_date, leg2tripid)]
    longest_type <- duration_sum[leg2trip_index$V1,.(user_id,start_date,
                                                     leg2tripid, subtype_decoded)]
  }
  
  
  # Calculate other attributes for complete trips
  other_attributes <- ucalitems_temporal_plot[
    , .(distance_after_split = sum(distance_after_split),
        duration_after_split = sum(duration_after_split),
        start_timestamp = first(start_timestamp),
        end_timestamp = last(end_timestamp),
        type_decoded = first(type_decoded),
        trip_purpose = first(trip_purpose),
        survey_not_null = max(survey_not_null), #why?
        start_dt = first(start_dt),
        end_dt = last(end_dt),
        end_date = last(end_date),
        dow = first(dow),
        IsWeekend = first(IsWeekend),
        start_time = first(start_time),
        end_time = last(end_time),
        id = first(id)),
    by = .(user_id, start_date, leg2tripid)]
  
  # Create a data.table for segment attributes
  segment_temp <- ucalitems_temporal_plot[, .(id, user_id, start_date, leg2tripid, 
                                              subtype_decoded, 
                                              distance_after_split, 
                                              duration_after_split)]
  
  # Remove spaces from subtype_decoded and convert duration and distance to strings
  segment_temp[, subtype_decoded := as.character(subtype_decoded)]
  segment_temp[, id := as.character(id)]
  #segment_temp[, duration_after_split := as.character(ceiling(60 * duration_after_split))]
  #segment_temp[, distance_after_split := as.character(ceiling(distance_after_split))]
  
  # Create separate data.tables for each segment attribute
  segment_attributes <- lapply(c("subtype_decoded", 
                                 "duration_after_split", 
                                 "distance_after_split",
                                 "id"), function(col) {
    segment_temp[, .(col = paste(.SD, collapse = "_")), 
                 by = .(user_id, start_date, leg2tripid), .SDcols = col]
  })
  
  # Merge segment attributes data.tables
  segment_attributes_df <- data.table(
    segment_attributes[[1]],
    segment_duration_hour = segment_attributes[[2]]$col,
    segment_distance_meter = segment_attributes[[3]]$col,
    segment_id = segment_attributes[[4]]$col
  )
  
  # Rename columns
  setnames(segment_attributes_df, c("col"), c("segment_subtype"))
  
  # Merge all the tables to get the result
  result <- Reduce(function(x, y) merge(x, y, 
                                        by = c("user_id",
                                               "start_date", 
                                               "leg2tripid"), all = TRUE),
                   list(longest_type, other_attributes, 
                        segment_attributes_df))
  
  #custom_order <- c('HOME', 'WORKPLACE','EDUCATION', 'FOOD & MEAL',
  #                  'MEDICAL & FITNESS', 'FUN & LEISURE',
  #                  'COMMUNITY & CULTURAL','RELIGIOUS & SPIRITUAL',
  #                  'CARE GIVING', 'SHOPPING ERRANDS', 'CIVIL ERRANDS',
  #                  'OTHER ACTIVITIES', 'WALK', 'BIKE', 'CAR - DRIVER',
  #                  'CAR - PASSENGER', 'VEHICLE','TAXI/UBER/LYFT',
  #                  'RAIL', 'BUS', 'WAIT', 'OTHER TRIPS',
  #                  'UNKNOWN')

  # Reorder the 'category' column based on the custom order
  #result[, subtype_decoded := factor(subtype_decoded,
  #                                   levels = custom_order)]

  #result[, trip_purpose := factor(trip_purpose,
  #                                   levels = custom_order)]
  
  #cat('# rows before leg2trip:', nrow(ucalitems_temporal_plot), 
  #    '. # rows after leg2trip:', nrow(result), '\n')
  
  return(result)
}
