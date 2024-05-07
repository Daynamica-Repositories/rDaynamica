ucal_items_version_updater <- function(csv_dict){
  ucal_items <- csv_dict$ucalitems
  
  # Check if subtype_decoded exists and create it if needed
  if(!("subtype_decoded" %in% names(ucal_items))){
    ucal_items$subtype_decoded <- ucal_items$subtype
    
  }
  # Check if type_decoded exists and create it if needed
  if(!("type_decoded" %in% names(ucal_items))){
    ucal_items$type_decoded <- ucal_items$type
  }
  #
  
  # Check the format of the date/time object
  if(!is.numeric(ucal_items$start_timestamp)){
    if(length(strsplit(as.character(ucal_items$start_timestamp[1]), ":")[[1]]) == 2){
      ucal_items$start_timestamp <- as.numeric(as.POSIXct(lubridate::mdy_hm(ucal_items$start_timestamp)))*1000
      ucal_items$end_timestamp <- as.numeric(as.POSIXct(lubridate::mdy_hm(ucal_items$end_timestamp)))*1000
    } else if(length(strsplit(as.character(ucal_items$start_timestamp[1]), ":")[[1]]) == 3){
      ucal_items$start_timestamp <- as.numeric(as.POSIXct(lubridate::mdy_hms(ucal_items$start_timestamp)))*1000
      ucal_items$end_timestamp <- as.numeric(as.POSIXct(lubridate::mdy_hms(ucal_items$end_timestamp)))*1000
      
    }
    # ucalitems_ljoin_ucisurvey <- csv_dict$ucalitems_ljoin_ucisurvey
  }
  
  csv_dict[["ucalitems"]] <- ucal_items
  return(csv_dict)
}



assign_trip_purpose <- function(ucalitems){
  ucalitems <- copy(ucalitems)
  setkey(ucalitems, user_id, start_timestamp)
  
  # Calculate the type_decoded_pre column
  ucalitems[, type_decoded_pre := shift(type_decoded), 
                          by = .(user_id)]
  
  ucalitems[, type_decoded_next := shift(type_decoded, n = -1), 
            by = .(user_id)]
  
  # Create a flag to identify trip boundaries
  ucalitems[, flag := (type_decoded != 'TRIP' | 
                         type_decoded_pre != 'TRIP'|
                         is.na(type_decoded_pre))]
  
  # Create leg2tripid
  ucalitems[, leg2tripid := cumsum(flag)]
  
  #assign trip purpose
  ucalitems[, trip_purpose := 
              fifelse(type_decoded == 'TRIP',
                      shift(subtype_decoded, n = -1),
                      subtype_decoded),
            by = .(user_id)]
  
  ucalitems[, trip_purpose := 
              fifelse(type_decoded_next == 'TRIP' & type_decoded == 'TRIP',
                      last(trip_purpose),
                      trip_purpose),
            by = .(user_id, leg2tripid)]
  
  columns_to_remove <- c("type_decoded_pre", "type_decoded_next", "flag")
  
  # Remove columns based on column names
  ucalitems[, .SD, .SDcols = !columns_to_remove]
  
  ucalitems[, trip_purpose := 
              fifelse(is.na(trip_purpose),
                      'UNKNOWN',
                      trip_purpose)]
  
  return(ucalitems)
}

#join trip_id back to the survey table
survey_join_id <- function(ucalitems_survey, ucalitems){
  # Join using data.table syntax
  if("calendar_item_id" %in% names(ucalitems_survey)) {
    setnames(ucalitems_survey, 
             old = c("calendar_item_id", "calendar_item_timestamp"), 
             new = c("cal_item_id", "start_timestamp"))
  }

  result <- merge(ucalitems_survey,
                  ucalitems[,c("user_id", "cal_item_id", 
                               "start_timestamp","leg2tripid")], 
                  by = c("user_id", "cal_item_id", "start_timestamp"),
                  all.x = T)
  
  return(result)
}

#join survey table & calendar item table
ucalitems_ljoin_ucisurvey <- function(ucalitems_survey, ucalitems) {

  # Filter rows where response is not NA
  ucalitems_survey_agg <- ucalitems_survey[!is.na(response)]
  
  # Aggregation by episode
  ucalitems_survey_agg <- ucalitems_survey_agg[, .(valid_survey = .N), 
                                               by = .(user_id, 
                                                      cal_item_id, 
                                                      start_timestamp)]
  
  # Set survey_not_null to TRUE
  ucalitems_survey_agg[, survey_not_null := TRUE]
  
  # Join using data.table syntax
  result <- merge(ucalitems,
                  ucalitems_survey_agg %>%
                    select(-start_timestamp), 
                  by = c("user_id", "cal_item_id"),
                  all.x = T)
  
  # Set survey_not_null to FALSE for missing values
  result[is.na(survey_not_null), survey_not_null := FALSE]
  result[, survey_not_null := max(survey_not_null), by = .(leg2tripid)]
  
  cat(sprintf("ucalitems with survey:%s \n without survey: %s",
              table(result$survey_not_null)[2],
              table(result$survey_not_null)[1]))
   return(result)
}


split_ucalitems <- function(ucalitems_ljoin_ucisurvey, 
                            local_timezone = 'US/Central',
                            day_start_hour = 0,
                            min_time_stamp = 0) {
  
  if (day_start_hour != round(day_start_hour) | day_start_hour < 0 | day_start_hour >6) {
    cli::cli_abort("Please set day_start_hour as interger [0,6]!")
  }
  
  # Convert timestamps to datetime objects with the specified timezone
  if(!is.numeric(ucalitems_ljoin_ucisurvey$start_timestamp)){
    # ucalitems_ljoin_ucisurvey <- csv_dict$ucalitems_ljoin_ucisurvey
    ucalitems_ljoin_ucisurvey[, `:=`(start_timestamp = as.numeric(as.POSIXct(lubridate::mdy_hm(start_timestamp)))*1000,
                                     end_timestamp = as.numeric(as.POSIXct(lubridate::mdy_hm(end_timestamp)))*1000)]
  }
  
  ucalitems_ljoin_ucisurvey[, `:=`(
    start_dt_ori = as.POSIXct(start_timestamp/1000, 
                              origin = "1970-01-01", 
                              tz = "UTC"),
    end_dt_ori = as.POSIXct(end_timestamp/1000, 
                            origin = "1970-01-01", 
                            tz = "UTC")
    )][, `:=`(
      start_dt = start_dt_ori - day_start_hour*3600,
      end_dt = end_dt_ori - day_start_hour*3600
    )][, `:=`(
      start_date = as.Date(start_dt, tz = local_timezone),
      end_date = as.Date(end_dt, tz = local_timezone)
    )][, 
       days := difftime(end_date, start_date, units = "days") + 1
    ]
  
  ucalitems_ljoin_ucisurvey <- ucalitems_ljoin_ucisurvey[days > 0][
    order(user_id, start_dt)
  ][, `:=`(
    id = seq_len(.N),
    duration_before_split = difftime(end_dt, start_dt, units = "hours") %>% 
      as.numeric()
  )]
  
  split_days <- lapply(ucalitems_ljoin_ucisurvey %>% split(by = 'id'),function(episode){
    start_date = seq.Date(episode$start_date,episode$end_date, by = 'days')
    id = rep(episode$id,episode$days)
    data.table(start_date,id)
  }) %>% rbindlist()
  
  split_days <- split_days[order(id, start_date)]
  
  # Outer join to fill missing values
  result <- merge(split_days, ucalitems_ljoin_ucisurvey, 
                  by = "id", all.x = TRUE,suffixes = c("", "_raw"))
  
  # Update start and end datetime for split days
  result[,start_dt := fifelse(
    start_date != start_date_raw,
    as.POSIXct(paste(start_date, "00:00:00")),
    start_dt
  )]
  
  result[,end_dt := fifelse(
    start_date != end_date,
    as.POSIXct(paste(start_date + 1, "00:00:00")),
    end_dt
  )]
  
  # Update duration and distance after splitting
  result[, `:=`(
    duration_after_split = difftime(end_dt, start_dt, units = "hours") %>% 
      as.numeric(),
    start_date = as.Date(start_dt, tz = local_timezone),
    end_date = as.Date(end_dt, tz = local_timezone),
    dow = weekdays(start_date))]
  
  result[,distance_after_split := 
           duration_after_split/duration_before_split * distance
         ][,distance_after_split := fifelse(
           is.na(distance_after_split), 0, distance_after_split)]
  
  #drop columns
  result <- result[,!names(result) %in% c("days", "start_date_raw"),with = F]
  
  cat(sprintf("# rows of original ucalitems: %d. 
              # rows after splitting: %d\n", 
              nrow(ucalitems_ljoin_ucisurvey), 
              nrow(result)))
  
  cat(sprintf("# hours in original ucalitems: %.2f. 
              # hours after splitting: %.2f\n", 
              sum(ucalitems_ljoin_ucisurvey$duration_before_split), 
              sum(result$duration_after_split)))
  
  cat(sprintf("# distance in original ucalitems: %.2f. 
              # distance after splitting: %.2f\n", 
              sum(ucalitems_ljoin_ucisurvey$distance), 
              sum(result$distance_after_split)))
  
  # Create interaction labels
  result[, `:=`(
    interact_with_app = (confirm_timestamp > min_time_stamp) | 
      (edit_timestamp > min_time_stamp),
    interact_by_confirm = confirm_timestamp > min_time_stamp,
    interact_by_edit = edit_timestamp > min_time_stamp)]
  
  return(result)
}

get_per_day_duration <- function(table) {
  
  mask_dict <- list(
    total = !is.na(table$user_id),
    no_off = table$type_decoded %in% c('ACTIVITY', 'TRIP'),
    interact_with_app = table$interact_with_app,
    interact_by_confirm = table$interact_by_confirm,
    interact_by_edit = table$interact_by_edit,
    with_subtype = (!table$subtype_decoded %in% c('ACTIVITY', 'TRIP')) & 
      table$type_decoded %in% c('ACTIVITY', 'TRIP'),
    with_survey = table$survey_not_null | table$subtype_decoded == 'HOME',
    trip_count = table$type_decoded == 'TRIP',
    trip_duration = table$type_decoded == 'TRIP',
    activity_count = table$type_decoded == 'ACTIVITY',
    activity_duration = table$type_decoded == 'ACTIVITY'
    )
  
  per_day <- mapply(function(ifvalid, key){
    table_sub <- table[ifvalid]
    if(key %in% c('interact_with_app', 'interact_by_confirm', 
                  'interact_by_edit', 'trip_count', 'activity_count')){
      per_day_duration <- table_sub[, .(hours = .N), 
                                    by = c('user_id', 'dow', 'start_date')]
    } else{
      per_day_duration <- table_sub[, .(hours = sum(duration_after_split)), 
                                    by = c('user_id', 'dow', 'start_date')]
    }
    per_day_duration[, stat_type := key]
  },mask_dict,names(mask_dict), SIMPLIFY = FALSE) %>%
    rbindlist()
  
  setkey(per_day, user_id, dow, start_date)
  
  per_day <- dcast(per_day, user_id + dow + start_date ~ stat_type, 
                  value.var = 'hours', 
                  fill = 0)
  
  per_day[, IsWeekend := fifelse(
    weekdays(start_date) %in% c('Saturday', 'Sunday'), 
    'Weekend', 'Weekday')]
  
  return(per_day)
}


exclude_user <- function(csv_dict, exclude_id){
  # get unique user ids in the original dataset
  userids <- unique(csv_dict[['ucalitems']][,user_id])
  # filter user ids
  userids <- userids[!apply(
    sapply(exclude_id, function(id) grepl(id, userids)), 
    1, any)]
  
  # for all files in the data dictionary, select records for the selected users only
  csv_dict = select_userids(csv_dict, userids)
  
  # Print the number of users advanced for further steps
  cat('Total number of users', length(userids), "\n")
  
  # get the total number of records in each table
  for (dict_name in names(csv_dict)){
    cat(sprintf('Table name: %s. # rows: %s. # columns: %s \n',
                dict_name, 
                nrow(csv_dict[[dict_name]]), 
                ncol(csv_dict[[dict_name]])))
  }
  
  return(csv_dict)
}

select_userids <- function(csv_dict, userids) {
  
  csv_dict_sub <- list()
  
  for (key in names(csv_dict)) {
    if("user_id" %in% names(csv_dict[[key]])){
      csv_dict_sub[[key]] <- filter(csv_dict[[key]], user_id %in% userids)
    }
    
  }
  
  return(csv_dict_sub)
}


rename_userids <- function(csv_dict, seed = NULL){
  if(!is.null(seed)){
    set.seed(seed)
  }
  study_size <- 10^c(1:6)
  
  unique_list_of_ids <- unique(c(csv_dict$calendar_item_survey$user_id,
                               csv_dict$ema_survey$user_id,
                               csv_dict$ucalitems$user_id))
  new_ids <- paste0("user_id_", sample(1:study_size[min(which(study_size > length(unique_list_of_ids)))],
         length(unique_list_of_ids),
         replace = F))
  id_dict <- data.frame(orig_id = unique_list_of_ids,
                        new_id = new_ids) %>%
    filter(!is.na(orig_id), !is.na(new_id))
  for(item in 1:length(csv_dict)){
    temp_data <- csv_dict[[item]]
    if(!("user_id" %in% names(temp_data))){
      next
    }
    for(i in 1:nrow(id_dict)){
      temp_data$user_id <- 
        gsub(id_dict$orig_id[i],
             id_dict$new_id[i],
             temp_data$user_id)
    }
    csv_dict[[item]] <- temp_data
  }
  csv_dict[["id_dictionary"]] <- id_dict
  return(csv_dict)
}

recategorize_calendar_items <- function(csv_dict, orig_cats, new_cats){
  if(length(orig_cats) != length(new_cats)){
    stop("Length of the original and new subtype category labels must be the same.")
  }
  
  tables_with_subtype_decoded_columns <- 
  lapply(csv_dict, names) %>% # Get Column Names for each table
    lapply(grepl, pattern = "subtype_decoded") %>% # Identify each column which matches subtype_decoded 
    lapply(sum) %>%
    unlist()
  tables_with_subtype_decoded_columns <-  names(tables_with_subtype_decoded_columns)[which(tables_with_subtype_decoded_columns == 1)]
  if(length(tables_with_subtype_decoded_columns) == 0){
    stop("Please make sure that the subtype column is properly labeled as 'subtype_decoded'.")
  }
  message(paste0("Modifying the subtypes of the following tables: ", paste(tables_with_subtype_decoded_columns, collapse = ", ")))
  
  unique_levels <- c()
  for(i in 1:length(tables_with_subtype_decoded_columns)){
    unique_levels <- c(unique_levels, unique(csv_dict[[tables_with_subtype_decoded_columns[i]]]$subtype_decoded))
  }
 
  test_for_mislabels <- which(!(orig_cats %in% unique_levels))
  if(length(test_for_mislabels) > 0){
    warning(paste0("The following labels were not found within the dataset. Labels: ", paste(orig_cats[test_for_mislabels], collapse = ", "), " had no match in dataset."))
  }
  
  
  
  # Note, you do not have to specify existing levels that you do not wish to change. 
  subtype_cats <- 
    data.frame(orig_labels = unique_levels,
             new_labels = unique_levels,
             new = F)
  
  for(i in 1:length(orig_cats)){
    subtype_cats$new_labels[which(subtype_cats$orig_labels == orig_cats[i])] <- new_cats[i]
    subtype_cats$new[which(subtype_cats$orig_labels == orig_cats[i])] <- T
  }
  subtype_cats <-
    subtype_cats %>%
    dplyr::arrange(new_labels) %>%
    group_by(new_labels) %>%
    dplyr::mutate(temp = row_number(),
           test = ifelse(temp == 1, 1, 0)) %>%
    ungroup() %>%
    dplyr::mutate(subtype = cumsum(test)) %>% # We need to relabel the subtype category
    select(-temp, -test)
  
  
  
#  csv_dict$ucalitems$orig_subtype_decoded <- csv_dict$ucalitems$subtype_decoded
#  csv_dict$ucalitems$orig_subtype <- csv_dict$ucalitems$subtype
# 
#   for(i in 1:nrow(subtype_cats)){
#     if(subtype_cats$new[i]){
#       csv_dict$ucalitems$subtype_decoded <- 
#         gsub(subtype_cats$orig_labels[i],
#              subtype_cats$new_labels[i],
#              csv_dict$ucalitems$subtype_decoded)
#     }
# 
#   }
#   csv_dict$ucalitems$subtype <- as.numeric(as.character(factor(csv_dict$ucalitems$subtype_decoded,
#                                        levels = subtype_cats$new_labels,
#                                        labels = subtype_cats$subtype)))
#   
    for(j in 1:length(tables_with_subtype_decoded_columns)){
      csv_dict[[tables_with_subtype_decoded_columns[j]]]$orig_subtype_decoded <- csv_dict[[tables_with_subtype_decoded_columns[j]]]$subtype_decoded
      csv_dict[[tables_with_subtype_decoded_columns[j]]]$orig_subtype <- csv_dict[[tables_with_subtype_decoded_columns[j]]]$subtype
      
      for(i in 1:nrow(subtype_cats)){
        if(subtype_cats$new[i]){
          csv_dict[[tables_with_subtype_decoded_columns[j]]]$subtype_decoded <- 
            ifelse(csv_dict[[tables_with_subtype_decoded_columns[j]]]$subtype_decoded == subtype_cats$orig_labels[i],
                   subtype_cats$new_labels[i],
                   csv_dict[[tables_with_subtype_decoded_columns[j]]]$subtype_decoded)
            #gsub(subtype_cats$orig_labels[i],
            #     subtype_cats$new_labels[i],
            #     csv_dict[[tables_with_subtype_decoded_columns[j]]]$subtype_decoded)
        }
        
      }
      csv_dict[[tables_with_subtype_decoded_columns[j]]]$subtype <- as.numeric(as.character(factor(csv_dict[[tables_with_subtype_decoded_columns[j]]]$subtype_decoded,
                                                                   levels = subtype_cats$new_labels,
                                                                   labels = subtype_cats$subtype)))
    }
  
  return(csv_dict)
  }

