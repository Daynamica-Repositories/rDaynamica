count_valid_per_days <- function(day_summary, 
                                 numerator_col = 'with_subtype', 
                                 denominator_filter = 'interact_with_app > 0') {
  
  # Filter the data.table based on the denominator_filter
  day_summary_sub <- day_summary[eval(parse(text = denominator_filter))]
  
  count_days <- function(dt,day = NA){
    total_days <- nrow(dt)
    if(!is.na(day)){
      result = data.table('Day of the Week' = 'Total',
                          'Total # of Days' = total_days)
    } else{
      result = data.table('Day of the Week' = dt$dow[1],
                          'Total # of Days' = total_days)
    }
    
    for (threshold in threshold_list) {
      sub_days = dt[eval(parse(
        text = paste0(numerator_col,'>=',threshold)))] %>% nrow()
      
      if (total_days == 0) {
        sub_days_per <- NA
      } else {
        sub_days_per <- sub_days / total_days * 100
        result <- cbind(result, sprintf("%d (%.1f%%)", sub_days, sub_days_per))
      }
    }
    return(result)
  }
  
  # Minimum hours to define valid days, include 24, 20, 16, 12, 8 hours
  #used as columns for the output table
  threshold_list <- c(24, 20, 16, 12, 8)
  threshold_list <- threshold_list - 0.01
  
  # Days of the week, used as rows for the output table
  dow_order <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 
                 'Saturday', 'Sunday')
  day_summary_sub[,dow := factor(dow,levels = dow_order)]
  
  suppressMessages(summary <- lapply(day_summary_sub %>% 
                      split(by = 'dow', sorted = TRUE),
                    count_days) %>% bind_rows())
  
  suppressMessages(summary <- bind_rows(summary,count_days(day_summary_sub, day='Total')))
  setnames(summary, c('Day of the Week', 'Total # of Days',
                      sprintf("%.0f hours", 
                              threshold_list)))
  
  return(summary)
}


query_valid_days_func <- function(csv_dict, tb,
                                  query_text, 
                                  csv_dict_ref = csv_dict, tb_ref='day_summary', 
                                  cols = c('user_id', 'start_date')) {
  
  df <- inner_join(
    csv_dict[[tb]],
    csv_dict_ref[[tb_ref]] %>% 
      filter(eval(parse(text = query_text))) %>% 
      select(all_of(cols))  %>% 
      unique(),
    by = cols
  )
  
  return(df)
}


filter_valid_days <- function(csv_dict, query_text) {
  
  csv_dict_sub <- list()
  
  # 0. day_summary filtered by query_text
  csv_dict_sub[['day_summary']] <- csv_dict[['day_summary']] %>% 
    filter(eval(parse(text = query_text)))
  cat(sprintf('# days before filtering: %d. # days after filtering: %d\n', 
              nrow(csv_dict[['day_summary']]), 
              nrow(csv_dict_sub[['day_summary']])))
  
  # 1. ucalitems_ljoin_ucisurvey_split filtered by query_text
  csv_dict_sub[['ucalitems_ljoin_ucisurvey_split']] <- copy(query_valid_days_func(
    csv_dict, 'ucalitems_ljoin_ucisurvey_split', query_text))
  csv_dict_sub[['ucalitems_ljoin_ucisurvey_split']][,IsWeekend := fifelse(
    weekdays(start_date) %in% c('Saturday', 'Sunday'), 
    'Weekend', 'Weekday')]
  
  cat(sprintf('Table Name: ucalitems_ljoin_ucisurvey_split
              # items before filtering: %d. 
              # items after filtering: %d\n', 
              nrow(csv_dict[['ucalitems_ljoin_ucisurvey_split']]), 
              nrow(csv_dict_sub[['ucalitems_ljoin_ucisurvey_split']])))
  
  # 2. ucalitems_temporal_plot, a copy of ucalitems_ljoin_ucisurvey for creating plots 
  #    with formatted time and decoded activity and trip types & subtypes
  csv_dict_sub[['ucalitems_temporal_plot']] <- 
    copy(csv_dict_sub[['ucalitems_ljoin_ucisurvey_split']])
  
  csv_dict_sub[['ucalitems_temporal_plot']][,`:=`(
      IsWeekend = fifelse(weekdays(start_date)%in% c('Saturday', 'Sunday'), 
                          'Weekend', 'Weekday'),
      start_time = format(start_dt, format = "%H:%M:%S"),
      end_time = format(end_dt, format = "%H:%M:%S"),
      type_decoded = fifelse(type_decoded %in% 
                               c('DATA COLLECTION STARTED','INACC','OFF'),
                             'DEVICE OFF', type_decoded),
      subtype_decoded = fcase(
        subtype_decoded == 'ACTIVITY', 'OTHER ACTIVITIES',
        subtype_decoded == 'TRIP', 'OTHER TRIPS',
        subtype_decoded == 'WORK', 'WORKPLACE',
        type_decoded == 'ACTIVITY' & 
          subtype_decoded == 'OTHER', 'OTHER ACTIVITIES',
        type_decoded == 'TRIP' & 
          subtype_decoded == 'OTHER','OTHER TRIPS',
        rep_len(TRUE, csv_dict_sub[['ucalitems_temporal_plot']] %>% nrow()),
        subtype_decoded),
      trip_purpose = fcase(
        trip_purpose == 'ACTIVITY', 'OTHER ACTIVITIES',
        trip_purpose == 'TRIP', 'OTHER TRIPS',
        trip_purpose == 'WORK', 'WORKPLACE',
        trip_purpose == 'OTHER', 'OTHER ACTIVITIES',
        rep_len(TRUE, csv_dict_sub[['ucalitems_temporal_plot']] %>% nrow()),
        trip_purpose)
    )]
  
  # I am turning this off. It assumes a constant set of levels which is not reliable. 
  # I don't think having a predefined ordering is as important as having the flexibility to 
  # accommodate different re categorizations of our data. 
  
  
  # custom_order <- c('HOME', 'WORKPLACE','EDUCATION', 'FOOD & MEAL', 
  #                   'MEDICAL & FITNESS', 'FUN & LEISURE', 
  #                   'COMMUNITY & CULTURAL','RELIGIOUS & SPIRITUAL',
  #                   'CARE GIVING', 'SHOPPING ERRANDS', 'CIVIL ERRANDS',
  #                   'OTHER ACTIVITIES', 'WALK', 'BIKE', 'CAR - DRIVER',
  #                   'CAR - PASSENGER', 'VEHICLE','TAXI/UBER/LYFT',
  #                   'RAIL', 'BUS', 'WAIT', 'OTHER TRIPS',
  #                   'UNKNOWN')
  # 
  # # Reorder the 'category' column based on the custom order
  # csv_dict_sub[['ucalitems_temporal_plot']][, subtype_decoded := 
  #                                             factor(subtype_decoded, 
  #                                                    levels = custom_order)]
  
  # 3. Filter activities in valid days and save activities as a new table 'ucalitems_activity'
  csv_dict_sub[['ucalitems_activity']] <- copy(query_valid_days_func(
    csv_dict, 'ucalitems_ljoin_ucisurvey_split', query_text)) %>% 
    filter(type_decoded == 'ACTIVITY')
  
  #cat(sprintf('# activities with centroid after filtering: %d.\n', 
  #            nrow(csv_dict_sub[['ucalitems_activity']])))
  
  # Save filtered data (original tables with filtered hours)
  # Note: the table exit_survey is not included because it's not complete (only 9 records)
  # Note: the table ucalitems (trip / activity episodes) is not included 
  # because it contains episodes that cross multiple days and cannot be assigned to a single day. 
  # Instead, please use the saved table ucalitems_ljoin_ucisurvey for episode-level calculation.
  if(!is.null(csv_dict[['ema_survey']])){
    # 4. ema_survey
    if('ema_survey_date' %in% names(csv_dict[['ema_survey']])){
      #setnames(csv_dict[['ema_survey']],'ema_survey_date','start_date')
      #csv_dict[['ema_survey']]$start_date <- csv_dict[['ema_survey']]$ema_survey_date
      csv_dict[['ema_survey']][,start_date := ema_survey_date]
      csv_dict[['ema_survey']][,start_date := as.Date(start_date)]
      #csv_dict[['ema_survey']][,start_date := as.Date(start_date, origin = "1970-01-01")]
    }
    
    csv_dict_sub[['ema_survey']] <- 
      copy(query_valid_days_func(csv_dict, 'ema_survey', query_text))
    
    cat(sprintf('Table Name: ema_survey
              # items before filtering: %d. 
              # items after filtering: %d\n', 
                nrow(csv_dict[['ema_survey']]), 
                nrow(csv_dict_sub[['ema_survey']])))
  }
  
  if(!is.null(csv_dict[['calendar_item_survey']])){
    # 5. calendar_item_survey
    csv_dict_sub[['calendar_item_survey']] <-
      copy(query_valid_days_func(csv_dict, 'calendar_item_survey', 
                                 query_text = '!is.na(id)',
                                 csv_dict_sub, 'ucalitems_ljoin_ucisurvey_split',
                                 cols = c('user_id', 'cal_item_id')))
    
    cat(sprintf('Table Name: calendar_item_survey 
              # items before filtering: %d. 
              # items after filtering: %d\n', 
                nrow(csv_dict[['calendar_item_survey']]), 
                nrow(csv_dict_sub[['calendar_item_survey']])))
  }
  
  
  return(csv_dict_sub)
}








