# Color maps
activity_color_discrete_map <- c(
  #activities
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
  'DEVICE OFF' = '#f0f0f0')

#trips
trip_color_discrete_map = c(
  'CAR - DRIVER' = '#fdbf6f',
  'CAR - PASSENGER' = '#ff7f00',
  'VEHICLE' = '#b15928',
  'TAXI/UBER/LYFT' = '#fb9a99',
  'RAIL' = '#1f78b4',
  'BUS' = '#a6cee3',
  'BIKE' = '#33a02c',
  'WALK' = '#b2df8a',
  'WAIT' = '#6a3d9a',
  'OTHER TRIPS' = '#d9d9d9')

figure_color_maps = list(
  'ACTIVITY'=  activity_color_discrete_map, 
  'TRIP' = trip_color_discrete_map
)

figure_height <- list('ACTIVITY' = 4.7,
                      'TRIP' = 3.2)

figure_title <- list('ACTIVITY' = 'Numeric values of activity duration and frequency per person per day by activity type.',
                     'TRIP' = 'Numeric values of trip duration, distance and frequency per person per day by trip type')

col_dict_final <- list(
  'ACTIVITY' = data.table(old = c('id','duration_after_split'),
                          new= c('Activity Counts','Activity Duration in Hours')),
  'TRIP' = data.table(old = c('id','duration_after_split','distance_after_split'),
                      new = c('Trip Counts','Trip Duration in Minutes',
                              'Trip Distance in Miles'))
)

agg_dict <- list(
  'ACTIVITY' = c(
    id = function(x) length(x),
    duration_after_split = function(x) sum(x)
  ),
  'TRIP' = c(
    id = function(x) length(x),
    duration_after_split = function(x) sum(x),
    distance_after_split = function(x) sum(x)
  )
)

# # overview_statistics function
# overview_statistics <- function(csv_dict, 
#                                 stat_group_cols = c('IsWeekend', 'Statistics')) {
#   per_day <- list()
#   
#   # Group by person_day
#   group_cols <- c('user_id', 'start_date')
#   
#   # Activity Space by person_day----
#   agg_list <- list(
#     c('convex_hull', 'area_mile', '8_Convex Hull Area (Square Miles)'),
#     c('sde_ellipse', 'area_mile', '90_Ellipse Area (Square Miles)'),
#     c('sde_ellipse', 'sx_mile', '91_Ellipse semi-major Axis (Miles)'),
#     c('sde_ellipse', 'sy_mile', '92_Ellipse semi-minor Axis (Miles)')
#   )
#   
#   for (item in agg_list) {
#     activity_space <- csv_dict[[item[1]]][, c('user_id', 'start_date', 
#                                                 item[2]), with = FALSE]
#     setnames(activity_space, item[2], 'value')
#     activity_space$Statistics <- item[3]
#     per_day[[length(per_day) + 1]] <- activity_space
#   }
#   
#   # Hours of no-off data-------
#   valid_duration <- csv_dict$day_summary[, .(user_id, start_date, 
#                                                value = no_off * 60)]
#   valid_duration$Statistics <- '1_Recorded Data per Day (Minutes)'
#   per_day[[length(per_day) + 1]] <- valid_duration
#   
#   # Daily summaries for activity and trip----
#   agg_list <- list(
#     c('ACTIVITY', 'duration_after_split', 'sum', 60, 
#          '2_Total Activity Duration per Day (Minutes)', 
#          'ucalitems_ljoin_ucisurvey_split'),
#     c('TRIP', 'duration_after_split', 'sum', 60, 
#          '3_Total Trip Duration per Day (Minutes)', 
#          'ucalitems_ljoin_ucisurvey_split'),
#     c('TRIP', 'distance_after_split', 'sum', 0.000621371, 
#          '5_Total Trip Distance per Day (Miles)', 
#          'ucalitems_ljoin_ucisurvey_split'),
#     c('ACTIVITY', 'id', 'count', 1, 
#          '6_Activity Count per Day', 
#          'ucalitems_ljoin_ucisurvey_split'),
#     c('TRIP', 'id', 'count', 1, 
#          '70_Trip (Segment) Count per Day', 
#          'ucalitems_ljoin_ucisurvey_split'),
#     c('TRIP', 'id', 'count', 1, 
#          '71_Trip (Complete) Count per Day', 
#          'leg2trip')
#   )
#   
#   for (item in agg_list) {
#     types <- item[1]
#     tb <- item[length(item)]
#     df_sub <- csv_dict[[tb]] %>%
#       filter(type_decoded %in% types)
#     
#     per_day_item <- df_sub[, .(value = ifelse(item[3]=='sum',
#                                                sum(get(item[2])),
#                                                length(get(item[2])))),
#                            by = c(group_cols)]
#     per_day_item[,value:=value * as.numeric(item[4])] 
#     
#     per_day_item <- merge(per_day_item, 
#                           valid_duration[, .(user_id, start_date)], 
#                           by = c('user_id', 'start_date'), all = TRUE)
#     per_day_item$value[is.na(per_day_item$value)] <- 0
#     per_day_item$Statistics <- item[[5]]
#     
#     per_day[[length(per_day) + 1]] <- per_day_item
#   }
#   
#   per_day_df <- rbindlist(per_day)
#   
#   # Extract data info, DOW
#   per_day_df$start_date <- as.Date(per_day_df$start_date)
#   #per_day_df$dow_num <- as.integer(format(per_day_df$start_date, '%u'))
#   per_day_df$dow <- weekdays(per_day_df$start_date)
#   per_day_df$IsWeekend <- (per_day_df$dow=='Saturday')|(per_day_df$dow=='Sunday')
#   
#   # Summarize the median, mean, std, min, max
#   result <- per_day_df[, .(Median = median(value, na.rm = TRUE), 
#                            Mean = mean(value, na.rm = TRUE),
#                            SD = sd(value, na.rm = TRUE), 
#                            Min = min(value, na.rm = TRUE),
#                            Max = max(value, na.rm = TRUE)),
#                        by = stat_group_cols]
#   
#   result <- result[order(Statistics)]
#   result[1:2,Max:=fifelse(Max>1440,1440,Max)]
#   result$Statistics <- gsub(".*_", "", result$Statistics)
#   
#   if (identical(stat_group_cols, c('Statistics'))) {
#     setnames(result, c('Statistics', 'Median', 'Mean', 'SD', 'Min', 'Max'))
#   } else if (identical(stat_group_cols, c('IsWeekend', 'Statistics'))) {
#     setnames(result, c('IsWeekend', 'Statistics', 'Median', 'Mean', 
#                        'SD', 'Min', 'Max'))
#   } else {
#     stop("Sorry, stat_group_cols parameter not correct")
#   }
#   
#   return(result)
# }


# overview_statistics function
overview_statistics <- function(csv_dict, 
                                stat_group_cols = c('IsWeekend', 'Statistics')) {
  per_day <- list()
  
  # Group by person_day
  group_cols <- c('user_id', 'start_date')
  if(!is.null(csv_dict[["convex_hull"]])){
    # Activity Space by person_day----
    agg_list <- list(
      c('convex_hull', 'area_mile', '8_Convex Hull Area (Square Miles)'),
      c('sde_ellipse', 'area_mile', '90_Ellipse Area (Square Miles)'),
      c('sde_ellipse', 'sx_mile', '91_Ellipse semi-major Axis (Miles)'),
      c('sde_ellipse', 'sy_mile', '92_Ellipse semi-minor Axis (Miles)')
    )
    
    for (item in agg_list) {
      activity_space <- csv_dict[[item[1]]][, c('user_id', 'start_date', 
                                                item[2]), with = FALSE]
      setnames(activity_space, item[2], 'value')
      activity_space$Statistics <- item[3]
      per_day[[length(per_day) + 1]] <- activity_space
    }
    
    # Hours of no-off data-------
    valid_duration <- csv_dict$day_summary[, .(user_id, start_date, 
                                               value = no_off * 60)]
    valid_duration$Statistics <- '1_Recorded Data per Day (Minutes)'
    per_day[[length(per_day) + 1]] <- valid_duration
    
    # Daily summaries for activity and trip----
    agg_list <- list(
      c('ACTIVITY', 'duration_after_split', 'sum', 60, 
        '2_Total Activity Duration per Day (Minutes)', 
        'ucalitems_ljoin_ucisurvey_split'),
      c('TRIP', 'duration_after_split', 'sum', 60, 
        '3_Total Trip Duration per Day (Minutes)', 
        'ucalitems_ljoin_ucisurvey_split'),
      c('TRIP', 'distance_after_split', 'sum', 0.000621371, 
        '5_Total Trip Distance per Day (Miles)', 
        'ucalitems_ljoin_ucisurvey_split'),
      c('ACTIVITY', 'id', 'count', 1, 
        '6_Activity Count per Day', 
        'ucalitems_ljoin_ucisurvey_split'),
      c('TRIP', 'id', 'count', 1, 
        '70_Trip (Segment) Count per Day', 
        'ucalitems_ljoin_ucisurvey_split'),
      c('TRIP', 'id', 'count', 1, 
        '71_Trip (Complete) Count per Day', 
        'leg2trip')
    )
    
    for (item in agg_list) {
      types <- item[1]
      tb <- item[length(item)]
      df_sub <- csv_dict[[tb]] %>%
        filter(type_decoded %in% types)
      
      per_day_item <- df_sub[, .(value = ifelse(item[3]=='sum',
                                                sum(get(item[2])),
                                                length(get(item[2])))),
                             by = c(group_cols)]
      per_day_item[,value:=value * as.numeric(item[4])] 
      
      per_day_item <- merge(per_day_item, 
                            valid_duration[, .(user_id, start_date)], 
                            by = c('user_id', 'start_date'), all = TRUE)
      per_day_item$value[is.na(per_day_item$value)] <- 0
      per_day_item$Statistics <- item[[5]]
      
      per_day[[length(per_day) + 1]] <- per_day_item
    }
    
  } else{
    # Activity Space by person_day----
    valid_duration <- csv_dict$day_summary[, .(user_id, start_date, 
                                               value = no_off * 60)]
    valid_duration$Statistics <- '1_Recorded Data per Day (Minutes)'
    per_day[[length(per_day) + 1]] <- valid_duration
    
    agg_list <- list(
      c('ACTIVITY', 'duration_after_split', 'sum', 60, 
        '2_Total Activity Duration per Day (Minutes)', 
        'ucalitems_ljoin_ucisurvey_split'),
      c('TRIP', 'duration_after_split', 'sum', 60, 
        '3_Total Trip Duration per Day (Minutes)', 
        'ucalitems_ljoin_ucisurvey_split'),
      c('TRIP', 'distance_after_split', 'sum', 0.000621371, 
        '5_Total Trip Distance per Day (Miles)', 
        'ucalitems_ljoin_ucisurvey_split'),
      c('ACTIVITY', 'id', 'count', 1, 
        '6_Activity Count per Day', 
        'ucalitems_ljoin_ucisurvey_split'),
      c('TRIP', 'id', 'count', 1, 
        '70_Trip (Segment) Count per Day', 
        'ucalitems_ljoin_ucisurvey_split'),
      c('TRIP', 'id', 'count', 1, 
        '71_Trip (Complete) Count per Day', 
        'leg2trip')
    )
    for (item in agg_list) {
      types <- item[1]
      tb <- item[length(item)]
      df_sub <- csv_dict[[tb]] %>%
        filter(type_decoded %in% types)
      
      per_day_item <- df_sub[, .(value = ifelse(item[3]=='sum',
                                                sum(get(item[2])),
                                                length(get(item[2])))),
                             by = c(group_cols)]
      per_day_item[,value:=value * as.numeric(item[4])] 
      
      per_day_item <- merge(per_day_item, 
                            valid_duration[, .(user_id, start_date)], 
                            by = c('user_id', 'start_date'), all = TRUE)
      per_day_item$value[is.na(per_day_item$value)] <- 0
      per_day_item$Statistics <- item[[5]]
      
      per_day[[length(per_day) + 1]] <- per_day_item
    }
  }
  
  per_day_df <- rbindlist(per_day)
  
  # Extract data info, DOW
  per_day_df$start_date <- as.Date(per_day_df$start_date)
  #per_day_df$dow_num <- as.integer(format(per_day_df$start_date, '%u'))
  per_day_df$dow <- weekdays(per_day_df$start_date)
  per_day_df$IsWeekend <- (per_day_df$dow=='Saturday')|(per_day_df$dow=='Sunday')
  
  # Summarize the median, mean, std, min, max
  result <- per_day_df[, .(Median = median(value, na.rm = TRUE), 
                           Mean = mean(value, na.rm = TRUE),
                           SD = sd(value, na.rm = TRUE), 
                           Min = min(value, na.rm = TRUE),
                           Max = max(value, na.rm = TRUE)),
                       by = stat_group_cols]
  
  result <- result[order(Statistics)]
  result[1:2,Max:=fifelse(Max>1440,1440,Max)]
  result$Statistics <- gsub(".*_", "", result$Statistics)
  
  if (identical(stat_group_cols, c('Statistics'))) {
    setnames(result, c('Statistics', 'Median', 'Mean', 'SD', 'Min', 'Max'))
  } else if (identical(stat_group_cols, c('IsWeekend', 'Statistics'))) {
    setnames(result, c('IsWeekend', 'Statistics', 'Median', 'Mean', 
                       'SD', 'Min', 'Max'))
  } else {
    stop("Sorry, stat_group_cols parameter not correct")
  }
  
  return(result)
}

get_valid_days <- function(day_summary, trip_num = -1) {
  day_summary <- day_summary[trip_count > trip_num, ]  
  # Filter rows based on trip_count
  
  valid_days_count <- list(
    'All Days' = nrow(day_summary),
    'Weekend' = nrow(subset(day_summary, IsWeekend == "Weekend")),
    'Weekday' = nrow(subset(day_summary, IsWeekend == "Weekday"))
  )
  
  return(valid_days_count)
}

get_summary <- function(df, mytype,valid_days_count, by_field ){
  df <- df %>% filter(type_decoded == mytype)
  
  if(mytype == 'ACTIVITY'){
    df[, duration_after_split := duration_after_split]
  } else{df[, duration_after_split := duration_after_split * 60]}
  
  df[, distance_after_split := distance_after_split / unit_convert]
  
  # Group by and aggregate
  cols <- names(agg_dict[[mytype]])
  
  for (col in cols) {
    fun = agg_dict[[mytype]][col][[1]]
    df[,eval(col):= fun(get(col)),
       by = c('user_id', 'IsWeekend', 'start_date', by_field)]
  }
  
  df <- df[,c(cols,'user_id', 'IsWeekend', 'start_date', by_field),
           with=F] %>% unique()
  
  # Calculate mean for all days of a week, weekends, and weekdays
  result <- list()
  for (key in names(valid_days_count)) {
    df_sub <- copy(df)
    if (key != 'All Days') {
      df_sub <- df_sub[IsWeekend == key]
    }
    value = valid_days_count[key][[1]]
    for (col in cols) {
      df_sub[,eval(col) := as.numeric(get(col))]
      df_sub[,eval(col):= sum(get(col)/value),
             by = c(by_field)]
    }
    
    df_sub <- df_sub[,c(cols,by_field), with=F] %>% unique()
    
    agg_re <- melt(df_sub, id.vars = c(by_field), 
                   variable.name = 'variable', 
                   value.name = 'value')
    agg_re[, day_type := key]
    
    result[[key]] <- agg_re
  }
  
  # Reformat the resulted table
  result_df <- rbindlist(result, fill = TRUE)
  result_df[, variable := factor(variable)]
  result_df[, variable := mapvalues(variable, 
                                    from = col_dict_final[[mytype]]$old,
                                    to = col_dict_final[[mytype]]$new)]
  
  result_df <- result_df[order(variable, day_type)]
  
  return(result_df)
}

activity_trip <- function(ucalitems, day_summary, mytype, 
                          by_field = 'subtype_decoded', trip_num = -1) {
  valid_days_count <- get_valid_days(day_summary, trip_num)
  
  result_df <- get_summary(ucalitems,mytype,valid_days_count,by_field)
  
  # Add a row to calculate the total
  total_row <- cbind(by_field = 'Total', result_df[, .(value = sum(value)), 
                         by = .(variable,day_type)])
  colnames(total_row) [1] <- by_field
  result_df <- rbind(result_df, total_row)
  
  # Pivot the table
  result_df <- dcast(result_df, 
                     formula = formula(paste(by_field,'~ variable + day_type')), 
                     value.var = 'value')
  
  
  # Sort the subtypes by the specified order
  result_df[is.na(result_df)] <- 0
  
  return(result_df)
}

activity_trip_figure <- function(ucalitems, tb, day_summary, mytype, 
                                 by_field = 'subtype_decoded', directory, agg_col,
                                 save_results = F) {
  valid_days_count <- get_valid_days(day_summary)
  
  df <- copy(ucalitems)
  
  if (mytype == 'ACTIVITY' & 
      agg_col == 'duration_after_split' &
      by_field == 'subtype_decoded') {
    df[type_decoded == 'TRIP', subtype_decoded := 'TRIP']
    df[type_decoded == 'TRIP', type_decoded := 'ACTIVITY']
    df[type_decoded == 'DEVICE OFF', subtype_decoded := 'DEVICE OFF']
    df[type_decoded == 'DEVICE OFF', type_decoded := 'ACTIVITY']
    #df[, subtype_decoded := replace(subtype_decoded, subtype_decoded == 'WORK', 'WORKPLACE')]
  } else if (mytype == 'TRIP' & by_field == 'trip_purpose'){
    levels(df$trip_purpose) <- c(levels(df$trip_purpose), 'DEVICE OFF')
    df[, trip_purpose := replace(trip_purpose, trip_purpose == 'UNKNOWN', 'DEVICE OFF')]
  }
  
  
  new_agg_col <- col_dict_final[[mytype]][old==agg_col,'new'][[1]]
  result_df <- get_summary(df,mytype,valid_days_count,by_field)
  result_df <- result_df[variable == new_agg_col]
  
  result_df[,eval(by_field):=factor(get(by_field))]
  legend_order <- rev(levels(result_df[,get(by_field)]))
  
  #if(by_field=='trip_purpose'){
  #  figure_color_map = figure_color_maps[['ACTIVITY']]
  #} else{
  #  figure_color_map = figure_color_maps[[mytype]]
  #}
  
  fig<-plot_ly(data = result_df) %>%
    add_trace(x = ~value, y = ~day_type, 
              type = "bar",
              color = as.formula(paste0("~", by_field))#, 
              #colors = figure_color_map
              ) %>%
    layout(
      barmode = 'stack',
      yaxis = list(
        title=list(text=''),
        tickfont = list(size = 16)),
      xaxis = list(
        title=list(text=new_agg_col,
                   font = list(size = 18)),
        tickfont = list(size = 16)
      ),
      legend = list(
        title = list(text = by_field),
        traceorder = 'normal', 
        items = list(order = legend_order),
        font = list(size = 16)
      ),
      width = 1000, height = 450)
  
  if(save_results){
    if (tb == 'ucalitems_temporal_plot' && mytype == 'TRIP') {
      filename <- file.path(directory, 
                            paste(mytype, "_segment_", new_agg_col, 
                                  "_",by_field,".html", sep = ""))
    } else if (tb == 'leg2trip') {
      filename <- file.path(directory, 
                            paste(mytype, "_complete_", new_agg_col, 
                                  "_",by_field,".html", sep = ""))
    } else {
      filename <- file.path(directory, 
                            paste(mytype, "_", new_agg_col, 
                                  "_",by_field, ".html", sep = ""))
    }
    fig %>% saveWidget(file = filename)
  }

  
  
  
  return(fig)
}
