# Function to format valid days
format_valid_days <- function(df, sheetname, wb) {
  addWorksheet(wb, sheetname)
  
  hs1 <- createStyle(halign = "CENTER", valign = "center",
                     textDecoration = "Bold")
  
  #headings
  writeData(wb, sheetname, df, startRow = 1, startCol = 1, 
            headerStyle = hs1)
  
  addStyle(wb, sheet = sheetname, rows = 1:nrow(df)+1, cols = 1,
           style = hs1)
  
  #all the columns
  xlsxStyle  <- createStyle(halign = "center", valign = "center")
  addStyle(wb, sheet = sheetname, rows = 1:nrow(df)+1, cols = 2:length(df),
           style = xlsxStyle, gridExpand = T)
  
  setColWidths(wb, sheet = sheetname, cols = 1:ncol(df), widths = 15)
}

format_daily_statistics <- function(df, sheetname, wb) {
  addWorksheet(wb, sheetname)
  
  hs1 <- createStyle(halign = "CENTER", valign = "center",
                     textDecoration = "Bold")
  
  #headings
  writeData(wb, sheetname, df, startRow = 1, startCol = 1, 
            headerStyle = hs1)
  
  addStyle(wb, sheet = sheetname, rows = 1:nrow(df)+1, cols = 1,
           style = hs1)
  
  #all the columns
  xlsxStyle  <- createStyle(halign = "center", valign = "center")
  addStyle(wb, sheet = sheetname, rows = 1:nrow(df)+1, cols = 2:length(df),
           style = xlsxStyle, gridExpand = T)
  
  setColWidths(wb, sheet = sheetname, cols = 2:ncol(df), widths = 15)
  setColWidths(wb, sheet = sheetname, cols = 1, widths = 40)
}

# Function to format subtype
format_subtype <- function(df, sheetname, wb) {
  addWorksheet(wb, sheetname)
  
  hs1 <- createStyle(halign = "CENTER", valign = "center",
                     textDecoration = "Bold")
  
  #headings
  d <- matrix(NA, nrow = 3, ncol = ncol(df))
  d[1,] <- c(NA,gsub("_.*", "", colnames(df)[-1]))
  d[2,] <- c('Day Type',gsub(".*_", "", colnames(df)[-1]))
  d[3,1] <- 'Subtype'
  d <- data.table(d)

  writeData(wb, sheetname, df, startRow = 1, startCol = 1, 
            headerStyle = hs1,colNames = FALSE)
  
  writeData(wb, sheetname, d, startCol = 1, startRow = 1, 
            rowNames = FALSE, colNames = FALSE)
  
  addStyle(wb, sheet = sheetname, rows = 1:3, cols = 1:ncol(df),
           style = hs1, gridExpand = T)
  
  addStyle(wb, sheet = sheetname, rows = 1:nrow(df)+3, cols = 1,
           style = hs1)
  
  #merge heading in 1st row
  for (i in 1:((ncol(df)-1)/3)){
    mergeCells(wb, sheetname, cols = ((i-1)*3+2):(i*3+1), rows = 1)
  }
  
  #all the columns
  xlsxStyle  <- createStyle(halign = "center", valign = "center")
  addStyle(wb, sheet = sheetname, rows = 3:nrow(df)+1, cols = 2:length(df),
           style = xlsxStyle, gridExpand = T)
  
  setColWidths(wb, sheet = sheetname, cols = 1:ncol(df), widths = 15)
}

save_tables_plots <- function(directory, csv_dict, csv_dict_sub) {
  print(dim(csv_dict[['ucalitems_ljoin_ucisurvey_split']]))
  print(dim(csv_dict_sub[['ucalitems_ljoin_ucisurvey_split']]))
        
  
  # Summary tables
  subtype_confirmed_hours <- count_valid_per_days(
    day_summary = csv_dict[['day_summary']],
    numerator_col = 'with_subtype',
    denominator_filter = 'interact_by_confirm>0'
  )
  
  survey_answered_hours <- count_valid_per_days(
    day_summary = csv_dict[['day_summary']],
    numerator_col = 'with_survey',
    denominator_filter = '(interact_by_confirm>0)&(with_subtype>=(12-0.1))'
  )
  
  daily_summary <- overview_statistics(
    csv_dict = csv_dict_sub,
    stat_group_cols = c('Statistics')
  )
  
  activity <- activity_trip(
    ucalitems = csv_dict_sub[['ucalitems_temporal_plot']],
    day_summary = csv_dict_sub[['day_summary']],
    mytype = 'ACTIVITY',
    trip_num = -1
  )
  
  trip <- activity_trip(
    ucalitems = csv_dict_sub[['ucalitems_temporal_plot']],
    day_summary = csv_dict_sub[['day_summary']],
    mytype = 'TRIP',
    trip_num = -1
  )
  
  complete_trip <- activity_trip(
    ucalitems = csv_dict_sub[['leg2trip']],
    day_summary = csv_dict_sub[['day_summary']],
    mytype = 'TRIP',
    trip_num = -1
  )
  
  trip_purpose <- activity_trip(
    ucalitems = csv_dict_sub[['ucalitems_temporal_plot']],
    day_summary = csv_dict_sub[['day_summary']],
    mytype = 'TRIP',
    by_field = 'trip_purpose',
    trip_num = -1
  )
  
  complete_trip_purpose <- activity_trip(
    ucalitems = csv_dict_sub[['leg2trip']],
    day_summary = csv_dict_sub[['day_summary']],
    mytype = 'TRIP',
    by_field = 'trip_purpose',
    trip_num = -1
  )
  
  # Write to Excel and format
  wb <- createWorkbook()
  
  format_valid_days(subtype_confirmed_hours, 'subtype_confirmed_hours', wb)
  format_valid_days(survey_answered_hours, 'survey_answered_hours', wb)
  format_daily_statistics(daily_summary, 'daily_summary', wb)
  format_subtype(activity, 'activity_subtype', wb)
  format_subtype(trip, 'trip_segment_subtype', wb)
  format_subtype(complete_trip, 'trip_complete_subtype', wb)
  format_subtype(trip_purpose, 'trip_segment_purpose', wb)
  format_subtype(complete_trip_purpose, 'trip_complete_purpose', wb)
  
  saveWorkbook(wb,file.path(directory,'summary_tables.xlsx'),overwrite = T)
  
  # Figures
  activity_trip_figure(
    ucalitems = csv_dict_sub[['ucalitems_temporal_plot']],
    tb = 'ucalitems_temporal_plot',
    day_summary = csv_dict_sub[['day_summary']],
    mytype = 'ACTIVITY',
    directory = directory,
    agg_col = 'duration_after_split'
  )
  
  activity_trip_figure(
    ucalitems = csv_dict_sub[['ucalitems_temporal_plot']],
    tb = 'ucalitems_temporal_plot',
    day_summary = csv_dict_sub[['day_summary']],
    mytype = 'TRIP',
    directory = directory,
    agg_col = 'id'
  )
  
  activity_trip_figure(
    ucalitems = csv_dict_sub[['ucalitems_temporal_plot']],
    tb = 'ucalitems_temporal_plot',
    day_summary = csv_dict_sub[['day_summary']],
    mytype = 'TRIP',
    directory = directory,
    agg_col = 'duration_after_split'
  )
  
  activity_trip_figure(
    ucalitems = csv_dict_sub[['ucalitems_temporal_plot']],
    tb = 'ucalitems_temporal_plot',
    day_summary = csv_dict_sub[['day_summary']],
    mytype = 'TRIP',
    directory = directory,
    agg_col = 'distance_after_split'
  )
  
  activity_trip_figure(
    ucalitems = csv_dict_sub[['leg2trip']],
    tb = 'leg2trip',
    day_summary = csv_dict_sub[['day_summary']],
    mytype = 'TRIP',
    directory = directory,
    agg_col = 'id'
  )
  
  activity_trip_figure(
    ucalitems = csv_dict_sub[['leg2trip']],
    tb = 'leg2trip',
    day_summary = csv_dict_sub[['day_summary']],
    mytype = 'TRIP',
    directory = directory,
    agg_col = 'duration_after_split'
  )
  
  activity_trip_figure(
    ucalitems = csv_dict_sub[['leg2trip']],
    tb = 'leg2trip',
    day_summary = csv_dict_sub[['day_summary']],
    mytype = 'TRIP',
    directory = directory,
    agg_col = 'distance_after_split'
  )
  
  activity_trip_figure(
    ucalitems = csv_dict_sub[['leg2trip']],
    tb = 'leg2trip',
    day_summary = csv_dict_sub[['day_summary']],
    mytype = 'TRIP',
    by_field = 'trip_purpose',
    directory = directory,
    agg_col = 'id'
  )
  
  activity_trip_figure(
    ucalitems = csv_dict_sub[['leg2trip']],
    tb = 'leg2trip',
    day_summary = csv_dict_sub[['day_summary']],
    mytype = 'TRIP',
    by_field = 'trip_purpose',
    directory = directory,
    agg_col = 'duration_after_split'
  )
  
  activity_trip_figure(
    ucalitems = csv_dict_sub[['leg2trip']],
    tb = 'leg2trip',
    day_summary = csv_dict_sub[['day_summary']],
    mytype = 'TRIP',
    by_field = 'trip_purpose',
    directory = directory,
    agg_col = 'distance_after_split'
  )
}
