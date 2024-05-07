# Configuration for plots
# 1. colors for activity and trip types----
color_discrete_maps <- c(
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
  
  #trips
  'CAR - DRIVER' = '#fdbf6f',
  'CAR - PASSENGER' = '#ff7f00',
  'VEHICLE' = '#b15928',
  'TAXI/UBER/LYFT' = '#fb9a99',
  'RAIL' = '#1f78b4',
  'BUS' = '#a6cee3',
  'BIKE' = '#33a02c',
  'WALK' = '#b2df8a',
  'WAIT' = '#6a3d9a',
  'OTHER TRIPS' = '#d9d9d9',
  
  #device off
  'UNKNOWN' = 'white'
)

# 2. height of rectangulars in the temporal plots----
heigh_dicts <- c(
  'ACTIVITY' = 0.65,
  'TRIP' = 0.85,
  'DEVICE OFF' = 0.65
)

# Define the plot_indi_temp function-----
plot_indi_temp <- function(ucalitems_temporal_plot,
                           UserId, 
                           color_discrete_map = color_discrete_maps,
                           heigh_dict = heigh_dicts,
                           save_figure = F,
                           directory) {
  # Select the valid episodes for the user and sort by time
  df <- ucalitems_temporal_plot[user_id == UserId]
  setorder(df, start_dt)
  
  # Check if the number of selected episodes > 0
  if (nrow(df) > 0) {
    # Format the date string to show on the plots
    df[, Date := paste(start_date, '<br>', dow)]
    df[, end_time := fifelse(end_time=='00:00:00','23:59:00',end_time)]
    setorder(df, Date)
    
    
    fig <- plot_ly(width = 1000,
                   height = 300 + 50 * df$Date %>% unique() %>% length())%>%
      add_segments(data = df[type_decoded=='ACTIVITY']%>% group_by(Date),
                   x = ~as.POSIXct(start_time, format = "%H:%M"),
                   xend = ~as.POSIXct(end_time, format = "%H:%M"),
                   y = ~Date,
                   yend = ~Date,
                   color = ~subtype_decoded, 
                   colors = color_discrete_map,
                   type = "scatter",
                   mode = "lines",
                   line = ~list(width = 50 * heigh_dict[type_decoded]),
                   legendgroup=~type_decoded) %>%
      add_segments(data = df[type_decoded=='TRIP']%>% group_by(Date),
                   x = ~as.POSIXct(start_time, format = "%H:%M"),
                   xend = ~as.POSIXct(end_time, format = "%H:%M"),
                   y = ~Date,
                   yend = ~Date,
                   color = ~subtype_decoded, 
                   colors = color_discrete_map,
                   type = "scatter",
                   mode = "lines",
                   line = ~list(width = 50 * heigh_dict[type_decoded]),
                   legendgroup=~type_decoded) %>%
      add_segments(data = df[type_decoded=='DEVICE OFF']%>% group_by(Date),
                   x = ~as.POSIXct(start_time, format = "%H:%M"),
                   xend = ~as.POSIXct(end_time, format = "%H:%M"),
                   y = ~Date,
                   yend = ~Date,
                   color = ~subtype_decoded, 
                   colors = color_discrete_map,
                   type = "scatter",
                   mode = "lines",
                   line =~list(width = 50 * heigh_dict[type_decoded]),
                   legendgroup=~type_decoded) %>%
      layout(
        xaxis = list(
          title = 'Time',
          tickformat = '%H:%M',
          tickvals = seq.POSIXt(as.POSIXct("00:00",format="%H:%M"),
                                as.POSIXct("00:00",format="%H:%M") + 3600, 
                                by = '4 hours')),
        yaxis = list(autorange = "reversed"),
        font=list(
          family="Arial",
          size = 18,  # Set the font size here
          color = "#000000")
      ) 
    if(save_figure){
      # Save plot as an HTML file
      html_file <- paste(directory, "/", UserId, ".html", sep = "")
      fig %>% saveWidget(file = html_file)
      cat(UserId, nrow(df), "\n")
    }
    return(fig)
  }
}




generate_time_of_day_plot <- function(data_subset, 
                                      color_codes_activity = color_codes_activity_data,
                                      color_codes_trips = color_codes_trips_data,
                                      min_time = 0,
                                      max_time = 1){
  plotting_data <- 
    data_subset %>%
    tibble() %>%
    mutate(start_time_frac = (hour(hms(start_time))*60 + minute(hms(start_time)) + second(hms(start_time))/60)/1440,
           end_time_frac = (hour(hms(end_time))*60 + minute(hms(end_time)) + second(hms(end_time))/60)/1440,
           end_time_frac = ifelse(end_time_frac == 0, 1, end_time_frac),
           date_label = paste0(ymd(start_date),"\n",  weekdays(ymd(start_date))),
           start_time_frac = ifelse(start_time_frac < min_time, min_time, start_time_frac),
           start_time_frac = ifelse(start_time_frac > max_time, max_time, start_time_frac),
           end_time_frac = ifelse(end_time_frac < min_time, min_time, end_time_frac),
           end_time_frac = ifelse(end_time_frac > max_time, max_time, end_time_frac))
  
  time_frac <- c(0, 4, 8, 12, 16, 20, 24)/24
  label_frac <- c("12:00 AM",
                  "4:00 AM",
                  "8:00 AM",
                  "12:00 PM",
                  "4:00 PM",
                  "8:00 PM",
                  "12:00 PM")
  
  scale_x_continuous(breaks = time_frac[ which(time_frac >= min_time & time_frac <= max_time)],
                     labels = label_frac[ which(time_frac >= min_time & time_frac <= max_time)])
  
  
  p <- plotting_data %>%
    filter(type_decoded == "ACTIVITY") %>%
    #filter(date_label == '2022-11-13\nSunday') %>%
    ggplot() +
    geom_rect(aes(xmin = start_time_frac,
                  xmax = end_time_frac,
                  ymin = 1,
                  ymax = 0,
                  fill = subtype_decoded), color = "black", alpha = 0.4) + 
    scale_fill_manual(limits = color_codes_activity$labels, values = color_codes_activity$values) +
    #scale_fill_brewer(palette = "Pastel2") +
    labs(fill = "Activity Type") +
    guides(fill = guide_legend(ncol = 2, override.aes = list(alpha = 0.4))) +
    new_scale_fill()
  
  if(nrow(plotting_data %>% filter(type_decoded == "TRIP")) != 0){
    p <- p +     geom_rect(data = plotting_data %>%
                             filter(type_decoded == "TRIP"),
                           aes(xmin = start_time_frac,
                               xmax = end_time_frac,
                               ymin = 0.75,
                               ymax = 0.25,
                               fill = subtype_decoded,
                               #text = text,
                               #linetype = confirmed_indicator
                           ),color = "black", alpha = 1) +
      scale_fill_brewer(palette = "Reds") +
      labs(fill = "Trip Type")
  }
 p +
    guides(fill = guide_legend(ncol = 2)) + 
    geom_vline(data = data.frame(intercept =c(4, 8, 12, 16, 20)/24),
               aes(xintercept = intercept), color = "#797a7a") + 
    facet_grid(date_label ~ ., switch = "y") +
    theme_minimal() +
    #theme_bw() +
    theme(axis.text.y = element_blank(),
          panel.spacing.y = unit(0, "lines"),
          strip.text.y.left = element_text(angle = 0, size = 14),
          axis.text.x = element_text(size = 14),
          axis.title.x = element_text(size = 16),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"))+
    ylab("") +
    xlab("Time of Day") +
    labs(fill = "Travel Type") +
    guides(fill = guide_legend(ncol = 2)) +
   scale_x_continuous(breaks = time_frac[which(time_frac >= min_time & time_frac <= max_time)],
                      labels = label_frac[which(time_frac >= min_time & time_frac <= max_time)]) +
    #scale_fill_brewer(palette = "Set1")
    scale_fill_manual(limits = color_codes_trips$labels, values = color_codes_trips$values) + 
    theme(legend.position = "bottom")%>%
    return()
}

