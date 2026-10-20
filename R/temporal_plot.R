#' Default Discrete Color Map for Daynamica Plotting
#' @export
color_discrete_maps <- c(
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
  'UNKNOWN' = 'white'
)

#' Default Segment Heights for Daynamica Plotting
#' @export
heigh_dicts <- c(
  'ACTIVITY' = 0.65,
  'TRIP' = 0.85,
  'DEVICE OFF' = 0.65
)

#' Plot Individual Temporal Calendar Items using Plotly
#'
#' @param ucalitems_temporal_plot Data table prepared for temporal plotting.
#' @param UserId Target user ID string.
#' @param color_discrete_map Color mapping named vector.
#' @param heigh_dict Height mapping vector.
#' @param save_figure Logical indicating whether to save HTML output.
#' @param directory Output directory for saved HTML plots.
#' @return Plotly object.
#' @export
plot_indi_temp <- function(ucalitems_temporal_plot,
                           UserId,
                           color_discrete_map = color_discrete_maps,
                           heigh_dict = heigh_dicts,
                           save_figure = FALSE,
                           directory = NULL) {
  df <- ucalitems_temporal_plot[user_id == UserId]
  data.table::setorder(df, start_dt)

  if (nrow(df) > 0) {
    df[, Date := paste(start_date, '<br>', dow)]
    df[, end_time := data.table::fifelse(end_time == '00:00:00', '23:59:00', end_time)]
    data.table::setorder(df, Date)

    fig <- plotly::plot_ly(width = 1000, height = 300 + 50 * length(unique(df$Date))) %>%
      plotly::add_segments(data = df[type_decoded == 'ACTIVITY'] %>% dplyr::group_by(Date),
                           x = ~as.POSIXct(start_time, format = "%H:%M"),
                           xend = ~as.POSIXct(end_time, format = "%H:%M"),
                           y = ~Date, yend = ~Date,
                           color = ~subtype_decoded, colors = color_discrete_map,
                           type = "scatter", mode = "lines",
                           line = ~list(width = 50 * heigh_dict[type_decoded]),
                           legendgroup = ~type_decoded) %>%
      plotly::add_segments(data = df[type_decoded == 'TRIP'] %>% dplyr::group_by(Date),
                           x = ~as.POSIXct(start_time, format = "%H:%M"),
                           xend = ~as.POSIXct(end_time, format = "%H:%M"),
                           y = ~Date, yend = ~Date,
                           color = ~subtype_decoded, colors = color_discrete_map,
                           type = "scatter", mode = "lines",
                           line = ~list(width = 50 * heigh_dict[type_decoded]),
                           legendgroup = ~type_decoded) %>%
      plotly::add_segments(data = df[type_decoded == 'DEVICE OFF'] %>% dplyr::group_by(Date),
                           x = ~as.POSIXct(start_time, format = "%H:%M"),
                           xend = ~as.POSIXct(end_time, format = "%H:%M"),
                           y = ~Date, yend = ~Date,
                           color = ~subtype_decoded, colors = color_discrete_map,
                           type = "scatter", mode = "lines",
                           line = ~list(width = 50 * heigh_dict[type_decoded]),
                           legendgroup = ~type_decoded) %>%
      plotly::layout(
        xaxis = list(
          title = 'Time',
          tickformat = '%H:%M',
          tickvals = seq.POSIXt(as.POSIXct("00:00", format = "%H:%M"),
                                as.POSIXct("00:00", format = "%H:%M") + 3600,
                                by = '4 hours')),
        yaxis = list(autorange = "reversed"),
        font = list(family = "Arial", size = 18, color = "#000000")
      )

    if (save_figure && !is.null(directory)) {
      if (!dir.exists(directory)) dir.create(directory, recursive = TRUE)
      html_file <- file.path(directory, paste0(UserId, ".html"))
      htmlwidgets::saveWidget(fig, file = html_file)
    }
    return(fig)
  }
}

#' Generate Time of Day ggplot Visualization
#'
#' @param data_subset Subset of calendar items for a given user/period.
#' @param color_codes_activity Data frame of activity color codes.
#' @param color_codes_trips Data frame of trip color codes.
#' @param min_time Minimum time fraction of day [0,1].
#' @param max_time Maximum time fraction of day [0,1].
#' @return ggplot object.
#' @export
generate_time_of_day_plot <- function(data_subset,
                                      color_codes_activity,
                                      color_codes_trips,
                                      min_time = 0,
                                      max_time = 1) {
  plotting_data <- data_subset %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      start_time_frac = (lubridate::hour(lubridate::hms(start_time)) * 60 + lubridate::minute(lubridate::hms(start_time)) + lubridate::second(lubridate::hms(start_time)) / 60) / 1440,
      end_time_frac = (lubridate::hour(lubridate::hms(end_time)) * 60 + lubridate::minute(lubridate::hms(end_time)) + lubridate::second(lubridate::hms(end_time)) / 60) / 1440,
      end_time_frac = ifelse(end_time_frac == 0, 1, end_time_frac),
      date_label = paste0(lubridate::ymd(start_date), "\n", weekdays(lubridate::ymd(start_date))),
      start_time_frac = ifelse(start_time_frac < min_time, min_time, start_time_frac),
      start_time_frac = ifelse(start_time_frac > max_time, max_time, start_time_frac),
      end_time_frac = ifelse(end_time_frac < min_time, min_time, end_time_frac),
      end_time_frac = ifelse(end_time_frac > max_time, max_time, end_time_frac)
    )

  time_frac <- c(0, 4, 8, 12, 16, 20, 24) / 24
  label_frac <- c("12:00 AM", "4:00 AM", "8:00 AM", "12:00 PM", "4:00 PM", "8:00 PM", "12:00 PM")

  p <- plotting_data %>%
    dplyr::filter(type_decoded == "ACTIVITY") %>%
    ggplot2::ggplot() +
    ggplot2::geom_rect(ggplot2::aes(xmin = start_time_frac, xmax = end_time_frac, ymin = 1, ymax = 0, fill = subtype_decoded), color = "black", alpha = 0.4) +
    ggplot2::scale_fill_manual(limits = color_codes_activity$labels, values = color_codes_activity$values) +
    ggplot2::labs(fill = "Activity Type") +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 2, override.aes = list(alpha = 0.4))) +
    ggnewscale::new_scale_fill()

  if (nrow(plotting_data %>% dplyr::filter(type_decoded == "TRIP")) != 0) {
    p <- p + ggplot2::geom_rect(
      data = plotting_data %>% dplyr::filter(type_decoded == "TRIP"),
      ggplot2::aes(xmin = start_time_frac, xmax = end_time_frac, ymin = 0.75, ymax = 0.25, fill = subtype_decoded),
      color = "black", alpha = 1
    ) +
      ggplot2::scale_fill_brewer(palette = "Reds") +
      ggplot2::labs(fill = "Trip Type")
  }

  p <- p +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 2)) +
    ggplot2::geom_vline(data = data.frame(intercept = c(4, 8, 12, 16, 20) / 24), ggplot2::aes(xintercept = intercept), color = "#797a7a") +
    ggplot2::facet_grid(date_label ~ ., switch = "y") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      panel.spacing.y = ggplot2::unit(0, "lines"),
      strip.text.y.left = ggplot2::element_text(angle = 0, size = 14),
      axis.text.x = ggplot2::element_text(size = 14),
      axis.title.x = ggplot2::element_text(size = 16),
      legend.text = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_text(size = 14),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white"),
      plot.background = ggplot2::element_rect(fill = "white")
    ) +
    ggplot2::ylab("") +
    ggplot2::xlab("Time of Day") +
    ggplot2::scale_x_continuous(
      breaks = time_frac[which(time_frac >= min_time & time_frac <= max_time)],
      labels = label_frac[which(time_frac >= min_time & time_frac <= max_time)]
    ) +
    ggplot2::scale_fill_manual(limits = color_codes_trips$labels, values = color_codes_trips$values) +
    ggplot2::theme(legend.position = "bottom")

  return(p)
}
