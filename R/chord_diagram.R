#' Calculate Flow Rate Transitions Between Discrete Time Units
#'
#' @param data Sequence data frame or table.
#' @param split_size Number of time units to step forward when computing transitions.
#' @param data_subset Optional logical vector to filter input rows.
#' @return Data frame of flow rates between states.
#' @export
flow_rate_calculator <- function(data, split_size, data_subset = NULL) {
  if (split_size > 24 || split_size < 1 || 288 %% split_size != 0) {
    stop("Please choose a valid split size.")
  }

  if (!is.null(data_subset)) {
    data <- data[data_subset, ]
  }

  table_col_names <- sort(as.numeric(names(data)[-c(1, 2)]))
  indices <- seq(min(table_col_names), max(table_col_names), by = split_size)

  current_time <- indices[-length(indices)]
  next_time <- indices[-1]

  flow_rate_data <- data.frame(time = numeric(), next_time = numeric(), starting_state = character(), ending_state = character(), flow_rate = numeric())

  data_seq <- data[, -c(1, 2)]
  data_seq <- data_seq[, order(as.numeric(names(data_seq)))]

  for (i in seq_along(current_time)) {
    cur_col <- current_time[i]
    nxt_col <- next_time[i]

    df_step <- data.frame(
      time = cur_col,
      next_time = nxt_col,
      starting_state = data_seq[[cur_col]],
      ending_state = data_seq[[nxt_col]],
      stringsAsFactors = FALSE
    ) %>%
      dplyr::group_by(time, next_time, starting_state, ending_state) %>%
      dplyr::summarize(flow_rate = dplyr::n(), .groups = "drop")

    flow_rate_data <- dplyr::bind_rows(flow_rate_data, df_step)
  }

  last_idx <- length(current_time)
  df_eod <- data.frame(
    time = next_time[last_idx],
    next_time = current_time[1],
    starting_state = data_seq[[next_time[last_idx]]],
    ending_state = data_seq[[current_time[1]]],
    stringsAsFactors = FALSE
  ) %>%
    dplyr::group_by(time, next_time, starting_state, ending_state) %>%
    dplyr::summarize(flow_rate = dplyr::n(), .groups = "drop")

  flow_rate_data <- dplyr::bind_rows(flow_rate_data, df_eod)
  return(flow_rate_data)
}

#' Process Flow Data with Category Formatting
#'
#' @param dataset Flow rate data frame output from `flow_rate_calculator`.
#' @param category_data Data frame containing category labels, colors, and order.
#' @return Formatted flow data frame ready for chord diagram plotting.
#' @export
process_flow_data <- function(dataset, category_data = NULL) {
  if (is.null(category_data)) {
    stop("category_data parameter must be provided.")
  }

  dataset %>%
    dplyr::mutate(
      starting_state = factor(starting_state, levels = category_data$label, labels = category_data$subtype),
      ending_state = factor(ending_state, levels = category_data$label, labels = category_data$subtype)
    ) %>%
    dplyr::mutate(.frame = as.numeric(factor(time, levels = sort(unique(time)), labels = seq_along(unique(time))))) %>%
    dplyr::left_join(category_data, by = c("starting_state" = "subtype")) %>%
    dplyr::filter(!is.na(starting_state) & !is.na(ending_state)) %>%
    dplyr::select(starting_state, ending_state, flow_rate, order, color, .frame, time)
}

#' Create Individual Chord Diagram Plot PNG Files
#'
#' @param data_list List of flow rate data frames.
#' @param category_data Category mapping metadata.
#' @param data_names Vector of title labels for each plot in `data_list`.
#' @param location Target directory for saving PNG files.
#' @param nrow Number of subplot rows.
#' @param ncol Number of subplot columns.
#' @param width Plot width in inches.
#' @param height Plot height in inches.
#' @return Output status string.
#' @export
create_individual_plots <- function(data_list, category_data, data_names = NULL,
                                    location = "./outputs/plot-gif/",
                                    nrow = 1,
                                    ncol = 1,
                                    width = 7,
                                    height = 7) {
  if (!dir.exists(location)) dir.create(location, recursive = TRUE)

  for (f in unique(data_list[[1]]$.frame)) {
    if (f %% 2 == 1 || f < 15) next

    time_of_day <- paste0(ifelse(f < 52, floor(f / 4), floor(f / 4) - 12), ":", f %% 4 * 15, ifelse(f < 48, " AM", " PM"))
    if (f < 4) time_of_day <- paste0(12, ":", f %% 4 * 15, ifelse(f < 48, " AM", " PM"))
    time_of_day <- gsub(":0 ", ":00 ", time_of_day)

    # Format zero-padded frame number for proper alphabetical sorting by gifski
    frame_str <- sprintf("%04d", f)
    png_path <- file.path(location, paste0("globalchord_", frame_str, ".png"))
    grDevices::png(file = png_path, height = height, width = width, units = "in", res = 300)
    graphics::par(mfrow = c(nrow, ncol))

    for (i in seq_along(data_list)) {
      tweened_data <- data_list[[i]]
      circlize::circos.clear()
      graphics::par(mar = rep(0, 4), cex = 1)
      circlize::circos.par(start.degree = 90, track.margin = c(-0.1, 0.1), gap.after = 10, points.overflow.warning = FALSE, clock.wise = FALSE)

      temp_data <- dplyr::filter(tweened_data, .frame == f)
      color_vec_names <- unique(temp_data$starting_state)
      temp <- dplyr::filter(category_data, subtype %in% color_vec_names)
      color_vec <- temp$color
      names(color_vec) <- temp$subtype

      suppressMessages(circlize::chordDiagram(
        x = temp_data, directional = 1,
        order = category_data$subtype,
        grid.col = color_vec,
        annotationTrack = "grid",
        transparency = 0.25, annotationTrackHeight = c(0.05, 0.12),
        direction.type = c("diffHeight", "arrows"), link.arr.type = "big.arrow",
        diffHeight = -0.04, link.sort = TRUE, link.largest.ontop = TRUE
      ))

      circlize::circos.track(track.index = 1, bg.border = NA, panel.fun = function(x, y) {
        xlim <- circlize::get.cell.meta.data("xlim")
        sector.index <- circlize::get.cell.meta.data("sector.index")
        reg1 <- category_data %>% dplyr::filter(subtype == sector.index) %>% dplyr::pull(subtype)

        circlize::circos.text(x = mean(xlim), y = 4, labels = reg1, facing = "bending", cex = 1)
        circlize::circos.axis(h = "top", labels.cex = 0.6, labels.niceFacing = FALSE, labels.pos.adjust = FALSE)
      })

      if (!is.null(data_names)) {
        graphics::text(x = 0.85, y = 1, labels = data_names[i], cex = 1.5)
      }

      graphics::text(x = -0.85, y = 1.0, labels = "Time:", cex = 1.3)
      graphics::text(x = -0.85, y = 0.88, labels = time_of_day, cex = 1.3)
    }
    grDevices::dev.off()
  }
  return(paste0("Plots have been generated at: ", location))
}
