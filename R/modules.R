#' Run Standard Daynamica Analysis Module
#'
#' Renders the standard Daynamica HTML report summarizing activity, trip, survey, and dataset statistics.
#'
#' @param daynamica_data Preprocessed Daynamica data list dictionary output from filtering.
#' @param output_dir Output directory for the report and exported CSVs.
#' @param file_name Output HTML filename (without extension).
#' @param report_author Author name string for report header.
#' @param report_title Title string for report header.
#' @param export_results Logical indicating whether to export CSV tables alongside HTML.
#' @param user_id_list Vector of user IDs to generate daily visualizations for.
#' @return Output file path string.
#' @export
run_standard_analysis <- function(daynamica_data,
                                  output_dir = "outputs/primary_analysis",
                                  file_name = "primary_analysis_result",
                                  report_author = "Daynamica Team",
                                  report_title = "Daynamica Summary Report",
                                  export_results = TRUE,
                                  user_id_list = NULL) {

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  # render() runs from the template's folder, so relative paths must be resolved here
  output_dir <- normalizePath(output_dir, winslash = "/")

  rmd_template <- system.file("rmd", "standard_daynamica_analysis_module.Rmd", package = "rDaynamica")
  if (rmd_template == "" || !file.exists(rmd_template)) {
    rmd_template <- file.path("inst", "rmd", "standard_daynamica_analysis_module.Rmd")
  }

  if (is.null(user_id_list) && !is.null(daynamica_data$day_summary)) {
    user_id_list <- unique(daynamica_data$day_summary$user_id)[1:min(2, length(unique(daynamica_data$day_summary$user_id)))]
  }

  output_file <- file.path(output_dir, paste0(file_name, ".html"))

  param_list <- list(
    daynamica_data = daynamica_data,
    report_author = report_author,
    report_title = report_title,
    export_results = export_results,
    output_location = output_dir,
    user_id_list = user_id_list
  )

  rmarkdown::render(
    input = rmd_template,
    output_file = output_file,
    params = param_list,
    envir = new.env(parent = globalenv())
  )

  cli::cli_alert_success("Standard analysis report generated at: {output_file}")
  return(output_file)
}

#' Run Calendar Item Visualization Report Module
#'
#' Renders the User Calendar Item Visualization HTML report.
#'
#' @param daynamica_data Preprocessed Daynamica data list dictionary.
#' @param output_dir Output directory for the report.
#' @param file_name Output HTML filename (without extension).
#' @param report_author Author name string.
#' @param report_title Title string.
#' @param user_id_list Vector of user IDs to plot (defaults to first 15).
#' @param limit_tod Optional vector c(min_time, max_time) fraction of day [0,1].
#' @param color_codings Optional list of custom activity and trip color codes.
#' @return Output file path string.
#' @export
run_visualization_report <- function(daynamica_data,
                                     output_dir = "outputs/ucal_visualization_output",
                                     file_name = "ucal_visualization_example",
                                     report_author = "Daynamica Team",
                                     report_title = "Daynamica UCAL Visualization Report",
                                     user_id_list = NULL,
                                     limit_tod = c(0, 1),
                                     color_codings = NULL) {

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  # render() runs from the template's folder, so relative paths must be resolved here
  output_dir <- normalizePath(output_dir, winslash = "/")

  rmd_template <- system.file("rmd", "calendar_item_visualization_module.Rmd", package = "rDaynamica")
  if (rmd_template == "" || !file.exists(rmd_template)) {
    rmd_template <- file.path("inst", "rmd", "calendar_item_visualization_module.Rmd")
  }

  if (is.null(user_id_list) && !is.null(daynamica_data$day_summary)) {
    user_id_list <- head(unique(daynamica_data$day_summary$user_id), 15)
  }

  output_file <- file.path(output_dir, paste0(file_name, ".html"))

  param_list <- list(
    daynamica_data = daynamica_data,
    report_author = report_author,
    report_title = report_title,
    user_id_list = user_id_list,
    limit_tod = limit_tod,
    color_codings = color_codings
  )

  rmarkdown::render(
    input = rmd_template,
    output_file = output_file,
    params = param_list,
    envir = new.env(parent = globalenv())
  )

  cli::cli_alert_success("Visualization report generated at: {output_file}")
  return(output_file)
}

#' Generate Chord Diagram GIF Visualization
#'
#' Processes sequence data and generates a chord diagram GIF showcasing state flow rates over time.
#'
#' @param daynamica_data Preprocessed Daynamica data dictionary.
#' @param output_dir Directory where output GIF and frame PNGs are saved.
#' @param gif_filename Output GIF filename.
#' @param unit_length Sequence unit length in minutes (default 5).
#' @param split_size Flow calculation step size (default 3 = 15 minutes).
#' @return Output GIF file path string.
#' @export
generate_chord_diagram_gif <- function(daynamica_data,
                                       output_dir = "outputs/plot-gif",
                                       gif_filename = "ucal_chord_diagram_visualization_all_data_visualization.gif",
                                       unit_length = 5,
                                       split_size = 3) {

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  # render() runs from the template's folder, so relative paths must be resolved here
  output_dir <- normalizePath(output_dir, winslash = "/")

  frame_dir <- file.path(output_dir, "frames")
  if (!dir.exists(frame_dir)) {
    dir.create(frame_dir, recursive = TRUE)
  }

  seq_data <- generate_sequence_data(daynamica_data$ucalitems_temporal_plot, unit_length = unit_length)
  vector_length <- length(seq_data$id_key$letters_subtype)
  chord_palette <- c("#80B1D3", "#BC80BD", "#FB8072", "#FCCDE5", "#FDB462", "#B3DE69", "#FFFFB3",
                     "#8DD3C7", "#BEBADA", "#D9D9D9", "#CCEBC5", "#FFED6F")
  if (vector_length > length(chord_palette)) {
    cli::cli_abort(c(
      "The chord diagram supports at most {length(chord_palette)} subtype categories, but the data has {vector_length}.",
      "i" = "Use {.fn recategorize_calendar_items} to group subtypes into fewer categories."
    ))
  }

  category_data <- data.frame(
    label = seq_data$id_key$letters_subtype,
    subtype = seq_data$id_key$subtype_decoded,
    order = seq_len(vector_length),
    color = chord_palette[seq_len(vector_length)],
    stringsAsFactors = FALSE
  )

  flow_data <- flow_rate_calculator(seq_data$wide_sequence_data, split_size = split_size) %>%
    process_flow_data(category_data = category_data)

  create_individual_plots(
    data_list = list(flow_data),
    category_data = category_data,
    data_names = "All Data",
    location = paste0(frame_dir, "/"),
    nrow = 1, ncol = 1, width = 7, height = 7
  )

  png_files <- list.files(path = frame_dir, pattern = "\\.png$", full.names = TRUE)
  gif_file <- file.path(output_dir, gif_filename)

  gifski::gifski(png_files, gif_file, width = 800, height = 800, delay = 1.25)

  cli::cli_alert_success("Chord diagram GIF generated at: {gif_file}")
  return(gif_file)
}

#' Run Activity Chains Analysis Module
#'
#' Renders the Activity Chains tutorial and summary HTML report.
#'
#' @param daynamica_data Preprocessed Daynamica dataset dictionary.
#' @param output_dir Directory where output report is saved.
#' @param file_name Output HTML filename (without extension).
#' @param report_author Author name string.
#' @param report_title Title string.
#' @return Output file path string.
#' @export
run_activity_chains_report <- function(daynamica_data,
                                       output_dir = "outputs/Module_4",
                                       file_name = "Activity_Chains_Module_Example",
                                       report_author = "Daynamica Team",
                                       report_title = "Daynamica Activity Chains Example") {

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  # render() runs from the template's folder, so relative paths must be resolved here
  output_dir <- normalizePath(output_dir, winslash = "/")

  rmd_template <- system.file("rmd", "activity_chains_module.Rmd", package = "rDaynamica")
  if (rmd_template == "" || !file.exists(rmd_template)) {
    rmd_template <- file.path("inst", "rmd", "activity_chains_module.Rmd")
  }

  output_file <- file.path(output_dir, paste0(file_name, ".html"))

  param_list <- list(
    daynamica_data = daynamica_data,
    report_author = report_author,
    report_title = report_title
  )

  rmarkdown::render(
    input = rmd_template,
    output_file = output_file,
    params = param_list,
    envir = new.env(parent = globalenv())
  )

  cli::cli_alert_success("Activity Chains report generated at: {output_file}")
  return(output_file)
}
