#' Read Daynamica CSV Files into a Dictionary List
#'
#' @param folder_path Path to the folder containing Daynamica CSV files.
#' @param file_prefix Prefix string to strip from filenames when creating dictionary keys.
#' @param file_suffix Optional suffix string to strip from filenames when creating dictionary keys.
#' @return A named list of data.tables representing each imported CSV file.
#' @export
path2dict <- function(folder_path, file_prefix, file_suffix = NULL) {
  csv_dict <- list()

  if (file.exists(folder_path)) {
    cli::cli_alert_info("{folder_path} folder exists...")
    cli::cli_alert_info("Reading data into list...")

    filenames <- list.files(folder_path)

    for (filename in filenames) {
      filename_path <- file.path(folder_path, filename)
      dict_name <- gsub(file_prefix, '', filename)
      dict_name <- gsub('.csv', '', dict_name)
      if (!is.null(file_suffix)) {
        dict_name <- strsplit(dict_name, file_suffix)[[1]][1]
      }

      csv_dict[[dict_name]] <- data.table::as.data.table(utils::read.csv(filename_path))
      cli::cli_inform("Table name: {dict_name}. # rows: {nrow(csv_dict[[dict_name]])}. # columns: {ncol(csv_dict[[dict_name]])}")
    }
  } else {
    cli::cli_alert_danger("{folder_path} folder does not exist, please check your folder path.")
  }
  return(csv_dict)
}

#' Save Dictionary List of Data Tables to CSV Files
#'
#' @param csv_dict Named list of data tables.
#' @param folder_path Directory where CSV files will be saved.
#' @param index Logical indicating whether row names should be saved.
#' @export
dict2file <- function(csv_dict, folder_path, index = FALSE) {
  if (dir.exists(folder_path)) {
    cli::cli_alert_info("{folder_path} folder exists...")
    cli::cli_alert_info("Saving data into directory...")

    for (filename in names(csv_dict)) {
      filename_path <- file.path(folder_path, paste0(filename, ".csv"))

      if (filename == 'leg2trip') {
        csv_dict[[filename]] <- as.data.frame(csv_dict[[filename]])
      } else if (filename == 'convex_hull' || filename == 'sde_ellipse') {
        if ("geometry" %in% names(csv_dict[[filename]])) {
          csv_dict[[filename]] <- csv_dict[[filename]][, !"geometry"]
        }
      }

      data.table::fwrite(csv_dict[[filename]], file = filename_path, row.names = index)
      cli::cli_inform("Table saved: {filename}")
    }
  } else {
    cli::cli_alert_danger("{folder_path} folder does not exist, please check your folder path.")
  }
}
