path2dict <- function(folder_path, file_prefix, file_suffix) {
  csv_dict <- list()
  
  if (file.exists(folder_path)) {
    cat(sprintf("%s folder exists...\n", folder_path))
    cat("now reading data into list...\n")
    
    filenames <- list.files(folder_path)
    
    for (filename in filenames) {
      filename_path <- file.path(folder_path, filename)
      dict_name <- gsub(file_prefix, '', filename)
      dict_name <- gsub('.csv', '', dict_name)
      if(!is.null(file_suffix)){
        dict_name <-  strsplit(dict_name, file_suffix)[[1]][1]
      }
                        
      csv_dict[[dict_name]] <- read.csv(filename_path) %>% as.data.table()
      cat(sprintf('Table name: %s. # rows: %s. # columns: %s \n',
                  dict_name, 
                  nrow(csv_dict[[dict_name]]), 
                  ncol(csv_dict[[dict_name]])))
    }
  } else {
    cat(sprintf("%s folder does not exist, please check your folder path \n", 
                folder_path))
  }
  return(csv_dict)
}

dict2file <- function(csv_dict, folder_path, index = FALSE) {
  if (dir.exists(folder_path)) {
    cat(sprintf("%s folder exists...\n", folder_path))
    cat("now saving data into dictionary...\n")
    
    for (filename in names(csv_dict)) {
      filename_path <- file.path(folder_path, paste0(filename, ".csv"))
      
      if (filename == 'leg2trip'){
        csv_dict[[filename]] = csv_dict[[filename]] %>% as.data.frame()
      } else if (filename == 'convex_hull' | filename == 'sde_ellipse') {
        csv_dict[[filename]] = csv_dict[[filename]][, !"geometry"]
      }
      
      fwrite(csv_dict[[filename]], file = filename_path, row.names = index)
      cat(sprintf("Table name: %s\n", filename))
    }
  } else {
    cat(sprintf("%s folder does not exist, please check your folder path...\n", 
                folder_path))
  }
}
