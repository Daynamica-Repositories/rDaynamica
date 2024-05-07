library(lubridate)

generate_sequence_data <- 
  function(dataset, unit_length = 5){
    if(1440 %% unit_length != 0){
      stop("Please select a unit_length which can fits nicely into 1440 minutes. Examples(non-exclusive): 1, 2, 3, 5, 10, 15, 20, 30, 60.")
    }
    long_sequence_data <- 
      dataset %>%
      tibble() %>%
      mutate(start_time_frac = floor((hour(hms(start_time))*60 + minute(hms(start_time)) + second(hms(start_time))/60)/unit_length) + 1,
             start_time_frac = ifelse(start_time_frac == 0, 1, start_time_frac),
             end_time_frac = ceiling((hour(hms(end_time))*60 + minute(hms(end_time)) + second(hms(end_time))/60)/unit_length),
             end_time_frac = ifelse(end_time_frac == 0, 1440/unit_length, end_time_frac),
             diff = end_time_frac - start_time_frac,
             letters_subtype = letters[subtype]) %>%
      select(user_id, start_date,
             start_time_frac,
             end_time_frac,
             diff,
             subtype, 
             letters_subtype,
             subtype_decoded) %>%
      filter(diff > 0) %>%
      distinct() %>%
      tidyr::uncount(diff) %>%
      group_by(user_id, start_date, start_time_frac, end_time_frac) %>%
      dplyr::mutate(#loc = paste0("T_", row_number() + start_time_frac - 1),
        loc = row_number() + start_time_frac - 1) %>% # This isn't a perfect implementation if there are duplicate user_days, but it is close and likely sufficient for visualization purposes.
      filter(loc < 1440/unit_length) %>%
      distinct() %>%
      mutate(end_time_frac = end_time_frac - 1,
             diff = end_time_frac - start_time_frac)
    
    
    wide_sequence_data <- 
      long_sequence_data %>%
      #filter(user_id == "user_id_201",
      #       start_date == "2022-12-06") %>% 
      group_by(user_id,
               start_date,
               loc) %>%
      arrange(desc(diff)) %>%
      slice(1) %>% # This and the two sets of functions above operate to deal with overlapping segments.
      #It takes the label of the longest segment if segments overlap. 
      tidyr::pivot_wider(id_cols = c("user_id", "start_date"),
                         names_from = "loc",
                         values_from = "letters_subtype")
    id_key <- 
      dataset %>%
      select(subtype_decoded, 
             subtype) %>% 
      distinct() %>%
      mutate(letters_subtype = letters[subtype])
    
    sequence_data_list <- list("long_sequence_data" = long_sequence_data,
                               "wide_sequence_data" = wide_sequence_data,
                               "id_key" = id_key)
    return(sequence_data_list)
  }



