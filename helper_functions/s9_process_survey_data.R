process_surveys <- function(dict_tables){
  if(!is.null(dict_tables[['calendar_item_survey']])){
    calendar_item_question_dict <- 
      dict_tables$calendar_item_survey %>% 
      mutate(question_id = paste0("Q", question_id)) %>%
      select(question_id,
             question_text,
             Q_Type) %>% 
      distinct()
    
    wide_calendar_item_survey_table <- 
      dict_tables$calendar_item_survey %>%
      mutate(question_id = paste0("Q", question_id)) %>%
      tidyr::pivot_wider(id_cols = c("study_id", "user_id",
                                     "cal_item_id",
                                     "start_timestamp",
                                     "response_timestamp"),
                         names_from = "question_id",
                         values_from = "response") %>%
      dplyr::rename(survey_timestamp = start_timestamp,
                    survey_response_timestamp = response_timestamp)
    
    ucal_items_temporal_plot_with_survey_data <- 
      dict_tables$ucalitems_temporal_plot %>%
      left_join(wide_calendar_item_survey_table %>%
                  group_by(user_id, cal_item_id) %>%
                  arrange(desc(survey_response_timestamp)) %>%
                  slice(1),
                by = c("user_id", "cal_item_id")) 
    
    result_list <- list("ucal_items_temporal_plot_with_survey_data" = ucal_items_temporal_plot_with_survey_data,
                        "wide_calendar_item_survey_table" = wide_calendar_item_survey_table,
                        "calendar_item_question_dict" = calendar_item_question_dict)
    dict_tables <- append(dict_tables,result_list)
  } else{
    message("Skipping Calendar Item Surveys because no 'calendar_item_survey' table identified")
  }

  if(!is.null(dict_tables[['ema_survey']])){
  ema_survey_dict <- 
    dict_tables$ema_survey %>% 
    mutate(question_id = paste0("Q", question_id)) %>%
    select(question_id,
           question_text,
           Q_Type) %>% 
    distinct()
  
  wide_ema_survey_table <- 
    dict_tables$ema_survey %>%
    mutate(question_id = paste0("Q", question_id)) %>%
    tidyr::pivot_wider(id_cols = c("study_id", "user_id",
                                   "start_date",
                                   "timestamp",
                                   "response_timestamp"),
                       names_from = "question_id",
                       values_from = "response")
  
  result_list <- list(
                      "wide_ema_survey_table" = wide_ema_survey_table,
                      "ema_survey_dict" = ema_survey_dict)
  dict_tables <- append(dict_tables,result_list)
  
  }else{
    message("Skipping EMA surveys because no 'ema_survey' table identified")
  }
  return(dict_tables)
}
