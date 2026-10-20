#' Process In-App Survey Data into Wide Format
#'
#' @param dict_tables Dictionary list of data tables.
#' @return Updated `dict_tables` with formatted wide survey tables and dictionaries.
#' @export
process_surveys <- function(dict_tables) {
  if (!is.null(dict_tables[['calendar_item_survey']])) {
    calendar_item_question_dict <- dict_tables$calendar_item_survey %>%
      dplyr::mutate(question_id = paste0("Q", question_id)) %>%
      dplyr::select(question_id, question_text, Q_Type) %>%
      dplyr::distinct()

    wide_calendar_item_survey_table <- dict_tables$calendar_item_survey %>%
      dplyr::mutate(question_id = paste0("Q", question_id)) %>%
      tidyr::pivot_wider(
        id_cols = c("study_id", "user_id", "cal_item_id", "start_timestamp", "response_timestamp"),
        names_from = "question_id",
        values_from = "response"
      ) %>%
      dplyr::rename(survey_timestamp = start_timestamp, survey_response_timestamp = response_timestamp)

    ucal_items_temporal_plot_with_survey_data <- dict_tables$ucalitems_temporal_plot %>%
      dplyr::left_join(
        wide_calendar_item_survey_table %>%
          dplyr::group_by(user_id, cal_item_id) %>%
          dplyr::arrange(dplyr::desc(survey_response_timestamp)) %>%
          dplyr::slice(1),
        by = c("user_id", "cal_item_id")
      )

    result_list <- list(
      "ucal_items_temporal_plot_with_survey_data" = ucal_items_temporal_plot_with_survey_data,
      "wide_calendar_item_survey_table" = wide_calendar_item_survey_table,
      "calendar_item_question_dict" = calendar_item_question_dict
    )
    dict_tables <- append(dict_tables, result_list)
  } else {
    cli::cli_inform("Skipping Calendar Item Surveys because no 'calendar_item_survey' table was found.")
  }

  if (!is.null(dict_tables[['ema_survey']])) {
    ema_survey_dict <- dict_tables$ema_survey %>%
      dplyr::mutate(question_id = paste0("Q", question_id)) %>%
      dplyr::select(question_id, question_text, Q_Type) %>%
      dplyr::distinct()

    wide_ema_survey_table <- dict_tables$ema_survey %>%
      dplyr::mutate(question_id = paste0("Q", question_id)) %>%
      tidyr::pivot_wider(
        id_cols = c("study_id", "user_id", "start_date", "timestamp", "response_timestamp"),
        names_from = "question_id",
        values_from = "response"
      )

    result_list <- list(
      "wide_ema_survey_table" = wide_ema_survey_table,
      "ema_survey_dict" = ema_survey_dict
    )
    dict_tables <- append(dict_tables, result_list)
  } else {
    cli::cli_inform("Skipping EMA surveys because no 'ema_survey' table was found.")
  }

  return(dict_tables)
}
