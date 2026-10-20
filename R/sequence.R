#' Convert Calendar Items into Sequence Data
#'
#' Converts calendar items dataset into discrete time interval sequences.
#'
#' @param dataset Data frame or data table of temporal calendar items.
#' @param unit_length Time unit length in minutes (e.g. 5, 10, 15).
#' @return Named list containing long sequence data, wide sequence data, and category ID key.
#' @export
generate_sequence_data <- function(dataset, unit_length = 5) {
  if (1440 %% unit_length != 0) {
    stop("Please select a unit_length that divides evenly into 1440 minutes (e.g. 1, 2, 3, 5, 10, 15, 20, 30, 60).")
  }
  if (any(dataset$subtype < 1 | dataset$subtype > length(letters), na.rm = TRUE)) {
    cli::cli_abort(c(
      "{.field subtype} must be integer codes from 1 to {length(letters)}.",
      "i" = "Raw Daynamica subtype codes start at 0 and overlap between activities and trips.",
      "i" = "Run {.fn recategorize_calendar_items} first to assign consistent codes."
    ))
  }

  long_sequence_data <- dataset %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      start_time_frac = floor((lubridate::hour(lubridate::hms(start_time)) * 60 + lubridate::minute(lubridate::hms(start_time)) + lubridate::second(lubridate::hms(start_time)) / 60) / unit_length) + 1,
      start_time_frac = ifelse(start_time_frac == 0, 1, start_time_frac),
      end_time_frac = ceiling((lubridate::hour(lubridate::hms(end_time)) * 60 + lubridate::minute(lubridate::hms(end_time)) + lubridate::second(lubridate::hms(end_time)) / 60) / unit_length),
      end_time_frac = ifelse(end_time_frac == 0, 1440 / unit_length, end_time_frac),
      diff = end_time_frac - start_time_frac,
      letters_subtype = letters[subtype]
    ) %>%
    dplyr::select(user_id, start_date, start_time_frac, end_time_frac, diff, subtype, letters_subtype, subtype_decoded) %>%
    dplyr::filter(diff > 0) %>%
    dplyr::distinct() %>%
    tidyr::uncount(diff) %>%
    dplyr::group_by(user_id, start_date, start_time_frac, end_time_frac) %>%
    dplyr::mutate(loc = dplyr::row_number() + start_time_frac - 1) %>%
    dplyr::filter(loc < 1440 / unit_length) %>%
    dplyr::distinct() %>%
    dplyr::mutate(end_time_frac = end_time_frac - 1, diff = end_time_frac - start_time_frac)

  wide_sequence_data <- long_sequence_data %>%
    dplyr::group_by(user_id, start_date, loc) %>%
    dplyr::arrange(dplyr::desc(diff)) %>%
    dplyr::slice(1) %>%
    tidyr::pivot_wider(id_cols = c("user_id", "start_date"), names_from = "loc", values_from = "letters_subtype")

  id_key <- dataset %>%
    dplyr::select(subtype_decoded, subtype) %>%
    dplyr::distinct() %>%
    dplyr::mutate(letters_subtype = letters[subtype])

  return(list("long_sequence_data" = long_sequence_data, "wide_sequence_data" = wide_sequence_data, "id_key" = id_key))
}

#' Create Activity Chains
#'
#' @param dataset Temporal calendar items dataset or wide sequence data frame.
#' @param include_trips Logical indicating whether to include trips.
#' @param minimum_duration_min Minimum duration threshold in minutes.
#' @param special_labels_long Long category names for custom labeling.
#' @param special_labels_short Short category letters for custom labeling.
#' @param collapse_identical_labels Logical indicating whether to collapse duplicate adjacent labels.
#' @return Data frame with activity chains string column.
#' @export
create_activity_chains <- function(dataset, include_trips = FALSE, minimum_duration_min = 15,
                                   special_labels_long = c("HOME", "WORKPLACE", "WORK"),
                                   special_labels_short = c("H", "W", "W"),
                                   collapse_identical_labels = FALSE) {

  if ("string" %in% names(dataset)) {
    return(dataset)
  }

  minimum_duration <- minimum_duration_min / 60
  main_data <- dataset %>%
    dplyr::select(user_id, start_date, end_date, start_time, end_time, duration_after_split, type_decoded, subtype_decoded) %>%
    dplyr::filter(duration_after_split > minimum_duration) %>%
    tibble::as_tibble()

  if (!include_trips) {
    main_data <- main_data %>% dplyr::filter(type_decoded %in% c("ACTIVITY"))
  } else {
    main_data <- main_data %>% dplyr::filter(type_decoded %in% c("ACTIVITY", "TRIP"))
  }

  long_labels <- c(special_labels_long)
  short_labels <- c(special_labels_short)
  main_data <- main_data %>%
    dplyr::mutate(subtype_short = as.character(factor(subtype_decoded, levels = c(long_labels), labels = c(short_labels)))) %>%
    dplyr::mutate(subtype_short = ifelse(type_decoded == "TRIP", "T", subtype_short))

  main_data$subtype_short[is.na(main_data$subtype_short)] <- "O"

  activity_chains <- main_data %>%
    dplyr::group_by(user_id, start_date) %>%
    dplyr::arrange(start_time) %>%
    dplyr::summarize(string = paste(subtype_short, collapse = ""), .groups = "drop")

  if (collapse_identical_labels) {
    unique_labels <- unique(main_data$subtype_short)
    for (i in seq_along(unique_labels)) {
      lbl <- unique_labels[i]
      for (k in 1:5) {
        activity_chains$string <- gsub(paste0(lbl, lbl), lbl, activity_chains$string)
      }
    }
  }

  return(activity_chains)
}

# Activity Chains Internal Helper Functions
get_prev <- function(string, pos) {
  return(substr(string, start = 1, stop = pos))
}

get_current <- function(string, pos) {
  return(substr(string, start = pos, stop = pos))
}

is_terminal_act <- function(string, pos) {
  return(get_current(string, pos) == "" & get_current(string, pos - 1) != "")
}

get_count <- function(string, prev_string) {
  pos <- nchar(prev_string) + 1
  valid_rows <- (get_prev(string, pos - 1) == prev_string)
  vec <- get_current(string, pos)
  vec[is_terminal_act(string, pos)] <- "X"
  vec <- vec[valid_rows]
  prev_ended <- which(vec == "")
  if (length(prev_ended) > 0) {
    vec <- vec[-prev_ended]
  }
  return(as.data.frame(table(vec), stringsAsFactors = FALSE))
}

#' Create Conditional Activity Chains Summary Table for Sankey Plots
#'
#' @param sub_data Activity chains data frame.
#' @param lower_limit Minimum prevalence proportion threshold.
#' @return Formatted data frame for Sankey diagram.
#' @export
create_sankey_data <- function(sub_data, lower_limit = 0.01) {
  pattern <- unique(sub_data$string)
  pattern <- pattern[!is.na(pattern)]

  sub_pattern <- c()
  for (i in 1:max(nchar(pattern))) {
    sub_pattern <- unique(c(sub_pattern, get_prev(pattern, i)))
  }
  possible_prev_strings <- sort(sub_pattern)

  results <- data.frame(
    prev = character(),
    current = character(),
    position = numeric(),
    count = numeric(),
    stringsAsFactors = FALSE
  )

  for (i in 0:length(possible_prev_strings)) {
    prev <- if (i > 0) possible_prev_strings[i] else ""
    tab <- get_count(sub_data$string, prev)
    position <- nchar(prev) + 1

    if (nrow(tab) > 0) {
      for (j in 1:nrow(tab)) {
        current <- as.character(tab$vec[j])
        count <- as.numeric(tab$Freq[j])
        results <- rbind(results, data.frame(prev = prev, current = current, position = position, count = count, stringsAsFactors = FALSE))
      }
    }
  }

  results <- results %>% dplyr::arrange(position, prev, current)
  results <- dplyr::left_join(
    results,
    results %>% dplyr::group_by(prev) %>% dplyr::summarize(n = sum(as.numeric(count)), .groups = "drop"),
    by = "prev"
  ) %>%
    dplyr::mutate(group_percentage = paste0(round(as.numeric(count) / n, 2) * 100, "%")) %>%
    dplyr::select(prev, current, count, group_percentage)

  results <- results %>% dplyr::arrange(prev, current)
  results$count <- as.numeric(results$count)

  total <- nrow(sub_data)
  plot_data <- results %>% dplyr::filter(as.numeric(count) >= floor(lower_limit * total))

  return(plot_data)
}

#' Create Sankey Network Plot from Activity Chains Data
#'
#' @param data Output data frame from `create_sankey_data`.
#' @param start_from_H Logical indicating whether to filter nodes starting with 'H'.
#' @param color_override Optional color scale override string.
#' @return networkD3 sankeyNetwork widget object.
#' @export
create_plot_from_data <- function(data, start_from_H = TRUE, color_override = NULL) {
  if (start_from_H) {
    data <- data[-which(substr(data$prev, 1, 1) != "H"), ]
  }
  data$target <- paste0(data$prev, data$current)
  data <- data %>% dplyr::arrange(dplyr::desc(as.numeric(count)))

  links <- data.frame(
    source = c(data$prev),
    target = c(data$target),
    value = c(data$count),
    stringsAsFactors = FALSE
  )
  nodes <- data.frame(
    name = unique(c(as.character(links$source), as.character(links$target))),
    stringsAsFactors = FALSE
  )

  links$IDsource <- match(links$source, nodes$name) - 1
  links$IDtarget <- match(links$target, nodes$name) - 1

  nodes$group <- as.factor(substr(as.character(nodes$name), nchar(as.character(nodes$name)), nchar(as.character(nodes$name))))

  p <- networkD3::sankeyNetwork(
    Links = links,
    Nodes = nodes,
    Source = "IDsource",
    Target = "IDtarget",
    Value = "value",
    NodeID = "name",
    NodeGroup = "group",
    sinksRight = FALSE,
    fontSize = 12
  )

  return(p)
}

#' Run Chain Simulation
#'
#' @param data Input activity chains data frame.
#' @param export_data_only Logical indicating whether to return raw simulated data frame.
#' @param sim_count Number of simulated person-days.
#' @return Simulated data frame or Sankey plot object.
#' @export
run_chain_simulation <- function(data, export_data_only = FALSE, sim_count = 200) {
  temp <- data.frame(table(data$string), stringsAsFactors = FALSE) %>%
    dplyr::mutate(perc = round(Freq / nrow(data), 4)) %>%
    dplyr::filter(perc >= 0.001) %>%
    dplyr::mutate(
      standardized_perc = perc / sum(perc),
      counts = rowSums(stats::rmultinom(sim_count, 1, prob = standardized_perc))
    ) %>%
    dplyr::arrange(dplyr::desc(standardized_perc))

  if (export_data_only) {
    return(temp)
  }

  vec <- c()
  temp$Var1 <- as.character(temp$Var1)
  for (i in seq_len(nrow(temp))) {
    vec <- c(vec, rep(temp$Var1[i], temp$counts[i]))
  }

  testing_dataset <- data.frame(string = vec, stringsAsFactors = FALSE)
  p <- testing_dataset %>%
    create_sankey_data(lower_limit = 0.01) %>%
    create_plot_from_data()

  return(p)
}
