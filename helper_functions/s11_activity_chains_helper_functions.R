create_activity_chains <- function(dataset, include_trips = F, minimum_duration_min = 15,
                                   special_labels_long = c("HOME", "WORKPLACE", "WORK"),
                                   special_labels_short = c("H", "W", "W"),
                                   collapse_identical_labels = F){
  
  minimum_duration <- minimum_duration_min/60
  main_data <- dataset %>%
    select(user_id, start_date, end_date, start_time, end_time,
           duration_after_split,
           type_decoded,
           subtype_decoded) %>%
    filter(duration_after_split > minimum_duration) %>%
    tibble()
  
  # Assess whether we should keep Activities and Trips or just Activities
  if(!include_trips){
    main_data <- 
      main_data %>%
      filter(type_decoded %in% c("ACTIVITY"))
  } else{
    main_data <- 
      main_data %>%
      filter(type_decoded %in% c("ACTIVITY", "TRIP"))
  }
  
  # Relabel IDs
  long_labels <- c(special_labels_long)
  short_labels <- c(special_labels_short)
  main_data <- 
    main_data %>%
    mutate(subtype_short = as.character(factor(subtype_decoded,
                                               levels = c(long_labels),
                                               labels = c(short_labels)))) %>%
    mutate(subtype_short = ifelse(type_decoded == "TRIP", "T", subtype_short))
  main_data$subtype_short[is.na(main_data$subtype_short)] <- "O"
  
  activity_chains <- 
    main_data %>%
    group_by(user_id, start_date) %>%
    arrange(start_time) %>%
    dplyr::summarize(string = paste(subtype_short, collapse = ""))
  
  if(collapse_identical_labels){
    unique_labels = main_data$subtype_short %>% unique()
    for(i in 1:length(unique_labels)){
      # This is inefficient but easy. It protects against a 16 long string replicate label. 
      activity_chains$string <- gsub(paste0(unique_labels[i], unique_labels[i]), unique_labels[i], activity_chains$string)
      activity_chains$string <- gsub(paste0(unique_labels[i], unique_labels[i]), unique_labels[i], activity_chains$string)
      activity_chains$string <- gsub(paste0(unique_labels[i], unique_labels[i]), unique_labels[i], activity_chains$string)
      activity_chains$string <- gsub(paste0(unique_labels[i], unique_labels[i]), unique_labels[i], activity_chains$string)
      activity_chains$string <- gsub(paste0(unique_labels[i], unique_labels[i]), unique_labels[i], activity_chains$string)
    }
  }
  
  return(activity_chains)
}


# Helper Functions
get_prev <- function(string, pos){
  return(substr(string, start = 1, stop = pos))
}
get_current <- function(string, pos){
  return(substr(string, start = pos, stop = pos))
}
is_terminal_act <- function(string, pos){
  return(get_current(string, pos) == "" & get_current(string, pos -1) != "")
}
#Provide a vector for string, and the previous string for the second value. This will give you a table of the next entries. 
get_count <- function(string, prev_string){
  pos <- nchar(prev_string) + 1
  valid_rows <- (get_prev(string, pos-1) == prev_string)
  vec <- get_current(string, pos)
  vec[is_terminal_act(string, pos)] <- "X"
  vec <- vec[valid_rows]
  prev_ended <- which(vec == "")
  if(length(prev_ended > 0)){
    vec <- vec[-which(vec == "")]
  }
  return(as.data.frame(table(vec)))
}




create_sankey_data <- function(sub_data, lower_limit = 0.01){
  pattern <- unique(sub_data$string)
  if(length(which(is.na(pattern))) > 0){
    pattern <- pattern[-which(is.na(pattern))]
  }
  sub_pattern <- c()
  for(i in 1:max(nchar(pattern))){
    sub_pattern <- unique(c(sub_pattern, get_prev(pattern, i)))
  }
  possible_prev_strings <- sort(sub_pattern)
  results <- data.frame(prev = as.character(),
                        current = as.character(),
                        position = numeric(),
                        count = numeric())
  results$prev <- as.character(results$prev)
  results$current <- as.character(results$current)
  i = 1
  for(i in 0:length(possible_prev_strings)){
    if(i >0){
      prev <- possible_prev_strings[i]
    } else {prev <- ""}
    tab <-  get_count(sub_data$string, prev)
    position <- nchar(prev) + 1
    for(j in 1:nrow(tab)){
      current <- tab$vec[j]
      count <- tab$Freq[j]
      results[nrow(results) + 1,] <- c(as.character(prev),
                                       as.character(current),
                                       as.numeric(position),
                                       as.numeric(count))
      
    }
  }
  
  results <- results %>% arrange(position, prev, current)
  results <- left_join(results, results %>%
                         group_by(prev) %>%
                         dplyr::summarize(n = sum(as.numeric(count))) %>%
                         ungroup(), by = "prev") %>%
    mutate(group_percentage = paste0(round(as.numeric(count)/n,2)*100,"%")) %>%
    select(prev, current, count, group_percentage)
  results <- results %>% arrange(prev, current)
  results$count <- as.numeric(results$count)
  total <- nrow(sub_data)
  plot_data <- results %>% filter(as.numeric(count) >= floor(lower_limit*total))
  #if(contains_M == FALSE){
  #  plot_data <- plot_data[-unique(c(grep("M", plot_data$prev), grep("M", plot_data$current))), ]
  #}
  return(plot_data)
}



create_plot_from_data <- function(data, start_from_H = TRUE, color_override = NULL){
  #data <- data[-which(data$prev == ""),]
  if(start_from_H == TRUE){
    data <- data[-which(substr(data$prev, 1,1 ) != "H"),]
  }
  data$target <- paste0(data$prev, data$current)
  data <- data %>% arrange(desc(as.numeric(count)))
  links <- data.frame(
    source=c(data$prev), 
    target=c(data$target), 
    value=c(data$count)
  )
  nodes <- data.frame(
    name=c(as.character(links$source), as.character(links$target)) %>% 
      unique()
  )
  
  # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
  links$IDsource <- match(links$source, nodes$name)-1 
  links$IDtarget <- match(links$target, nodes$name)-1
  #links$group <- as.factor(data$current)
  nodes$group <- as.factor("my_unique_group")
  nodes$group <- as.factor(substr(as.character(nodes$name),
                                  nchar(as.character(nodes$name)),
                                  nchar(as.character(nodes$name))))
  # prepare color scale: I give one specific color for each node.
  #my_color <- 'd3.scaleOrdinal() .domain(["H", "O","S", "X", "my_unique_group"]) .range(["blue", "red", "yellow", "purple","grey"])'
    # Make the Network. I call my colour scale with the colourScale argument
  p <- sankeyNetwork(Links = links,
                     Nodes = nodes,
                     Source = "IDsource",
                     Target = "IDtarget", 
                     Value = "value",
                     NodeID = "name",
                     NodeGroup = "group",
                     #LinkGroup = "group",
                     sinksRight = FALSE,
                     fontSize = 12)
  p
  return(p)
}



run_chain_simulation <- function(data, export_data_only = F, sim_count = 200){
  temp <- data.frame(table(data$string)) %>%
    mutate(perc = round(Freq/nrow(data),4)) %>%
    filter(perc >= 0.001) %>% 
    mutate(standardized_perc = perc/sum(perc),
           counts = rowSums(rmultinom(sim_count, 1, prob = standardized_perc))) %>% 
    arrange(desc(standardized_perc)) 
  if(export_data_only == T){
    return(temp)
  }
  
  testing_dataset <- data.frame(labeled = character(sum(temp$counts)), stringsAsFactors = F)
  vec <- c()
  temp$Var1 <- as.character(temp$Var1)
  for(i in 1:nrow(temp)){
    vec <- c(vec, rep(temp$Var1[i], temp$counts[i]))
  }
  testing_dataset$string <- vec
  testing_dataset %>% 
    create_sankey_data(lower_limit = 0.01) %>%
    create_plot_from_data() %>% return()
}
