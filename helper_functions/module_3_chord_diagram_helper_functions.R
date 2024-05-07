################
### Packages ###
################
library(dplyr)
library(data.table)
library(cli)
library(plyr)
library(openxlsx)
library(tweenr)
library(circlize)
library(gifski)

#################
### Functions ###
#################

# In flow_rate_calculator, we are defining the rate of change between distinct time points. 
# Every 5 minutes is potentially too granular of a time comparison so we opt to have a way to aggregate above that when defining change over time. 
# Split_size tells us how many units to jump when defining transitions rates from state to state. 

flow_rate_calculator <- function(data, split_size, data_subset = NULL){
  
  # This control statement assumes a unit_length of 5. 
  # Its requirements are the following, less than 2 hour segments, must be able at least 1 unit length long and have no remainder when you divide the total units by split_length. 
  if(split_size > 24 | split_size < 1 | 288 %% split_size != 0){
    return("Please choose valid split size")
  }
  
  # Allow us to quickly generate a collection of these by passing the same dataset but a vector to filter on. 
  if(!is.null(data_subset )){
    data <- data[data_subset, ] 
  }
  
  ## Bookkeeping
  # List the units in the dataset (287 in our case, this is an artifact of how we generated the sequence data that no sequence at section 288)
  # Better implementations likely would have 288 units in the sequence
  table_col_names <- sort(as.numeric(names(data)[-c(1,2)]))
  # Select every split_size^th unit from the sequence. 
  indices <- seq(min(table_col_names), max(table_col_names), by = split_size)
  #Define a vector of Current Time and Next Time for aggregation purposes.
  current_time <- indices[-length(indices)]
  next_time <- indices[-1]
  
  # Initialize the data frame. 
  flow_rate_data <- data.frame(time = numeric(),
                               next_time = numeric(),
                               starting_state = character(),
                               ending_state = character(),
                               flow_rate= numeric())
  
  # More bookkeeping
  id_cols <- data[, c(1,2)] # Save ID columns
  data <- data[, -c(1,2)] # Drop ID Columns
  data <- data[, order(as.numeric(names(data)))] # Rearrange columns to be in the proper ordering. 
  names(data) <- paste0("X", names(data)) # Rename Columns
  
  
  # For each current_time, we are going to build a data.frame saying, if you started at time current_time[i], at next_time[i] (one split_time forward)
  # where were you compared to where you started. Then we aggregate it to counts. 
  for(i in 1:length(current_time)){
    flow_rate_data <- 
      bind_rows(flow_rate_data,
                suppressMessages(data.frame(time = current_time[i],
                           next_time = next_time[i],
                           starting_state = pull(data[, current_time[i]]),
                           ending_state = pull(data[, next_time[i]])) %>%
                  group_by(time, next_time, starting_state, ending_state) %>% 
                  dplyr::summarize(flow_rate = n())))
  }
  # Must also do this for going from Current Time = End of Day to Next Time = Beginning of Day. 
  flow_rate_data <-
    bind_rows(flow_rate_data,
              suppressMessages(data.frame(time = next_time[length(current_time)],
                         next_time = current_time[1],
                         starting_state = pull(data[, current_time[i]]),
                         ending_state = pull(data[, next_time[i]])) %>%
                group_by(time, next_time, starting_state, ending_state) %>%
                dplyr::summarize(flow_rate = n())))
  return(flow_rate_data)
}

process_flow_data <- function(dataset){
  dataset %>%# Drop the missing elements. This is when we have sequences that have some missing elements within them (eg phone is turned off).
    # Relabel the data as appropriate. 
    mutate(starting_state = factor(starting_state,
                                   levels = category_data$label,
                                   labels = category_data$subtype),
           ending_state = factor(ending_state,
                                 levels = category_data$label,
                                 labels = category_data$subtype)) %>%
    mutate(.frame = as.numeric(factor(time,
                                      levels = sort(unique(time)),
                                      labels = 1:length(unique(time))))) %>%
    left_join(category_data,
              by = c("starting_state"= "subtype")) %>%
    filter(!is.na(starting_state) & !is.na(ending_state)) %>%
    select(starting_state, ending_state,  flow_rate, order, color, .frame, time)
}

create_individual_plots <- function(data_list, data_names = NULL,
                                    location = "./outputs/plot-gif/",
                                    nrow = 1, 
                                    ncol = 1,
                                    width = 7,
                                    height = 7){
  
  # Print each figure. 
  for(f in unique(data_list[[1]]$.frame)){
    # I only want to create the plots using every other timepoint. In this case, that would mean every 30 minutes.
    if(f %% 2 == 1){
      next
    }
    # I do not want to generate figures when the time is less than a certain value. In this case, it works out to being less than 4:00 AM. 
    # Thus the .gif will ignore the time between 12:00 AM and 4:00AM.
    if(f < 15){
      next
    }
    
    time_of_day <- paste0(ifelse(f < 52, floor(f/4), floor(f/4) - 12), ":", f%%4*15, ifelse(f < 48, " AM", " PM"))
    if(f <4){
      time_of_day <- paste0(12, ":", f%%4*15, ifelse(f < 48, " AM", " PM"))
    }
    time_of_day <- gsub(":0 ", ":00 ", time_of_day)
    print(paste0("Plotting file: ", f, " of ", length(unique(data_list[[1]]$.frame))))
    # open a PNG plotting device
    png(file = paste0(location, "globalchord", f, ".png"), height = height, width = width, 
        units = "in", res = 1000)
    par(mfrow = c(nrow, ncol))
    for(i in 1:length(data_list)){
      tweened_data <- data_list[[i]]
      # initialise the circos plot
      circos.clear()
      par(mar = rep(0, 4), cex=1)
      #circos.par(start.degree = 90, track.margin=c(-0.1, 0.1), 
      #           gap.degree = 4, points.overflow.warning = FALSE)
      circos.par(start.degree = 90, track.margin=c(-0.1, 0.1), 
                 gap.after = 10, points.overflow.warning = FALSE,
                 clock.wise = F)
      # Filter the data to the right frame. 
      temp_data <- filter(tweened_data, .frame == f)
      
      # Color Bookkepping. 
      color_vec_names <- unique(temp_data$starting_state)
      temp <- category_data %>%
        filter(subtype %in% color_vec_names) 
      color_vec <- temp$color
      names(color_vec) <- temp$subtype
      
      # plot the chord diagram
      suppressMessages(chordDiagram(x = tweened_data %>% filter(.frame == f), directional = 1,
                                    order = category_data$subtype,
                                    grid.col = color_vec,
                                    annotationTrack = "grid",
                                    transparency = 0.25,  annotationTrackHeight = c(0.05, 0.12),
                                    direction.type = c("diffHeight", "arrows"), link.arr.type = "big.arrow",
                                    diffHeight  = -0.04, link.sort = TRUE, link.largest.ontop = TRUE))
      
      # Add the outer track with the text labels and numbers
      circos.track(track.index = 1, bg.border = NA, panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        sector.index = get.cell.meta.data("sector.index")
        reg1 = category_data %>% filter(subtype == sector.index) %>% mutate(reg1 = subtype) %>% pull(reg1)
        reg2 = category_data %>% filter(subtype == sector.index) %>% mutate(reg2 = NA) %>% pull(reg2)
        
        circos.text(x = mean(xlim), y = ifelse(is.na(reg2), 4, 4),
                    labels = reg1, facing = "bending", cex = 1)
        #circos.text(x = mean(xlim), y = 2.75, labels = reg2, facing = "bending", cex = 1.1)
        circos.axis(h = "top", labels.cex = 0.6,
                    labels.niceFacing = FALSE, labels.pos.adjust = FALSE)
      })
      if(!is.null(data_names)){
        text(x = 0, y = 1, label = data_names[i], cex = 1.8)
      }
      
      # Create a clock for the Time of Day. (This assumes that we are working with 5 minute units.)
      time_of_day <- paste0(ifelse(f < 52, floor(f/4), floor(f/4) - 12), ":", f%%4*15, ifelse(f < 48, " AM", " PM"))
      if(f <4){
        time_of_day <- paste0(12, ":", f%%4*15, ifelse(f < 48, " AM", " PM"))
      }
      time_of_day <- gsub(":0 ", ":00 ", time_of_day)
      text(x = -0.75, y = 1.0, labels = c("Time:"), cex = 1.5)
      text(x = -0.75, y = 0.92, labels = time_of_day, cex = 1.5)
      # close plotting device
    }
    
    dev.off()
  }
  return(paste0("Plots have been generated at: ", location))
}