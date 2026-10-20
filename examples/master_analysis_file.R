#################################################
##### Daynamica Master Analysis Example Script #####
#################################################

# This script demonstrates how to use the `rDaynamica` package to import,
# clean, process, and analyze smartphone data from the Daynamica application.

library(rDaynamica)
library(dplyr)
library(data.table)

############################
### Script / Data Import ###
############################

# Load Daynamica data from CSV directory
# For demonstration, we use the sample anonymized dataset provided in the package or repository.
data_path <- system.file("extdata", "anonymized_data", package = "rDaynamica")
if (data_path == "" || !dir.exists(data_path)) {
  data_path <- "data/anonymized_data"
}

csv_dict <- path2dict(folder_path = data_path, file_prefix = "anonymized_", file_suffix = NULL)


##################################
### Data Cleaning & Processing ###
##################################

# Update calendar item formats & timestamps
csv_dict <- ucal_items_version_updater(csv_dict)

# Exclude specific user IDs if necessary
exclude_id <- c('test', 'user_id_382', 'user_id_422', '0')
csv_dict <- exclude_user(csv_dict, exclude_id)

# Format cal_item_id
csv_dict$ucalitems$cal_item_id <- as.integer(csv_dict$ucalitems$cal_item_id)

# Assign trip purpose based on subsequent activity
csv_dict[['ucalitems']] <- assign_trip_purpose(csv_dict[['ucalitems']])

# Join survey datasets if present
if (!is.null(csv_dict$calendar_item_survey)) {
  csv_dict[['ucalitems_survey']] <- survey_join_id(csv_dict[['calendar_item_survey']], csv_dict[['ucalitems']])
  csv_dict[['ucalitems_ljoin_ucisurvey']] <- ucalitems_ljoin_ucisurvey(csv_dict[['ucalitems_survey']], csv_dict[['ucalitems']])
} else {
  csv_dict[['ucalitems_ljoin_ucisurvey']] <- csv_dict[['ucalitems']] %>% mutate(survey_not_null = FALSE)
}

# Split multiday items across midnight/day boundaries
csv_dict[['ucalitems_ljoin_ucisurvey_split']] <- split_ucalitems(
  csv_dict[['ucalitems_ljoin_ucisurvey']],
  local_timezone = 'US/Central',
  day_start_hour = 0,
  min_time_stamp = 0
)

# Generate daily summaries and filter valid days (>= 12 hours)
csv_dict[['day_summary']] <- get_per_day_duration(csv_dict[['ucalitems_ljoin_ucisurvey_split']])

minimum_hours <- 12
query_text <- paste0('(interact_with_app>0)&(with_subtype>=', minimum_hours, ')')
csv_dict_sub <- filter_valid_days(csv_dict, query_text)

# Process in-app survey responses
csv_dict_sub <- process_surveys(csv_dict_sub)


############################################################
### Module 1: Standard Daynamica Analysis Report        ###
############################################################

run_standard_analysis(
  daynamica_data = csv_dict_sub,
  output_dir = "outputs/primary_analysis",
  file_name = "primary_analysis_result",
  export_results = TRUE
)


############################################################
### Module 2: User Calendar Item Visualization Report    ###
############################################################

# Recategorize categories for simplified visualization
new_cats <- c(
  "Motorized Trip", "Car - Driver",
  "Motorized Trip", "Car - Passenger",
  "Other", "Care Giving",
  "Other", "Civil Errands",
  "Other", "Community & Cultural",
  "Other", "Education",
  "Eat Out", "Food & Meal",
  "Other", "Fun & Leisure",
  "Home", "Home",
  "Other", "Medical & Fitness",
  "Other", "Other Activities",
  "Motorized Trip", "Other Trips",
  "Motorized Trip", "Rail",
  "Other", "Religious & Spiritual",
  "Shop", "Shopping Errands",
  "Motorized Trip", "Taxi/Uber/Lyft",
  "Missing", "Unknown",
  "Motorized Trip", "Vehicle",
  "Non-Motorized Trip", "Wait",
  "Non-Motorized Trip", "Walk",
  "Work", "Workplace",
  "Non-Motorized Trip", "Bike",
  "Motorized Trip", "Bus"
)
recat_dict <- data.frame(matrix(toupper(new_cats), ncol = 2, byrow = TRUE), stringsAsFactors = FALSE)
names(recat_dict) <- c("new_categories", "orig_categories")

csv_dict_new_cat <- recategorize_calendar_items(
  csv_dict_sub,
  orig_cats = recat_dict$orig_categories,
  new_cats = recat_dict$new_categories
)

run_visualization_report(
  daynamica_data = csv_dict_new_cat,
  output_dir = "outputs/ucal_visualization_output",
  file_name = "ucal_visualization_example"
)


############################################################
### Module 3: Chord Diagram GIF Generator                ###
############################################################

generate_chord_diagram_gif(
  daynamica_data = csv_dict_new_cat,
  output_dir = "outputs/plot-gif",
  gif_filename = "ucal_chord_diagram_visualization.gif"
)


############################################################
### Module 4: Activity Chaining Sequences                ###
############################################################

run_activity_chains_report(
  daynamica_data = csv_dict_sub,
  output_dir = "outputs/Module_4",
  file_name = "Activity_Chains_Module_Example"
)
