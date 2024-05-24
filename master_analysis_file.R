##############################################
##### Daynamica Master Analysis Document #####
##############################################

################
### Overview ###
################

# This document organizes and collects different sets of code commonly used to analyze Daynamica data. 
# It may serve as a starting point for standard analyses, as well as accelerating new research and code development.
# This workflow is designed in a modular format. This means that each analysis is designed to be a standalone analysis without unnecessary dependencies on other modules.
# The hope is that by using a modular format, iteration and inclusion of new analysis modules shall be simple and painless.
# Some code is meant to be used to generate a report (.rmd) while other code will exist within standalone scripts (.R). 

# Certain functions which are viewed as critical and commonly used are stored within a collection of .R scripts in the folder "helper_functions". 
# These files will be used for data processing as well as common statistical and visualization tasks. 

# These analyses are designed to produce a more standardized and portable analysis dataset. 

# The modules are standardized to work with any Daynamica data. Each module has a collection of standard procedures as well as additional
# inputs or parameters which can be modified to customize a specific analysis. 
# The outputs will vary by each module. The primary module is the standard_daynamica_anlaysis_template. This template provides the foundation summary calculations of Daynamica data. 
# This document will showcase how this module may be used and modified to generate standard and custom Daynamica reports. 

#################
### Libraries ###
#################

# This section loads the standard R Packages that all of the analysis software uses in some form or another. 
# These packages are essential for the data import and cleaning procedures.
# Additional module specific packages will be imported when needed within each module. 

#/Load all necessary libraries
library(rmarkdown)
library(dplyr)
library(data.table)
library(cli)
library(plyr)
library(openxlsx)
library(tidyr)
library(ggplot2)
library(tibble)
#Note: Other R packages will be loaded on an as needed basis at the begin of helper function scripts and modules. 
#Note: For some rare tasks, we may not import an entire package and instead just use a single function. For example, we often need to modify dates
# in which case we use the lubridate package and will use the function lubridate::mdy_hms()

############################
### Script / Data Import ###
############################

# Load the primary helper function scripts
source('helper_functions/s1_io_data.R')
source('helper_functions/s2_preprocess_data_v2.R')
source('helper_functions/s3_valid_data.R')
source('helper_functions/s9_process_survey_data.R')

#* This section imports the daynamica data that will be analyzed in this document. 
#* The general procedure for importing data is quite straightforward. The primary input that is required for this analysis is to specify the location or directory that the data is located. 
#* The data will be imported using our predefined functions. The collection of Daynamica datasets are stored in a named list where each dataset is an individual element of the list. 

# This function imports the Daynamica data
csv_dict = path2dict(folder_path = "data/anonymized_data", file_prefix = "anonymized_", file_suffix = NULL)


#@Note: These will be deleted from the final repository. 

# csv_dict = path2dict("data/cti", "CTI_Study_", file_suffix = "_20210607_084743")
# csv_dict = path2dict("data/cti2", "CTI_Study_", file_suffix = "_20210607_084743")
# folder_path = 'data/raw_equity_data'
# #This is your project name:
# file_prefix = 'Transportation_Equity_'
# #This is the year of project:
# file_suffix = '_2023'
#csv_dict = path2dict(folder_path, file_prefix, file_suffix)
# #csv_dict = path2dict("data/test_data", "test_", file_suffix = NULL)
# csv_dict = path2dict("data/flexpass2", "UMN_FlexPass_", file_suffix = "_20211215_135445")

##################################
### Data Cleaning & Processing ###
##################################

#* Certain steps of the Daynamica analysis pipeline are considered essential and cannot be avoided. These steps must be performed for every Daynamica analysis. 
#* Because they are not specific to any particular analysis module, they are included within this document instead of being replicated at the beginning of each analysis module. 
#* There are a series of decisions which must be made in the process of cleaning and preparing the data for analysis. 
#* We provide reasonable defaults for how to clean and process the data but recognize that some researchers may need alternative parameterizations.
#* These specific decisions will be highlighted in this section and the data analyst may choose which option is best for them. 

# List out data cleaning and processing steps as well as the general comments surrounding them. 

# This function modifies different versions of daynamica data to be compatible with the set of analysis tools provided in this repository. 
csv_dict <- ucal_items_version_updater(csv_dict)

### Drop IDs from dataset
# Ids to omit from the analysis
# [Option 1]
# Provide keyword to match on:
exclude_id <- 'test'

# [Option 2]
# Provide specific User IDs:
exclude_id <- c('user_id_382', 'user_id_422', '0')


csv_dict <- exclude_user(csv_dict, exclude_id)

# Rename user_id's to anonymize the dataset. Within csv_dict will be a table with the keys linking the new ids to the original ids. 
# Use set.seed with any number to ensure reproducibility of the keys. 
# A new table is added to csv_dict which links original non-anonymized ids to the new anonymized ids. This should be saved in a sequre location. 
#csv_dict <- rename_userids(csv_dict, seed = NULL)


# Relabeling of the Calendar Items if you want a simple relabel or to reduce them to fewer more populated categories. 
# This type of functionality is more useful in Module 2 / 3
#orig_cats <- c("CAR - DRIVER",
#               "SHOPPING ERRANDS",
#               "CIVIL ERRANDS",
#               "MEDICAL & FITNESS",
#               "CAR - PASSENGER",
#               "CARE GIVING")
#
#new_cats <- c("CAR",
#              "ERRANDS",
#              "ERRANDS",
#              "ERRANDS",
#              "CAR",
#              "ERRANDS")
#
#csv_dict <- recategorize_calendar_items(csv_dict,
#                                        orig_cats = orig_cats,
#                                        new_cats = new_cats)

# For table joining purposes, we must make sure that the cal_item_id column is numeric.
csv_dict$ucalitems$cal_item_id <- as.integer(csv_dict$ucalitems$cal_item_id)


### Format Travel data 
# This step assigns the trip purpose based on the next activity
csv_dict[['ucalitems']] <- assign_trip_purpose(csv_dict[['ucalitems']])

# We check is survey datasets were provided. If so, we process them or add a new variable survey_not_null to False
if(!is.null(csv_dict$calendar_item_survey)){
  csv_dict[['ucalitems_survey']] <- survey_join_id(
    csv_dict[['calendar_item_survey']],
    csv_dict[['ucalitems']])
  
  ### Create Combined Tables
  # join survey table episode data
  csv_dict[['ucalitems_ljoin_ucisurvey']] <- ucalitems_ljoin_ucisurvey(
    ucalitems_survey = csv_dict[['ucalitems_survey']],
    ucalitems = csv_dict[['ucalitems']])
} else{
  csv_dict[['ucalitems_ljoin_ucisurvey']] <- csv_dict[['ucalitems']] %>%
    mutate(survey_not_null = F)
}



### Split Multiday Calendar Items
# This step will split records across multiple days into multiple single-day records, please specify:
#  - local_timezone in your study area
#  - the time defined the new day, e.g., 4 means a new day starts from 4 A.M.. 
# Please use integer with the range [0,6]
# Please note that our testing and development was done with day_start_hour set to 0. We cannot verify that all modules work with modified day_start_time.

csv_dict[['ucalitems_ljoin_ucisurvey_split']] <- split_ucalitems(
  csv_dict[['ucalitems_ljoin_ucisurvey']], 
  local_timezone='US/Central', 
  day_start_hour = 0,
  min_time_stamp = 0)

### Generate Daily Data Quality Summary Metrics
# App interaction summary: for each day of a given person (a.k.a. person-day), summarize:
#   
#   - total: total hours of data
# 
#   - no_off: total hours with device on
#   
#   - interact_with_app: total count of interactions via editing OR confirming
#   
#   - interact_by_confirm: total count of interactions via confirmation ONLY
#   
#   - interact_by_edit: total count of interactions via editing ONLY
#   
#   - with_subtype: total hours with subtype of trip or activity is confirmed
#   
#   - with_survey: total hours with in-app survey filled out
#   
#   - trip_count & trip_duration: total counts and hours of trips
#   
#   - activity_count & activity_duration: total counts and hours of activities
csv_dict[['day_summary']] <- get_per_day_duration(
  csv_dict[['ucalitems_ljoin_ucisurvey_split']])

### Get counts and % of valid days
# A valid day will have a minimum number of hours with user interactions 
# 
# We include 24, 20, 16, 12, and 8 hours for "minimum number of hours" 
# 
# Based on the output table, you can determine which to choose for filtering
count_valid_per_days(day_summary = csv_dict[['day_summary']], 
                     numerator_col = 'with_subtype', 
                     denominator_filter = 'interact_by_confirm>0')

### Filtering out days based on criteria
#This step filters out data that does not meet our acceptance criteria. 
#You can choose one from the following expressions based on your research need:

#choose 'minimum number of hours' to define valid data
minimum_hours = 12

#[Option 1]
# User interact with the app (i.e.,user confirm or edit the calendar item)
query_text = paste0('(interact_with_app>0)&(with_subtype>=',minimum_hours,')')

#[Option 2] 
# User confirm or edit the calendar item
query_text = paste0('((interact_by_confirm>0)|(interact_by_edit>0))&(with_subtype>=',12,')')

#[Option 3] 
# User confirm the calendar item
query_text = paste0('(interact_by_confirm>0)&(with_subtype>=',12,')')

csv_dict_sub = filter_valid_days(csv_dict, query_text)


# Process survey data. This will clean up the format of the survey data to make data analysis easier. 
# Survey data will be provided in Wide format and have a data dictionary to decipher questions. 
csv_dict_sub <- process_surveys(csv_dict_sub)




############################################################
### Data Analysis Module #1: Standard Daynamica Analysis ###
############################################################

#* Within this section we will run the Standard Daynamica Analysis Module. 
#* This module contains a collection of predefined tables and figures which highlight key and interesting summarizations of the Daynamica data provided. 
#* It is intended to provide new an unique insights into the data as well as a starting point for more advanced or nuanced analyses.
#* The exact specifications of these analyses are not the only way to summarize and analyze Daynamica data but
#* rather the most popular and effective methods we regularly use. We highly recommend researchers investigate 
#* the technical details of how each table and figure is generated and whether they meet each analyst's specific research goal. 

#* In order to implement this module, we will use a function called "render." 
#* This function can be thought of as telling R which module to run. 
#* In this case, our module is named "standard_daynamica_analysis_module.Rmd". 

### Module Specifications
output_subfolder <- "primary_analysis" # This tells the module where the results should be saved. This is useful if you are making a couple of reports simultaneously. 
output_location <- paste0("../outputs/", output_subfolder)



export_results <- T # This report generates a collection of results Specifying a T/F tells the module whether you would like results saved as csvs as well as within the report html. 

# Day Level Visualizations
# If you want to have some participant level UCAL visualizations, you need to specify which Id's you want to display. 
# For a more robust plotting of the UCALs, see the module: calendar_item_visualization_module.Rmd below. 
user_id_list <- unique(csv_dict_sub$day_summary$user_id)[1:2] # List of ID's to use in creating daily data summaries

# What do you want the report to be named?
file_name <- "primary_analysis_result" # Don't add the html extension

### Don't touch - This creates the output folder if it hasn't been made yet. 
if(!dir.exists(output_location)){
  dir.create(substr(output_location, 4, nchar(output_location)))
}
long_file_name <- paste0(output_location, "/", file_name, ".html")
### End Don't Touch Section

# Combine the parameters together into a list. 
param_list <- list(# Data Import and Report Headers
                   daynamica_data = csv_dict_sub,
                   report_author = "Daynamica Team",
                   report_title = "Daynamica Summary Report",
                   
                   # Day Level Visualizations
                   export_results = export_results,
                   output_location = output_location,
                   user_id_list = user_id_list)


# This code generates the report via the render function. When it completes, you will be able to view the results in the location you specified previously. 
render("analysis_modules/standard_daynamica_analysis_module.Rmd", output_file = long_file_name,
       params = param_list,
       envir = new.env())



#####################################################
### Data Analysis Module #1.1: Sub Group Analyses ###
#####################################################

#* A common analysis task is to perform a subgroup analysis. In these analyses we may look to see how specific subgroups differ from each other and the general population as a whole. 
#* This type of analysis can be easily performed using these modules. 
#* 
#* Common subgroups that may be of interest include the following:
#*  Demographic information (Sex, Age Stratification, Race, Education, Employment Status/Income etc)
#*  Day Information (Weekday vs Weekend, Month to Month comparisons etc)
#*  
#* Without too much work, individual reports can be easily generated for each subgroup, allowing for side by side comparisons of the statistics, tables, and figures. 
#* Following an introductory review of the subgroup information,
#*  it is very likely that custom statistics, tables, or figures may need to be generated to better characterize and summarize the key information and insights identified in the reports. 
#* 
#* Below is an artificial example of how subgroup reports may be generated.
#* In this case, we will be randomly identifying a collection of participants that we want to focus on. 
#* This can be replicated for a specific sub-group analysis by replacing the random sampling with a list of participants that meet a certain definition like being Female or Income < X. 
#* 
#* 
#######################################
# Valid Example of Sub Group Analysis #
#######################################

# Example Analysis

# List of unique user_ids in our dataset
id_list <- csv_dict_sub$ucalitems_ljoin_ucisurvey_split$user_id %>% unique()

# Identify subgroup via random sampling (This would be different in a real analysis).
subset_list <- sample(id_list, floor(length(id_list)/2))

# subset our dataset to the subgroup using the select_userids function.
subsetted_analysis_dataset <- select_userids(csv_dict_sub, subset_list)

# Specify the location for our new analysis results to go. 
output_subfolder <- "secondary_subset_analysis"
output_location <- paste0("../outputs/", output_subfolder)


subset_param_list <- list(# Data Import and Report Headers
  daynamica_data = subsetted_analysis_dataset,
  report_author = "Daynamica Team",
  report_title = "Daynamica Summary Report - Subset Report",
  
  # Day Level Visualizations
  export_results = export_results,
  output_location = output_location,
  user_id_list = NULL)

file_name <- "primary_analysis_result_subset_analysis" # Don't add the html extension
long_file_name <- paste0(output_location, "/", file_name, ".html")

if(!dir.exists(output_location)){
  dir.create(substr(output_location, 4, nchar(output_location)))
}

### Run Standard Analysis Module using specific subgroups
render("analysis_modules/standard_daynamica_analysis_module.Rmd", output_file = long_file_name,
       params = subset_param_list,
       envir = new.env())


##################################################################
### Data Analysis Module #2: User Calendar Item Visualizations ###
##################################################################

# This section will focus on the generation of the User Calendar Item Visualization Report. 
# It will utilize the same data as Standard Data Analysis Report so we do not need to repeat the data processing in the beginning of this document. 

# Filter the user_id_list to the first 15 users. This is convenient for us because it limits the time it takes for the report to generate. 
# If you generate all participants at once, the computation time may be long and the report may become unwieldy to review. 
# It may be preferable to keep the number of Id's to 50 or less to limit the size and rendering time of the document. 

user_id_list <- csv_dict_sub$day_summary$user_id %>% unique() %>% head(15)

# For this module, we will be relabeling the subtypes to a reduced list. This will help with data visualization. 
new_cats <- c(#"Unknown Activity",  # Activity
  "Motorized Trip",    "Car - Driver",
  "Motorized Trip",    "Car - Passenger",
  "Other",   "Care Giving",
  "Other",   "Civil Errands",
  "Other",   "Community & Cultural",
  "Other",   "Education",
  "Eat Out", "Food & Meal",
  "Other",   "Fun & Leisure",
  "Home",    "Home",
  "Other",   "Medical & Fitness",
  "Other",   "Other Activities",
  "Motorized Trip",    "Other Trips",
  "Motorized Trip",    "Rail",
  "Other",   "Religious & Spiritual",
  "Shop",    "Shopping Errands",
  "Motorized Trip",    "Taxi/Uber/Lyft",
  "Missing",   "Unknown",
  "Motorized Trip",    "Vehicle",
  "Non-Motorized Trip",    "Wait",
  "Non-Motorized Trip",    "Walk",
  "Work",     "Workplace",
  "Non-Motorized Trip",    "Bike",
  "Motorized Trip",    "Bus"
)
recat_subtype_dictionary <- data.frame(matrix(toupper(new_cats), ncol = 2, byrow = T))
names(recat_subtype_dictionary) <- c("new_categories", "orig_categories")

csv_dict_new_cat <- 
  recategorize_calendar_items(csv_dict_sub,
                              orig_cats = recat_subtype_dictionary$orig_categories,
                              new_cats = recat_subtype_dictionary$new_categories)

# Limiting Time of Day displayed on plot.
# Units are in fraction of day. So we are using hours / 24 hours to get fraction of day.  
limit_tod <- c(0, 24)/24 # Display all hours
#limit_tod <- c(4, 24)/24 # Display hours 4 - 24 of the plot. 

# This is how the color codings are assigned within the report. Modify this code or create your own data frame if you 
# want to specify your own colors. 
# unique_activity_labels <-
#   csv_dict_new_cat$ucalitems_temporal_plot %>%
#   tibble() %>%
#   filter(type_decoded == "ACTIVITY") %>%
#   pull(subtype_decoded) %>% unique()
# unique_trip_labels <-
#   csv_dict_new_cat$ucalitems_temporal_plot %>%
#   tibble() %>%
#   filter(type_decoded == "TRIP") %>%
#   pull(subtype_decoded) %>% unique()
# 
# activity_palette <- c(wes_palette("Zissou1", 1),
#                       wes_palette("Zissou1", 5)[4:5],
#                       "#679669", # Green
#                       "#876796", # Purple
#                       "#96676F", # Dark red purple
#                       rev(wes_palette("Royal1", 4)[c(4)]),
#                       wes_palette("Moonrise3", 2),
#                       wes_palette("GrandBudapest2"),
#                       wes_palette("GrandBudapest1"),
#                       wes_palette("Chevalier1", 4))[1:length(unique_activity_labels)]
# 
# trip_palette <- c(palette.colors(n = 8, "ggplot2")[-1],
#                   palette.colors(n = 8, "alphabet"))[1:length(unique_trip_labels)]
# 
# 
# color_codes_activity_data <- data.frame(labels = unique_activity_labels,
#                                         values = activity_palette)
# 
# color_codes_trips_data <- data.frame(labels = unique_trip_labels,
#                                      values = trip_palette)
# 
# color_codings <- list(color_codes_activity_data,
#                       color_codes_trips_data)
export_results <- F
output_location <- "../outputs/ucal_visualization_output"

file_name <- "ucal_visualization_example" # Don't add the html extension
long_file_name <- paste0(output_location, "/", file_name, ".html")

if(!dir.exists(output_location)){
  dir.create(substr(output_location, 4, nchar(output_location)))
}

param_list <- list(# Data Import and Report Headers
  daynamica_data = csv_dict_new_cat,
  report_author = "Daynamica Team",
  report_title = "Daynamica UCAL Visualization Report",
  
  # Day Level Visualizations

  user_id_list = user_id_list,
  limit_tod = limit_tod,
  color_codings = NULL)

render("analysis_modules/calendar_item_visualization_module.Rmd", output_file = long_file_name,
       params = param_list,
       env = new.env())

################################################################
### Data Analysis Module #3: Chord Diagram GIF visualization ###
################################################################

#* An additional data analysis module that we have developed is the Chord Diagram GIF. This allows researchers to see how the 
#* proportion of participant's time is spent in a specific category and how they transition from one activity or trip subtype
#* to another.

#* We have created a copy of an analysis that creates the GIF from start to finish.
#* Alternatively, you may run the analysis within this file using the functions provided below. 
# See file "chord_diagram_constructor_standalone.R"

# Warning, this can take a while to run. Be patient. It must do quite a bit of data processing as well as generate 
# a collection of .pngs to stitch together into a .gif file.  We have kept the call verbose so you 
# can see that progress is being made on the report. Future iterations may include a progress bar. 
#source("standalone_analyses/chord_diagram_constructor_standalone.R", local = myEnv)



# source the helper functions for this module.
source("helper_functions/module_3_chord_diagram_helper_functions.R")
source("helper_functions/s10_convert_to_sequence_data.R")

# We must relabel the categories more to get a better visualization. 
# This contains extra ones that might exist in different datasets but not necessarily your own. 
# If it doesn't exist in your dataset, the relabeling rule is ignored. 
# It matches on expression within a string so BUS must go after Business because otherwise, BUS will be replaced within BUSiness. 

# First "Column" for new category, Second "Column" for current category. 
new_cats <- c(#"Unknown Activity",  # Activity
  "Eat Out", "EAT_OUT",
  "Other",   "LEISURE_RECREATION",
  "Shop",    "SHOP",
  "Work",    "WORK",
  "Other",   "PERSONAL_BUSINESS",
  "Other",   "UNKNOWN_ACTIVITY",
  "Other",   "OTHER",
  "TRIP",    "IN_VEHICLE",
  "TRIP",    "UNKNOWN_TRAVEL_MODE",
  "Trip",    "CAR",
  "Trip",    "Bike",
  "Trip",    "Car - Driver",
  "Trip",    "Car - Passenger",
  "Other",   "Care Giving",
  "Other",   "Civil Errands",
  "Other",   "Community & Cultural",
  "Other",   "Education",
  "Eat Out", "Food & Meal",
  "Other",   "Fun & Leisure",
  "Home",    "Home",
  "Other",   "Medical & Fitness",
  "Other",   "Other Activities",
  "Trip",    "Other Trips",
  "Trip",    "Rail",
  "Other",   "Religious & Spiritual",
  "Shop",    "Shopping Errands",
  "Trip",    "Taxi/Uber/Lyft",
  "Missing",  "Unknown",
  "Trip",    "Vehicle",
  "Trip",    "Wait",
  "Trip",    "Walk",
  "Work",     "Workplace",
  "Trip",    "Bus"
)
recat_subtype_dictionary <- data.frame(matrix(toupper(new_cats), ncol = 2, byrow = T))
names(recat_subtype_dictionary) <- c("new_categories", "orig_categories")

csv_dict_new_cat <- 
  recategorize_calendar_items(csv_dict_sub,
                              orig_cats = recat_subtype_dictionary$orig_categories,
                              new_cats = recat_subtype_dictionary$new_categories)

# Convert the Calendar Items to Sequence Data
sequence_data_list <- generate_sequence_data(csv_dict_new_cat$ucalitems_temporal_plot, unit_length = 5)

# If after data reduction you have more than 7 levels,
# you will need to pick your own colors as the operation below won't work. 
vector_length <- length(sequence_data_list$id_key$letters_subtype)
# Define the colors that will be used for the plots. 
category_data <- data.frame(label = sequence_data_list$id_key$letters_subtype,
                            subtype = sequence_data_list$id_key$subtype_decoded,
                            order = c(1:7)[1:vector_length],
                            color = c("#80B1D3",
                                      "#BC80BD",
                                      "#FB8072",
                                      "#FCCDE5",
                                      "#FDB462",
                                      "#B3DE69",
                                      "#FFFFB3")[1:vector_length]
                            )

# Compute the Flow Rate between Category Types every 15 Minutes. 
# Stratifying by Weekday / Weekend via the data subset parameter.

step_data_15_min_weekday <- 
  flow_rate_calculator(sequence_data_list$wide_sequence_data, split_size = 3,
                       data_subset = wday(sequence_data_list$wide_sequence_data$start_date) %in% c(2:6)) %>%
  process_flow_data()

step_data_15_min_weekend <- 
  flow_rate_calculator(sequence_data_list$wide_sequence_data, split_size = 3,
                       wday(sequence_data_list$wide_sequence_data$start_date) %in% c(1, 7)) %>%
  process_flow_data()

step_data_15_min_all_data <- 
  flow_rate_calculator(sequence_data_list$wide_sequence_data, split_size = 3) %>%
  process_flow_data()

data_list <- list(step_data_15_min_weekday,
                  step_data_15_min_weekend)
data_names = c("Weekdays",
               "Weekends")

# Create a directory to store the figures. 
module_3_results_location <- "./outputs/plot-gif/"
dir.create(module_3_results_location)

module_3_sub_analysis_location <- "./outputs/plot-gif/dow_viz/"


dir.create(module_3_sub_analysis_location)

# Create the individual plots that will be stitched together in the GIF. 
# All files are saved in pre-specified folders and can be used separately for GIF verification or subsequent analyses. 
create_individual_plots(data_list,
                        data_names = data_names,
                        location = module_3_sub_analysis_location,
                        nrow = 1, 
                        ncol = 2,
                        width = 14,
                        height = 7)

# Stitching the pngs into a gif. 
png_files <- paste0(module_3_sub_analysis_location, list.files(path = module_3_sub_analysis_location, pattern = ".png"))

gif_file2 <- paste0(module_3_results_location, "ucal_chord_diagram_visualization_dow_visualization.gif")
gifski(png_files, gif_file2, width = 1600, height = 800, delay = 1.25)


# All data analysis instead of the weekday/weekend plot. 
module_3_sub_analysis_location <- "./outputs/plot-gif/dow_viz/"
dir.create(module_3_sub_analysis_location)

create_individual_plots(list(step_data_15_min_all_data),
                        data_names = "All Data",
                        location = module_3_sub_analysis_location,
                        nrow = 1, 
                        ncol = 1,
                        width = 7,
                        height = 7)

png_files <- paste0(module_3_sub_analysis_location, list.files(path = module_3_sub_analysis_location, pattern = ".png"))
gif_file2 <- paste0(module_3_results_location, "/ucal_chord_diagram_visualization_all_data_visualization.gif")
gifski(png_files, gif_file2, width = 800, height = 800, delay = 1.25)
