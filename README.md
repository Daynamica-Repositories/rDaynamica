# rDaynamica: Standard Analysis Tools for Daynamica Smartphone App Data

An R package designed to process, analyze, and visualize data collected from the Daynamica smartphone application. The package provides researcher-facing, user-friendly functions and automated report generation modules.

---

## Getting Started

### Installation

You can install `rDaynamica` directly in R using `devtools` or `remotes`:

```r
# install.packages("pak")
pak::pak("Daynamica-Repositories/rDaynamica")
```

Or load the functions locally during research development:

```r
library(rDaynamica)
```

---

## Quickstart Example

Running a full Daynamica data analysis pipeline is straightforward. See `examples/master_analysis_file.R` for a complete script.

```r
library(rDaynamica)

# 1. Import Daynamica datasets from CSV directory (sample data ships with the package)
data_path <- system.file("extdata", "anonymized_data", package = "rDaynamica")
csv_dict <- path2dict(folder_path = data_path, file_prefix = "anonymized_")

# 2. Preprocess & Clean Data
csv_dict <- ucal_items_version_updater(csv_dict)
csv_dict$ucalitems$cal_item_id <- as.integer(csv_dict$ucalitems$cal_item_id)
csv_dict[['ucalitems']] <- assign_trip_purpose(csv_dict[['ucalitems']])

# Join calendar item survey responses
csv_dict[['ucalitems_survey']] <- survey_join_id(csv_dict[['calendar_item_survey']], csv_dict[['ucalitems']])
csv_dict[['ucalitems_ljoin_ucisurvey']] <- ucalitems_ljoin_ucisurvey(csv_dict[['ucalitems_survey']], csv_dict[['ucalitems']])

# 3. Split multiday items & filter valid days (>= 12 hours)
csv_dict[['ucalitems_ljoin_ucisurvey_split']] <- split_ucalitems(
  csv_dict[['ucalitems_ljoin_ucisurvey']],
  local_timezone = 'US/Central',
  day_start_hour = 0
)
csv_dict[['day_summary']] <- get_per_day_duration(csv_dict[['ucalitems_ljoin_ucisurvey_split']])
csv_dict_sub <- filter_valid_days(csv_dict, query_text = "(interact_with_app>0)&(with_subtype>=12)")

# 4. Group subtypes into a few categories (required for the chord diagram)
recat <- c(
  "CAR - DRIVER" = "MOTORIZED TRIP", "CAR - PASSENGER" = "MOTORIZED TRIP",
  "OTHER TRIPS" = "MOTORIZED TRIP", "RAIL" = "MOTORIZED TRIP", "BUS" = "MOTORIZED TRIP",
  "TAXI/UBER/LYFT" = "MOTORIZED TRIP", "VEHICLE" = "MOTORIZED TRIP",
  "WAIT" = "NON-MOTORIZED TRIP", "WALK" = "NON-MOTORIZED TRIP", "BIKE" = "NON-MOTORIZED TRIP",
  "HOME" = "HOME", "WORKPLACE" = "WORK", "FOOD & MEAL" = "EAT OUT",
  "SHOPPING ERRANDS" = "SHOP", "UNKNOWN" = "MISSING",
  "CARE GIVING" = "OTHER", "CIVIL ERRANDS" = "OTHER", "COMMUNITY & CULTURAL" = "OTHER",
  "EDUCATION" = "OTHER", "FUN & LEISURE" = "OTHER", "MEDICAL & FITNESS" = "OTHER",
  "OTHER ACTIVITIES" = "OTHER", "RELIGIOUS & SPIRITUAL" = "OTHER"
)
csv_dict_new_cat <- recategorize_calendar_items(csv_dict_sub, orig_cats = names(recat), new_cats = unname(recat))

# 5. Run Analysis Modules
run_standard_analysis(csv_dict_sub, output_dir = "outputs/primary_analysis")
run_visualization_report(csv_dict_new_cat, output_dir = "outputs/ucal_visualization")
generate_chord_diagram_gif(csv_dict_new_cat, output_dir = "outputs/plot-gif")
run_activity_chains_report(csv_dict_sub, output_dir = "outputs/Module_4")
```

---

## Analysis Modules Summary

1. **`run_standard_analysis()`**: Generates an HTML report containing dataset summary statistics, active user plots, valid days breakdown, and activity/trip duration tables.
2. **`run_visualization_report()`**: Produces detailed multi-day participant timeline plots of calendar items.
3. **`generate_chord_diagram_gif()`**: Constructs animated chord diagram GIFs showcasing transition flow rates between activities/trips over time.
4. **`run_activity_chains_report()`**: Tutorial and analysis report for extracting activity sequence chains (e.g. `H -> T -> W -> T -> H`) and generating Sankey diagrams.

---

## Working with Survey Responses

Daynamica collects two kinds of surveys: calendar item surveys about specific activities and trips, and end-of-day (EMA) surveys.

```r
# One clean row per answer: multi-select options as a list, ratings as numbers
cis_tidy <- tidy_calendar_item_survey(csv_dict_sub$calendar_item_survey)
ema_tidy <- tidy_ema_survey(csv_dict_sub$ema_survey)

# Every question with the options chosen, response counts, and shares
survey_codebook(cis_tidy)

# Wide tables (one row per surveyed item or EMA submission), and survey
# answers joined to each activity and trip
csv_dict_sub <- process_surveys(csv_dict_sub)
csv_dict_sub$ucal_items_temporal_plot_with_survey_data
```

---

## Package Structure

- **`R/`**: Package functions (`io.R`, `preprocess.R`, `validation.R`, `summary.R`, `temporal_plot.R`, `sequence.R`, `chord_diagram.R`, `survey.R`, `modules.R`).
- **`tests/`**: Automated tests (`testthat`).
- **`inst/rmd/`**: Standalone RMarkdown analysis module templates.
- **`inst/extdata/`**: Sample anonymized dataset for testing and quickstart tutorials.
- **`examples/`**: `master_analysis_file.R` showing example workflow execution.

---

## Contribution Statement

Developed by Andrew Becker, Yaxuan Zhang, Yingling Fan, and Ying Song at the University of Minnesota.
