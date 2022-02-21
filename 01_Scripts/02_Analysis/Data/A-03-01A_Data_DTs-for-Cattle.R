# < Description > *
# > Script Group Indicator Number and Name
# # : A-03, Data
# #
# > Script Number(s)
# # : A-03-01A
# #
# > Purpose of the script(s)
# # : Study about the dataset including reports with `category == "Cattle"`.

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(ggplot2)
library(zoo)
library(stringr)
library(data.table)


# ------------------------------------------------------------------------------
# Set working directory, and run header script
# ------------------------------------------------------------------------------
# ------- Set project name -------
PROJ.NAME <- "My-Market-News"


# ------- Set working directory -------
PATH_PROJ <-
  paste("/Users/jmjo/Dropbox/00_JMJo/Projects", PROJ.NAME, sep = "/")
setwd(PATH_PROJ)


# ------- Run the header script -------
PATH_HEADER <- paste0("01_Scripts/H-", PROJ.NAME, ".R")
source(PATH_HEADER)


# ------------------------------------------------------------------------------
# Define path(s), parameter(s) and function(s)
# ------------------------------------------------------------------------------
# ------- Define path(s) -------
# # 1. Path(s) to load necessary script(s) and dataset(s)
# # 1.1. Path for cattle-associated data
DIR_TO.LOAD_CATTLE <- paste(PATH_DATA_INTERMEDIATE, "MMN-API/Data", sep = "/")
FILE_TO.LOAD_CATTLE <- "MMN_Data_Category-Cattle.RData"


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Conduct simple tests to understand the data
# ------------------------------------------------------------------------------
# ------- Load .RData file(s) -------
load_most.recent.data(DIR_TO.LOAD_CATTLE, FILE_TO.LOAD_CATTLE)


# ------- Conduct simple tests -------
# # 1. To understand about date-related columns
# # 1.1. Figure out the relationship between two date-related columns
dt_cattle[published_date < report_date, .N] == 0
dt_cattle[report_end_date < report_begin_date, .N] == 0
dt_cattle[report_end_date < report_date, .N] == 0
dt_cattle[report_date < report_begin_date, .N] == 0

# # 1.2. For `report_begin_date` and `report_end_date`
dt_cattle[
  report_begin_date == report_end_date, .N, keyby = .(market_type_category)
]
dt_cattle[
  report_begin_date != report_end_date, .N, keyby = .(market_type_category)
]


# # 2. Check the number of unique values in each column
sapply(
  names(dt_cattle),
  function (col) dt_cattle[, .N, keyby = col][, .N]
)


# ------- Conduct simple tests -------
# # 1. Create ggplot object(s)
cols_by <- c(
  "category", "class", "commodity", "frame", "muscle_grade",
  "offspring_weight_est", "quality_grade_name", "yield_grade",
  "weight_collect", "age"
)
list_values <-
  dt_cattle[, .N, keyby = cols_by][, N := NULL][6] %>%
    as.list()
subset.condition <- get_subset.condition_in.str(
  c(
    list_values[1:(length(list_values) - 1)],
    list(market_type_category = "Summary", office_state = "MO")
  )
)
sapply(
  names(dt_cattle),
  function (col) {
    dt_cattle[eval(parse(text = subset.condition)), .N, keyby = col][, .N]
  }
)
plot_sample <-
  ggplot(
    data = dt_cattle[eval(parse(text = subset.condition))]
  ) +
    aes(
      x = report_date, y = avg_price,
      color = get("age"), shape = get("pregnancy_stage")
    ) +
    geom_point() +
    # geom_line() +
    scale_y_continuous(label = scales::comma) +
    labs(
      x = "", y = "Average Price\n", color = "Age", shape = "Pregnancy Stage"
    ) +
    theme_linedraw()


# # 2.
cols_by <- c(
  "category", "class", "commodity", "frame",
  "muscle_grade", "quality_grade_name", "lot_desc", "age", "pregnancy_stage",
  "weight_collect", "offspring_weight_est", "yield_grade"
)
dt_cattle[, .N, keyby = cols_by] %>% View(.)
