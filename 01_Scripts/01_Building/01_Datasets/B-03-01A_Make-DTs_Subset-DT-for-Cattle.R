# < Description > *
# > Script Group Indicator Number and Name
# # : B-03, Make-DTs
# #
# > Script Number(s)
# # : B-03-01A
# #
# > Purpose of the script(s)
# # : Subset the DT for `category == "Cattle"` to make a DT for R shiny app.

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(stringr)
library(zoo)
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
# # 1.1. Path for the DT including cattle-related data
DIR_TO.LOAD_CATTLE <- paste(PATH_DATA_INTERMEDIATE, "MMN-API/Data", sep = "/")
FILE_TO.LOAD_CATTLE <- "MMN_Data_Category-Cattle.RData"


# # 2. Path(s) to save dataset(s)
# # 2.1. Path for the DT subsetted
DIR_TO.SAVE_CATTLE <- paste(PATH_DATA_INTERMEDIATE, "MMN-API/Data", sep = "/")
FILE_TO.SAVE_CATTLE <- "MMN_Data_Category-Cattle_For-Shiny-App.RData"
PATH_TO.SAVE_CATTLE <- paste(
  DIR_TO.SAVE_CATTLE,
  append_date.to.filename(FILE_TO.SAVE_CATTLE),
  sep = "/"
)


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Subset the DT to create a DT for a R shiny app
# ------------------------------------------------------------------------------
# ------- Load data required -------
load_most.recent.data(DIR_TO.LOAD_CATTLE, FILE_TO.LOAD_CATTLE)
dt_cattle_whole <- copy(dt_cattle)
rm(list = "dt_cattle")


# ------- Modify the DT loaded -------
# # 1. Create objects that will be used later
cols_product <- c("category", "class", "commodity")
cols_quality <- c(
  "frame", "muscle_grade", "quality_grade_name", "yield_grade", "dressing",
    "pregnancy_stage", "offspring_weight_est"
)
cols_month <- c("market_type_category", cols_product, cols_quality)
cols_reports_to.subset <- c(
  "brackets", "price_unit", "market_location", cols_month
)


# # 2. Add data fields that will be used to subset the DT
dt_cattle_whole[
  ,
  brackets := cut(
    avg_weight,
    breaks = seq(0, 10^4, by = 50),
    include.lowest = TRUE,
    dig.lab = as.character(10^4) %>% str_length(.)
  ),
  by = eval(cols_reports_to.subset[-1])
]
dt_cattle_whole[
  ,
  `:=` (
    report_date_earliest = min(report_date, na.rm = TRUE),
    report_date_latest = max(report_date, na.rm = TRUE)
  ),
  by = cols_month
]
dt_cattle_whole[
  ,
  months :=
    ((report_date_latest - report_date_earliest) / (365/12)) %>%
      floor(.) %>%
      as.numeric(.)
]
dt_cattle_whole[
  ,
  n_reports := .N,
  by = cols_reports_to.subset
]
dt_cattle_whole[
  ,
  n_per.month := floor(n_reports / months)
]


# ------- Subset the DT, and then modify it -------
# # 1. Subset the DT
dt_cattle <- dt_cattle_whole[
  paste0("!is.na(", cols_quality, ")") %>% paste(., collapse = " | ") %>%
    parse(text = .) %>%
    eval(.)
  # To drop observations with no quality-measure-related information
][
  12 <= months & !is.infinite(months)
][
  n_per.month >= 1
]


# # 2. Modify the subsetted DT
# # 2.1. Drop unnecessary data fields
cols_to.drop <- c(
  "report_date_earliest", "report_date_latest", "months", "n_reports",
  "n_per.month", "brackets"
)
dt_cattle[, (cols_to.drop) := NULL]


# ------- Save the DT created in .RData format -------
save(dt_cattle, file = PATH_TO.SAVE_CATTLE)
