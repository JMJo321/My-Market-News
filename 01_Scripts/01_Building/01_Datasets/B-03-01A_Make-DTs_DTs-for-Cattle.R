# < Description > *
# > Script Group Indicator Number and Name
# # : B-03, Make-DTs
# #
# > Script Number(s)
# # : B-03-01A
# #
# > Purpose of the script(s)
# # : Make DT(s) for `category == "Cattle"`

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
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
# # 1.1. Path for Report Data
DIR_TO.LOAD_SUMMARY <-
  paste(PATH_DATA_INTERMEDIATE, "MMN-API/Reports", sep = "/")
FILE_TO.LOAD_SUMMARY <- "MMN_Reports.RData"


# # 2. Path(s) to save dataset(s)
# # 2.1. Path for DT(s) including cattle-associated data
DIR_TO.SAVE_CATTLE <- paste(PATH_DATA_INTERMEDIATE, "MMN-API/Data", sep = "/")
FILE_TO.SAVE_CATTLE <- "MMN_Data_Category-Cattle.RData"
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
# Create DT(s) from Report Data
# ------------------------------------------------------------------------------
# ------- Load Report Data -------
load_most.recent.data(DIR_TO.LOAD_SUMMARY, FILE_TO.LOAD_SUMMARY)


# ------- Create DT(s) from report data -------
# # 1. Create DT(s) by extracting reports with `category == "Cattle"` only
slug.names_cattle <-
  dt_summary_appended[
    category %in% c("Cattle", "Sheep", "Goats", "Swine")
  ]$slug_name
# ## Note:
# ## Several reports with `category == "Cattle"` also include info. about
# ## not only sheep/goats/swine.
dt_cattle_raw <-
  dts_report_renamed[slug.names_cattle] %>%
    rbindlist(.)


# # 2. Drop unnecessary row(s) and column(s)
# # 2.1. Drop unnecessary row(s)
# # 2.1.1. For `final_ind`
dt_cattle_raw[, .N, keyby = .(final_ind)]
# ## Note:
# ## Drop observations with `final_ind == "Preliminary"`. This work will be
# ## completed below.

# # 2.2. Drop unnecessary column(s)
# # 2.2.1. Create DT(s) by dropping columns including comments
cols.by <-
  names(dt_cattle_raw)[
    str_detect(names(dt_cattle_raw), "(narrative)|(comments)", negate = TRUE)
  ]
dt_cattle <- dt_cattle_raw[
  , .N, by = cols.by  # To drop duplicated observations
][
  , N := NULL  # To drop a unnecessary column
][
  final_ind != "Preliminary"  # To drop "preliminary" info.
]
# # 2.2.2. For `weight_break_low` and `weight_break_high`
dt_cattle[, .N, keyby = .(weight_break_low, weight_break_high)]
# ## Note:
# ## Those columns only contain NAs.
dt_cattle[, `:=` (weight_break_low = NULL, weight_break_high = NULL)]


# # 3. Check the primary key(s) of the DT(s) created above
cols <- c(
  "slug_name", "published_datetime", "class", "commodity", "frame",
  "muscle_grade", "quality_grade_name", "lot_desc", "age", "pregnancy_stage",
  "weight_collect", "offspring_weight_est", "yield_grade", "head_count",
  "price_unit", "avg_weight", "avg_price_min", "avg_price"
)
stopifnot(dt_cattle[, .N, by = cols][N > 1, .N] == 0)


# ------- Modify the DT(s) created above -------
# # 1. Change values
# # 1.1. For NA-related values
# # 1.1.1. Create objects that will be used later
# # 1.1.1.1. Identify each column's data type
col.names <- names(dt_cattle)
names(col.names) <- names(dt_cattle)
col.types <- lapply(
  col.names,
  function (col) {
    dt_cattle[, .N, by = col][, .SD, .SDcols = col] %>%
      unlist(.) %>%
      typeof(.)
  }
)
# # 1.1.1.2. Replace values having NA in character with NA
cols_character <- col.types[col.types == "character"] %>% names(.)
reg.exp_na <- "(N/A)|(NA)"
for (col in cols_character) {
  dt_cattle[str_detect(get(col), reg.exp_na), (col) := NA]
}


# # 2. Change columns' data types
# # 2.1. From character to date(time)
# # 2.1.1. For date-related columns
cols_date <- names(dt_cattle)[str_detect(names(dt_cattle), "date$")]
for (col in cols_date) {
  dt_cattle[, (col) := as.Date(get(col), format = "%m/%d/%Y")]
}
# # 2.1.2. For datetime-related columns
cols_datetime <- names(dt_cattle)[str_detect(names(dt_cattle), "datetime$")]
for (col in cols_datetime) {
  dt_cattle[, (col) := as.POSIXct(get(col), format = "%m/%d/%Y %H:%M:%OS")]
}

# # 2.2. From character to numeric
# # 2.2.1. For average-weight-related columns
cols_avg.weight <- names(dt_cattle)[str_detect(names(dt_cattle), "avg_weight")]
for (col in cols_avg.weight) {
  dt_cattle[, (col) := as.numeric(get(col))]
}


# # 3. Add column(s)
# # 3.1. Add a column incl. the publication date
dt_cattle[, published_date := as.Date(published_datetime)]


# # 4. Change the order of columns
col.orders <- c(
  # ## Report-related
  "slug_id", "slug_name", "report_title",
  "published_datetime", "published_date",
  "report_date", "report_begin_date", "report_end_date", "final_ind",
  # ## Market-related
  "market_type_category", "market_type",
  "market_location_name", "market_location_city", "market_location_state",
  "office_code", "office_name", "office_city", "office_state",
  # ## Commodity-related
  "group", "category", "class", "commodity",
  # ## Summary-related
  "receipts", "receipts_week_ago", "receipts_year_ago",
  # ## Characteristic-related
  "age", "lot_desc",
  "frame", "muscle_grade", "quality_grade_name", "yield_grade", "dressing",
  "pregnancy_stage", "offspring_weight_est",
  # ## Weight-related
  "head_count", "weight_collect",
  "avg_weight_min", "avg_weight_max", "avg_weight",
  # ## Price-related
  "freight", "price_unit", "avg_price_min", "avg_price_max", "avg_price"
)
setcolorder(dt_cattle, col.orders)


# ------------------------------------------------------------------------------
# Save the DT(s) created above
# ------------------------------------------------------------------------------
# ------- Export the DT(s) in .RData format -------
save(dt_cattle, file = PATH_TO.SAVE_CATTLE)
