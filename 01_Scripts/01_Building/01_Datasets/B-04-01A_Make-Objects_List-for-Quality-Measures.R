# < Description > *
# > Script Group Indicator Number and Name
# # : B-04, XYZ
# #
# > Script Number(s)
# # : B-04-01A
# #
# > Purpose of the script(s)
# # : Make a list that includes an array of applicable quality measure(s) for
# #   each commodity.

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
DIR_TO.LOAD_CATTLE <- paste(PATH_DATA_INTERMEDIATE, "MMN-API/Data", sep = "/")
FILE_TO.LOAD_CATTLE <- "MMN_Data_Category-Cattle.RData"


# # 2. Path(s) to save dataset(s)
# # 2.1. Path for DT(s) including cattle-associated data
DIR_TO.SAVE_QUALITY.MEASURE <-
  paste(PATH_DATA_INTERMEDIATE, "MMN-API/Data", sep = "/")
FILE_TO.SAVE_QUALITY.MEASURE <- "MMN_Data_List-of-Quality-Measures.RData"
PATH_TO.SAVE_QUALITY.MEASURE <- paste(
  DIR_TO.SAVE_QUALITY.MEASURE,
  append_date.to.filename(FILE_TO.SAVE_QUALITY.MEASURE),
  sep = "/"
)


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Make a list incl. an array of applicable quality measure(s) for each commodity
# ------------------------------------------------------------------------------
# ------- Load Report Data -------
load_most.recent.data(DIR_TO.LOAD_CATTLE, FILE_TO.LOAD_CATTLE)


# ------- Create a list of quality measures -------
# # 1. Generate objects that will be used later
cols_product <- c("category", "class", "commodity")
cols_quality <- c(
  "frame", "muscle_grade", "quality_grade_name", "yield_grade", "dressing",
    "pregnancy_stage", "offspring_weight_est"
)
categories <- dt_cattle[, .N, keyby = eval(cols_product[1])]$category
dt_classes <- dt_cattle[, .N, keyby = eval(cols_product[1:2])][, N := NULL]
dt_products <- dt_cattle[, .N, keyby = cols_product][, N := NULL]


# # 2. Make a list of quality measures
# # 2.1. Generate an empty list
list_quality.measures <- as.list(categories) %>% setNames(., categories)

# # 2.2. Append class-related information
for (c in names(list_quality.measures)) {
  tmp_length <- dt_classes[category == c, .N]
  tmp_list <-
    rep(NA, times = tmp_length) %>%
      as.list(.) %>%
      setNames(., dt_classes[category == c]$class)
  list_quality.measures[[c]] <- tmp_list
}

# # 2.3. Append commodity-related information
for (row in 1:dt_classes[, .N]) {
  category_ <- dt_classes[row]$category
  class_ <- dt_classes[row]$class
  tmp_length <- dt_products[category == category_ & class == class_, .N]
  tmp_list <-
    rep(NA, times = tmp_length) %>%
      as.list(.) %>%
      setNames(
        ., dt_products[category == category_ & class == class_]$commodity
      )
  list_quality.measures[[category_]][[class_]] <- tmp_list
}

# # 2.4. Append qaulity-measure-related information
for (row in 1:dt_products[, .N]) {
  category_ <- dt_products[row]$category
  class_ <- dt_products[row]$class
  commodity_ <- dt_products[row]$commodity

  tmp_product <- dt_cattle[
    category == category_ & class == class_ & commodity == commodity_,
    .N,
    keyby = cols_quality
  ][, N := NULL]
  tmp_quality.measures <-c(NULL)
  for (qm in names(tmp_product)) {
    if (tmp_product[!is.na(get(qm)), .N, keyby = qm][, .N] > 0) {
      tmp_quality.measures <- c(tmp_quality.measures, qm)
    }
  }
  list_quality.measures[[category_]][[class_]][[commodity_]] <-
    tmp_quality.measures
}


# ------- Save the list created -------
save(list_quality.measures, file = PATH_TO.SAVE_QUALITY.MEASURE)
