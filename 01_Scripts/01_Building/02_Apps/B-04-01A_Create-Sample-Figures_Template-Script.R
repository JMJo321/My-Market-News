# < Description > *
# > Script Group Indicator Number and Name
# # : B-04, Create Sample Figures
# #
# > Script Number(s)
# # : B-04-01A
# #
# > Purpose of the script(s)
# # : (...)

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(stringr)
library(ggplot2)
library(data.table)
library(zoo)
library(lubridate)


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

# # 1.2. Path for the script including functions that will be used below
DIR_TO.LOAD_SCRIPT <- paste(PATH_SCRIPTS, "01_Building/02_Apps", sep = "/")
FILE_TO.LOAD_SCRIPT <- "B-04-01A-1_Create-Sample-Figures_Functions-only.R"
PATH_TO.LOAD_SCRIPT <- paste(DIR_TO.LOAD_SCRIPT, FILE_TO.LOAD_SCRIPT, sep = "/")


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Load necessary dataset(s) and/or script(s)
# ------------------------------------------------------------------------------
# ------- Load dataset(s) -------
load_most.recent.data(DIR_TO.LOAD_CATTLE, FILE_TO.LOAD_CATTLE)


# ------- Load script(s) -------
source(PATH_TO.LOAD_SCRIPT)


# ------------------------------------------------------------------------------
# Create sample figure(s): category == "Cattle" & class == "Heifers"
# ------------------------------------------------------------------------------
# ------- For commodity == "Feeder Cattle" -------
# # 1. Set options
# # 1.1. Set options
# # 1.1.1. For subsetting observations
# # 1.1.1.1. About market type
select_market.type.category <- "Auction"
# # 1.1.1.2. About commodity
select_category <- "Cattle"
select_class <- "Heifers"
select_commodity <- "Feeder Cattle"
# # 1.1.1.3. About quality
select_frame <- "Medium and Large"
select_muscle.grade <- "1"
select_quality.grade.name <- NA
select_yield.grade <- NA
select_dressing <- NA
select_pregnancy.state <- NA
select_offspring.weight.est <- NA
# # 1.1.1.4. About market location
select_market.location.state <- "FL"
select_market.location.city <- "Arcadia"
# # 1.1.1.5. About weight bracket
select_weight.bracket <- "(400,500]"

# # 1.1.2. For making ggplot object(s)
# # 1.1.2.1. About the interval length for weight brackets
select_interval.length <- 50
# # 1.1.2.2. About the time period
select_period_begin <- as.yearmon("Jan 2019")
select_period_end <- as.yearmon("Dec 2019")
# # 1.1.2.3. About variables displayed in figure(s)
select_qty <- "qty_headcount"
select_price <- "price_avg_weighted"
# # 1.1.2.4. About aggregate variable displayed in figure(s)
select_agg.qty <- "agg.qty_headcount"
select_agg.price <- "agg.price_avg_weighted"

# # 1.2. Make lists by using the options selected
list_select_base <- list(
  market_type_category = select_market.type.category,
  category = select_category,
  class = select_class,
  commodity = select_commodity,
  frame = select_frame,
  muscle_grade = select_muscle.grade,
  quality_grade_name = select_quality.grade.name,
  yield_grade = select_yield.grade,
  dressing = select_dressing,
  pregnancy_state = select_pregnancy.state,
  offspring_weight_est = select_offspring.weight.est
)
list_select_market.location <- list(
  market_location_state = select_market.location.state,
  market_location_city = select_market.location.city
)
list_select_weight.bracket <- list(
  weight_bracket = select_weight.bracket
)


# # 2. Create objects necessary for making figure(s)
# # 2.1. Create DT(s)
dt_base <- get_DT_commodity(dt_cattle, list_select_base)
dt_for.plot_selected.market.location_melted <-
  get_DT_for.plot_selected.market.location(dt_base, list_select_market.location)
dt_for.plot_selected.weight.bracket_melted <-
  get_DT_for.plot_selected.weight.bracket(dt_base, list_select_weight.bracket)

# # 2.2. Create ggplot object(s)
# # 2.2.1. Generate objects that are required to create ggplot object(s)
# # 2.2.1.1. About market location(s) displayed in figure(s)
derived.list_market.location_selected.weight.bracket <-
  get_market.loctions(dt_base)
select_market.location <-
  derived.list_market.location_selected.weight.bracket[3:7]
# # 2.2.1.2. About weight bracket(s) displayed in figure(s)
derived.list_weight.bracket_selected.market.location <-
  dt_for.plot_selected.market.location_melted[
    , .N, keyby = .(weight_bracket)
  ][
    , N := NULL
  ]$weight_bracket
select_derived.weight.bracket <-
  derived.list_weight.bracket_selected.market.location[3:9]
# # 2.2.2. Create ggpolot object(s)
plot_selected.market.location <- get_ggplot.obj_selected.market.location(
  dt_for.plot_selected.market.location_melted,
  select_period_begin, select_period_end,
  select_qty, select_price,
  select_derived.weight.bracket
)
plot_selected.weight.bracket <- get_ggplot.obj_selected.weight.bracket(
  dt_for.plot_selected.weight.bracket_melted,
  select_period_begin, select_period_end,
  select_qty, select_price,
  select_market.location
)
plot_selected.weight.bracket_agg.info <-
  get_ggplot.obj_selected.market.location_agg.info(
    dt_for.plot_selected.weight.bracket_melted,
    select_period_begin, select_period_end,
    select_agg.qty, select_agg.price,
    select_market.location
  )


# ------- For commodity == "Slaughter Cattle" -------
# # 1. Set options
# # 1.1. Set options
# # 1.1.1. For subsetting observations
# # 1.1.1.1. About market type
select_market.type.category <- "Auction"
# # 1.1.1.2. About commodity
select_category <- "Cattle"
select_class <- "Heifers"
select_commodity <- "Slaughter Cattle"
# # 1.1.1.3. About quality
select_frame <- NA
select_muscle.grade <- NA
select_quality.grade.name <- "Choice"
select_yield.grade <- "2-3"
select_dressing <- "Average"
select_pregnancy.state <- NA
select_offspring.weight.est <- NA
# # 1.1.1.4. About market location
select_market.location.state <- "PA"
select_market.location.city <- "New Holland"
# # 1.1.1.5. About weight bracket
select_weight.bracket <- "(1300,1400]"

# # 1.1.2. For making ggplot object(s)
# # 1.1.2.1. About the interval length for weight brackets
select_interval.length <- 100
# # 1.1.2.2. About the time period
select_period_begin <- as.yearmon("Jan 2019")
select_period_end <- as.yearmon("Dec 2019")
# # 1.1.2.3. About variables displayed in figure(s)
select_qty <- "qty_headcount"
select_price <- "price_avg_weighted"
# # 1.1.2.4. About aggregate variable displayed in figure(s)
select_agg.qty <- "agg.qty_headcount"
select_agg.price <- "agg.price_avg_weighted"

# # 1.2. Make lists by using the options selected
list_select_base <- list(
  market_type_category = select_market.type.category,
  category = select_category,
  class = select_class,
  commodity = select_commodity,
  frame = select_frame,
  muscle_grade = select_muscle.grade,
  quality_grade_name = select_quality.grade.name,
  yield_grade = select_yield.grade,
  dressing = select_dressing,
  pregnancy_state = select_pregnancy.state,
  offspring_weight_est = select_offspring.weight.est
)
list_select_market.location <- list(
  market_location_state = select_market.location.state,
  market_location_city = select_market.location.city
)
list_select_weight.bracket <- list(
  weight_bracket = select_weight.bracket
)


# # 2. Create objects necessary for making figure(s)
# # 2.1. Create DT(s)
dt_base <- get_DT_commodity(dt_cattle, list_select_base)
dt_for.plot_selected.market.location_melted <-
  get_DT_for.plot_selected.market.location(dt_base, list_select_market.location)
dt_for.plot_selected.weight.bracket_melted <-
  get_DT_for.plot_selected.weight.bracket(dt_base, list_select_weight.bracket)

# # 2.2. Create ggplot object(s)
# # 2.2.1. Generate objects that are required to create ggplot object(s)
# # 2.2.1.1. About market location(s) displayed in figure(s)
derived.list_market.location_selected.weight.bracket <-
  get_market.loctions(dt_base)
select_market.location <-
  derived.list_market.location_selected.weight.bracket[1:7]
# # 2.2.1.2. About weight bracket(s) displayed in figure(s)
derived.list_weight.bracket_selected.market.location <-
  dt_for.plot_selected.market.location_melted[
    , .N, keyby = .(weight_bracket)
  ][
    , N := NULL
  ]$weight_bracket
select_derived.weight.bracket <-
  derived.list_weight.bracket_selected.market.location
# # 2.2.2. Create ggpolot object(s)
plot_selected.market.location <- get_ggplot.obj_selected.market.location(
  dt_for.plot_selected.market.location_melted,
  select_period_begin, select_period_end,
  select_qty, select_price,
  select_derived.weight.bracket
)
plot_selected.weight.bracket <- get_ggplot.obj_selected.weight.bracket(
  dt_for.plot_selected.weight.bracket_melted,
  select_period_begin, select_period_end,
  select_qty, select_price,
  select_market.location
)
plot_selected.weight.bracket_agg.info <-
  get_ggplot.obj_selected.market.location_agg.info(
    dt_for.plot_selected.weight.bracket_melted,
    select_period_begin, select_period_end,
    select_agg.qty, select_agg.price,
    select_market.location
  )
