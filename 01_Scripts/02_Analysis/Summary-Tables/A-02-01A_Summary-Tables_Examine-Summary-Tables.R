# < Description > *
# > Script Group Indicator Number and Name
# # : A-02, Summary-Tables
# #
# > Script Number(s)
# # : A-02-01A
# #
# > Purpose of the script(s)
# # : Examine the summary tables created from the script "B-02-02B".

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(huxtable)
library(tidyverse)
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
DIR_TO.LOAD_SUMMARY <-
  paste(PATH_DATA_INTERMEDIATE, "MMN-API/Reports", sep = "/")
FILE_TO.LOAD_SUMMARY <- "MMN_Reports.RData"



# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# # 1. To count the number of observations satisfying conditions
get_count <- function (conditions_in.str, cols_in.str) {
  dt_summary_appended[
    eval(parse(text = conditions_in.str)),
    .N,
    by = cols_in.str
  ][
    ,
    .N
  ]
}


# ------------------------------------------------------------------------------
# Do research on the summary tables
# ------------------------------------------------------------------------------
# ------- Load data set(s) -------
# # 1. Load a .RData file
load_most.recent.data(DIR_TO.LOAD_SUMMARY, FILE_TO.LOAD_SUMMARY)


# # 2. To understand about reports
dt_summary_appended[
  , .N, keyby = .(category, slug_name)
][
  , .N, keyby = .(category)
]
# ## Note:
# ## 1) Need to examine reports with `is.na(category) == TRUE`.
# ## 2) Reports with `category == "Cattle"` need to be investigated.


# ------- Study about reports with `is.na(category) == TRUE` -------
# # 1. To understand about the reports
# # 1.1. Check the number of reports
dt_summary_appended[is.na(category), .N, by = .(slug_name)][, .N]
dt_summary_appended[
  is.na(category) & is.na(report_title), .N, by = .(slug_name)
][
  , .N
]
# ## Note:
# ## There are no date for those report.
dt_summary_appended[
  is.na(category) & !is.na(report_title), .N, by = .(slug_name)
][
  , .N
]
# ## Note:
# ## This result implies that we might get some useful information about
# ## data contained in those reports from their titles.


# # 2. Check whether inferring useful info. from report titles
# # 2.1. Create objects incl. regular expressions
reg.exp_truck.rate <- "(Truck\\sRate)"
reg.exp_grain <- "(Grain)|(grain)|(Rice)|(Bean)|(Wheat)"
reg.exp_livestock <- "(Livestock)|(Cattle)|(Pig)|(Sheep)|(Cow)"
reg.exp_hay <- "(Hay)"
reg.exp_ornamental <- "(Ornamental)"
reg.exp_sunflower <- "(Sunflower)"
reg.exp_export <- "(Export)"
reg.exp_shipping <- "(Shipping)"
reg.exp_etc <- "(Internet)|(Video)"
reg.exp <- paste(
  reg.exp_truck.rate, reg.exp_grain, reg.exp_livestock, reg.exp_hay,
  reg.exp_ornamental, reg.exp_sunflower, reg.exp_export, reg.exp_shipping,
  reg.exp_etc,
  sep = "|"
)

# # 2.2. Analysis 1:
# # whether those regular expressions created are enough to extract useful
# # infromation (especially, commodities discussed in reports) from report
# # titles
dt_summary_appended[
  is.na(category) & !is.na(report_title) &
    str_detect(report_title, reg.exp, negate = TRUE),
  .N,
  by = .(slug_name, report_title)
]
# ## Note:
# ## Those regular expressions cover every keyword of report titles.

# # 2.3. Analysis 2:
# # Check the number of reports that are identified by each regular expression
reg.exps <- c(
  reg.exp_truck.rate, reg.exp_grain, reg.exp_livestock, reg.exp_hay,
  reg.exp_ornamental, reg.exp_sunflower, reg.exp_export, reg.exp_shipping,
  reg.exp_etc
)
names(reg.exps) <- c(
  "Truck Rate", "Grain", "Livestock", "Hay",
  "Ornamental", "Sunflower", "Export", "Shipping",
  "Etc"
)
count_by.reg.exp <- lapply(
  reg.exps,
  function (x) {
    dt_summary_appended[
      is.na(category) & !is.na(report_title) &
        str_detect(report_title, x),
      .N,
      by = .(slug_name)
    ][
      ,
      .N
    ]
  }
)
count_by.reg.exp
count_by.reg.exp %>% unlist(.) %>% sum(.)
# ## Note:
# ## This result implies that there are observations that are identified
# ## mulitiple regression expressions.

dt_summary_appended[
  is.na(category) & !is.na(report_title) &
    str_detect(report_title, reg.exp_livestock),
  .N,
  by = .(slug_name)
][
  , .N
]
dt_summary_appended[
  is.na(category) & !is.na(report_title) &
    str_detect(report_title, reg.exp_livestock) &
    str_detect(report_title, reg.exp_etc),
  .N,
  by = .(slug_name)
][
  , .N
]
dt_summary_appended[
  is.na(category) & !is.na(report_title) &
    str_detect(report_title, reg.exp_etc),
  .N,
  by = .(slug_name)
][
  , .N
]
# ## Note:
# ## 140
# ## = 106 + 34
# ## = (# of reports)
# ##    + (# of reports identified with `reg.exp_etc` and `reg.exp_livestock`)

# # 2.4. Analysis 3:
# # Examine the kinds of commodity
sapply(
  reg.exps,
  function (x)
    dt_summary_appended[
      is.na(category) & !is.na(report_title) &
        str_detect(report_title, x),
      .N,
      keyby = .(commodity)
    ][, .N]
)
# ## Note:
# ## The reports regarding truck rates cover many kinds of commodity.


# ------- Study about reports with `category == "Cattle"` -------
# # 1.
condition_cattle <- "category == 'Cattle'"
sapply(
  names(dt_summary_appended), get_count, conditions_in.str = condition_cattle
)
# ## Note:
# ## This result shows columns that include mutiple values.

dt_summary_appended[
  eval(parse(text = condition_cattle)),
  .N,
  by = .(slug_name, market_type_category)
][
  ,
  .N,
  by = .(market_type_category)
]


# # 2.
# # 2.1. Create a DT that illustrates values of key data fields
# # 2.1.1. Create objects that will be used later
cattle_auction_category <- "Cattle"
condition_cattle_auction <-
  "category == 'Cattle' & market_type_category == 'Auction'"
cattle_auction_N <-
  dt_summary_appended[
    eval(parse(text = condition_cattle_auction)), .N, by = .(slug_name)
  ][
    , .N
  ]
cattle_auction_commodity <- dt_summary_appended[
  eval(parse(text = condition_cattle_auction)), .N, keyby = .(commodity)
]$commodity
cattle_auction_commodity <-
  cattle_auction_commodity[!is.na(cattle_auction_commodity)]
cattle_auction_class <- dt_summary_appended[
  eval(parse(text = condition_cattle_auction)), .N, keyby = .(class)
]$class
cattle_auction_class <-
  cattle_auction_class[!is.na(cattle_auction_class)]
cattle_auction_market.type <- dt_summary_appended[
  eval(parse(text = condition_cattle_auction)), .N, keyby = .(market_type)
]$market_type
cattle_auction_market.type <-
  cattle_auction_market.type[!is.na(cattle_auction_market.type)]
cattle_auction_market.type.category <- "Auction"
cattle_auction_office.name <- dt_summary_appended[
  eval(parse(text = condition_cattle_auction)), .N, keyby = .(office_name)
]$office_name
cattle_auction_office.name <-
  cattle_auction_office.name[!is.na(cattle_auction_office.name)]
# # 2.1.2. Create a DT
cattle_auction_cols <- c(
  "cattle_auction_category",
  "cattle_auction_market.type.category",
  "cattle_auction_N",
  "cattle_auction_commodity",
  "cattle_auction_class",
  "cattle_auction_market.type",
  "cattle_auction_office.name"
)
cattle_auction_target.length <-
  lapply(mget(cattle_auction_cols), length) %>% unlist(.) %>% max(.)
dt_cattle_auction <-
  lapply(
    mget(cattle_auction_cols),
    function (vector, target.length) {
      vector_modified <- c(vector, rep("", target.length - length(vector)))
      return (vector_modified)
    },
    target.length = cattle_auction_target.length
  ) %>%
    as.data.table(.)
# # 2.1.3. Create a huxtable
ht_cattle_auction <- huxtable(dt_cattle_auction)
contents(ht_cattle_auction)[1, 1:7] <- c(
  "Category", "Market Type Category", "# of Reports", "Commodity", "Class",
  "Market Type", "Office"
)
bold(ht_cattle_auction)[1,] <- TRUE
align(ht_cattle_auction)[1,] <- "center"
col_width(ht_cattle_auction)[c(1,3)] <- 10
col_width(ht_cattle_auction)[c(2, 4, 5)] <- 30
col_width(ht_cattle_auction)[6] <- 45
col_width(ht_cattle_auction)[7] <- 15
top_border(ht_cattle_auction)[1,] <- brdr(thickness = 1)
bottom_border(ht_cattle_auction)[c(1,35),] <- brdr(thickness = 1)
right_border(ht_cattle_auction)[, c(1:6)] <- brdr(thickness = 1)
ht_cattle_auction

# # 3.
# # 3.2. Create a DT that illustrates values of key data fields
# # 3.1.1. Create objects that will be used later
cattle_summary_category <- "Cattle"
condition_cattle_summary <-
  "category == 'Cattle' & market_type_category == 'Summary'"
cattle_summary_N <-
  dt_summary_appended[
    eval(parse(text = condition_cattle_summary)), .N, by = .(slug_name)
  ][
    , .N
  ]
cattle_summary_commodity <- dt_summary_appended[
  eval(parse(text = condition_cattle_summary)), .N, keyby = .(commodity)
]$commodity
cattle_summary_commodity <-
  cattle_summary_commodity[!is.na(cattle_summary_commodity)]
cattle_summary_class <- dt_summary_appended[
  eval(parse(text = condition_cattle_summary)), .N, keyby = .(class)
]$class
cattle_summary_class <-
  cattle_summary_class[!is.na(cattle_summary_class)]
cattle_summary_market.type <- dt_summary_appended[
  eval(parse(text = condition_cattle_summary)), .N, keyby = .(market_type)
]$market_type
cattle_summary_market.type <-
  cattle_summary_market.type[!is.na(cattle_summary_market.type)]
cattle_summary_market.type.category <- "Summary"
cattle_summary_office.name <- dt_summary_appended[
  eval(parse(text = condition_cattle_summary)), .N, keyby = .(office_name)
]$office_name
cattle_summary_office.name <-
  cattle_summary_office.name[!is.na(cattle_summary_office.name)]
# # 3.1.2. Create a DT
cattle_summary_cols <- c(
  "cattle_summary_category",
  "cattle_summary_market.type.category",
  "cattle_summary_N",
  "cattle_summary_commodity",
  "cattle_summary_class",
  "cattle_summary_market.type",
  "cattle_summary_office.name"
)
cattle_summary_target.length <-
  lapply(mget(cattle_summary_cols), length) %>% unlist(.) %>% max(.)
dt_cattle_summary <-
  lapply(
    mget(cattle_summary_cols),
    function (vector, target.length) {
      vector_modified <- c(vector, rep("", target.length - length(vector)))
      return (vector_modified)
    },
    target.length = cattle_summary_target.length
  ) %>%
    as.data.table(.)
# # 3.1.3. Create a huxtable
ht_cattle_summary <- huxtable(dt_cattle_summary)
contents(ht_cattle_summary)[1, 1:7] <- c(
  "Category", "Market Type Category", "# of Reports", "Commodity", "Class",
  "Market Type", "Office"
)
bold(ht_cattle_summary)[1,] <- TRUE
align(ht_cattle_summary)[1,] <- "center"
col_width(ht_cattle_summary)[c(1,3)] <- 10
col_width(ht_cattle_summary)[c(2, 4, 5)] <- 30
col_width(ht_cattle_summary)[6] <- 55
col_width(ht_cattle_summary)[7] <- 15
top_border(ht_cattle_summary)[1,] <- brdr(thickness = 1)
bottom_border(ht_cattle_summary)[c(1,30),] <- brdr(thickness = 1)
right_border(ht_cattle_summary)[, c(1:6)] <- brdr(thickness = 1)
ht_cattle_summary


# ------------------------------------------------------------------------------
# Issues
# ------------------------------------------------------------------------------
# ------- Issue 1: Irregular publication frequency -------
dt_summary_appended[, .N, keyby = .(frequency_days)]
# ## Note:
# ## For example, `slug_name == "AMS_1980"` is published on 2021-12-04,
# ## 2021-11-13, 2021-10-23, 2021-09-26, 2021-08-06, 2021-06-05, ...


# ------- Issue 2: Data being in report does not exist in API reponses -------
#
dts_report_renamed[["AMS_2707"]]
dts_report_renamed[["AMS_2710"]]
# ## Note:
# ## 1) The API-providing does not include any price information. However, the
# ##    report includes the information about price ranges.
# ## 2) Those reports lack both `category` and `commodity` information.


# ------- Issue 3: Reports with no info. about `category` -------
dt_summary_appended[is.na(category), .N, keyby = .(commodity)]
# ## Note:
# ## 1) Reports having commodity information are about truck rates.
# ## 2) For reports without both `category` and `commodity`, their titles
# ##    must be used to extract some useful information.
