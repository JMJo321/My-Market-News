# < Description > *
# > Script Group Indicator Number and Name
# # : X-00, XYZ
# #
# > Script Number(s)
# # : X-00-000
# #
# > Purpose of the script(s)
# # : (...)

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
# # 1. Path of dataset(s) for analysis
FILE_TO.LOAD <- "MMN_List-of-Reports.RData"
PATH_TO.LOAD <-
  paste(PATH_DATA_ANALYSIS, "List-of-Reports", FILE_TO.LOAD, sep = "/")


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# # 1. To compute the number of apearance of a commodity on a list of report
# #     titles
get_count <- function (commodity.name, report.titles) {
  count <- str_detect(report.titles, commodity.name) %>%
    report.titles[.] %>%
    length(.)
  return (count)
}


# ------------------------------------------------------------------------------
# Make lists of commodity, market type, and USDA office
# ------------------------------------------------------------------------------
# ------- Make a list of commodity -------
# # 1. Create a DT by using the API
# # 1.1. Get an API reponse
resp_commodity <-
  create_req(api_endpoint_commodity) %>%
    get_resp(.)

# # 1.2. Transform the response into a DT
dt_commodity <-
  httr2::resp_body_string(resp_commodity) %>%
    jsonlite::fromJSON(.) %>%
    setDT(.)

# # 1.3. Identify the primary key(s)
dt_commodity[, .N, by = .(commodity_lov_id)][N > 1, .N] == 0
dt_commodity[, .N, by = .(commodity_name)][N > 1, .N] == 0
# ## Note:
# ## `commodity_lov_id` and `commodity_name` are the primary key of the DT,
# ## respectively.


# # 2. Modify the DT
# # 2.1. Modify column(s)
# # 2.1.1. Make clean strings in `commodity_name`
dt_commodity[, commodity_name := str_trim(commodity_name, side = "both")]
# # 2.1.2. Transfer the data type of `commodity_lov_id` from character to
# #        numeric
dt_commodity[str_detect(commodity_lov_id, "^0"), .N] == 0
dt_commodity[, commodity_lov_id := as.numeric(commodity_lov_id)]


# ------- Make a list of market type -------
# # 1. Create a DT by using the API
# # 1.1. Get an API reponse
resp_market.type <-
  create_req(api_endpoint_market.type) %>%
    get_resp(.)

# # 1.2. Transform the response into a DT
dt_market.type <-
  httr2::resp_body_string(resp_market.type) %>%
    jsonlite::fromJSON(.) %>%
    setDT(.)

# # 1.3. Identify the primary key(s)
dt_market.type[, .N, by = .(market_type_id)][N > 1, .N] == 0
dt_market.type[, .N, by = .(market_type)][N > 1, .N] == 0
# ## Note:
# ## `market_type_id` and `market_type` are the primary key of the DT,
# ## respectively.


# # 2. Modify the DT
# # 2.1. Modify column(s)
# # 2.1.1. Make clean strings in `market_type`
dt_market.type[, market_type := str_trim(market_type, side = "both")]
# # 2.1.2. Transfer the data type of `market_type_id` from character to
# #        numeric
dt_market.type[str_detect(market_type_id, "^0"), .N] == 0
dt_market.type[, market_type_id := as.numeric(market_type_id)]


# ------- Make a list of office -------
# # 1. Create a DT by using the API
# # 1.1. Get an API reponse
resp_office <-
  create_req(api_endpoint_office) %>%
    get_resp(.)

# # 1.2. Transform the response into a DT
dt_office <-
  httr2::resp_body_string(resp_office) %>%
    jsonlite::fromJSON(.) %>%
    setDT(.)

# # 1.3. Identify the primary key(s)
dt_office[, .N, by = .(office_code)][N > 1, .N] == 0
dt_office[, .N, by = .(office_name)][N > 1, .N] == 0
# ## Note:
# ## `office_code` and `office_name` are the primary key of the DT,
# ## respectively.


# # 2. Modify the DT
# # 2.1. Modify column(s)
# # 2.1.1. Make clean strings in `office_name`
dt_office[, office_name := str_trim(office_name, side = "both")]
# # 2.1.2. Add state abbreviation of "Madison"
dt_office[
  str_detect(office_name, "^Madison$"),
  office_name := paste0(office_name, ", WI")
]

# # 2.2. Add column(s)
# # 2.2.1. Column including the state abbreviation of office
dt_office[
  ,
  office_state :=
    str_extract(office_name, "((?<=,\\s)[A-Z]+$)|((?<=-\\s)[A-Z]+$)")
]


# ------------------------------------------------------------------------------
# Conduct simple tests
# ------------------------------------------------------------------------------
# ------- With respect to `dt_commodity` -------
# # 1. Create object(s) that are required to do several tests
# # 1.1. Load dataset(s) required
load(PATH_TO.LOAD)
# # 1.2. Make object(s)
dt_reports_api <- tbl_reports_api[, c("slug_name", "report_title")] %>% setDT(.)
title_api <- dt_reports_api$report_title
title_idx.only <-
  dt_reports_idx[!slug_name %in% (dt_reports_api$slug_name)]$report_title


# # 2. Conduct test(s)
# # 2.1. Count the number of report titles that includes a specific name of
# #      commodity
# # 2.1.1. Add columns including the numbers
dt_commodity[
  ,
  count_api :=
    lapply(commodity_name, get_count, report.titles = title_api) %>%
      as.vector(., "numeric")
]
dt_commodity[
  ,
  count_idx.only :=
    lapply(commodity_name, get_count, report.titles = title_idx.only) %>%
      as.vector(., "numeric")
]

# # 2.1.2. Print the most frequent five commodities
dt_commodity[count_api > 0, .N, keyby = .(count_api, commodity_name)] %>%
  tail(., 5) %>%
  .$commodity_name
dt_commodity[
  count_idx.only > 0, .N, keyby = .(count_idx.only, commodity_name)
] %>%
  tail(., 5) %>%
  .$commodity_name
# ## Note:
# ## The results means that commodity is not the determinent of the
# ## accessibility of reports via the API.
