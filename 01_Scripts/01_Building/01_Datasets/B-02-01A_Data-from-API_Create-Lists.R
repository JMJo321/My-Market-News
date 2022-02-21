# < Description > *
# > Script Group Indicator Number and Name
# # : B-02, Data from API
# #
# > Script Number(s)
# # : B-02-01A
# #
# > Purpose of the script(s)
# # : Create DTs that include lists of reports, commodities, market types, and
# #   offices.

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(tidyverse)
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
# # 1. Path at which DFs will be stored
FILE_TO.SAVE <- "MMN_Lists-from-MMN-API.RData"
PATH_TO.SAVE <-
  paste(
    PATH_DATA_INTERMEDIATE, "MMN-API", "Lists",
    append_date.to.filename(FILE_TO.SAVE),
    sep = "/"
  )


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Make lists of reports, commodities, market typew, and USDA officew
# ------------------------------------------------------------------------------
# ------- Make a list of reports -------
# # 1. Create a DT by using the API
# # 1.1. Get an API reponse
resp_report <-
  create_api.req(api_endpoint_report) %>%
    get_api.resp(.)

# # 1.2. Transform the response into a TBL
tbl_report <-
  httr2::resp_body_string(resp_report) %>%
    jsonlite::fromJSON(.) %>%
    as_tibble(.)

# # 1.3. Transform the TBL into a DT
dt_report <- transform_tbl.to.dt(tbl_report)

# # 1.4. Identify the primary key(s)
dt_report[, .N, by = .(slug_id)][N > 1, .N] == 0
dt_report[, .N, by = .(slug_name)][N > 1, .N] == 0
# ## Note:
# ## `slug_id` and `slug_type` are the primary key of the DT, respectively.


# # 2. Modify the DT
# # 2.1. Modify existing column(s)
dt_report[
  ,
  published_date := as.POSIXct(
    published_date,
    tz = "MST",
    format = "%m/%d/%Y %H:%M:%OS"
  )
]
# ## Note:
# ## The time zone is coming from `Report Index ALL".

# # 2.2. Reorder columns
col.order <- c(
  "slug_id", "slug_name", "published_date",
  "report_title", "markets", "market_types", "offices"
)
setcolorder(dt_report, col.order)


# ------- Make a list of commodities -------
# # 1. Create a DT by using the API
# # 1.1. Get an API reponse
resp_commodity <-
  create_api.req(api_endpoint_commodity) %>%
    get_api.resp(.)

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


# ------- Make a list of market types -------
# # 1. Create a DT by using the API
# # 1.1. Get an API reponse
resp_market.type <-
  create_api.req(api_endpoint_market.type) %>%
    get_api.resp(.)

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


# ------- Make a list of offices -------
# # 1. Create a DT by using the API
# # 1.1. Get an API reponse
resp_office <-
  create_api.req(api_endpoint_office) %>%
    get_api.resp(.)

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
# Save the DTs created above
# ------------------------------------------------------------------------------
# ------- Save the DTs in .RData format -------
save(
  dt_report, dt_commodity, dt_market.type, dt_office,
  file = PATH_TO.SAVE
)
