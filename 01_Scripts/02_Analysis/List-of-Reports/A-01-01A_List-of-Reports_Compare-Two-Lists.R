# < Description > *
# > Script Group Indicator Number and Name
# # : A-01, List of Reports
# #
# > Script Number(s)
# # : A-01-01A
# #
# > Purpose of the script(s)
# # : Compare two lists of reports, which are created by "B-01-01A":
# #   1) A list obtained by using USDA API; and
# #   2) A list obtained by "Report Index All" on
# #      "https://mymarketnews.ams.usda.gov/filerepo/reports"

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
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
# # 1. Path of dataset(s) for analysis
FILE_TO.LOAD <- "MMN_List-of-Reports.RData"
PATH_TO.LOAD <-
  paste(PATH_DATA_ANALYSIS, "List-of-Reports", FILE_TO.LOAD, sep = "/")


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Load DFs
# ------------------------------------------------------------------------------
# ------- Load DFs that include list of reports -------
load(PATH_TO.LOAD)


# ------- Identify the primary key(s) -------
# # 1. For `tbl_reports_api`
select_cols <- c(
  "slug_id", "slug_name",
  "published_year.month", "published_year", "published_month"
)
dt_reports_api <- tbl_reports_api[, select_cols] %>% setDT(.)
dt_reports_api[, .N, by = .(slug_id)][N > 1, .N] == 0
dt_reports_api[, .N, by = .(slug_name)][N > 1, .N] == 0
# ## Note:
# ## The two tests above show that both `slug_id` and `slug_name` are,
# ##  respectively, the primary key.

# # 2. For `dt_reports_idx`
dt_reports_idx[, .N, by = .(slug_id)][N > 1, .N] == 0
dt_reports_idx[, .N, by = .(slug_id)][N > 1]
dt_reports_idx[slug_id == 2890]
# ## Note:
# ## This test demonstrates that `slug_id` is not the primary key.

dt_reports_idx[, .N, by = .(slug_name)][N > 1, .N] == 0
# ## Note:
# ## This test illustrates that `slug_name` is the primary key.


# ------------------------------------------------------------------------------
# Compare two lists
# ------------------------------------------------------------------------------
# ------- Whether one list covers the other list or not -------
dt_reports_api[slug_name %in% (dt_reports_idx$slug_name), .N]
slug.name_both <-
  dt_reports_api[slug_name %in% (dt_reports_idx$slug_name)]$slug_name
# ## The number of reports that are in both lists:685
dt_reports_api[!slug_name %in% (dt_reports_idx$slug_name), .N]
slug.name_api.only <-
  dt_reports_api[!slug_name %in% (dt_reports_idx$slug_name)]$slug_name
# ## The number of reports that are only in the list created from USDA API: 5
dt_reports_idx[!slug_name %in% (dt_reports_api$slug_name), .N]
slug.name_idx.only <-
  dt_reports_idx[!slug_name %in% (dt_reports_api$slug_name)]$slug_name
# ## The number of reports that are only in the list created from
# ## "Report Index All": 923

# ## Note:
# ## Those numbers show that one list does not cover the other list entirely.


# ------- Check the publication years -------
dt_reports_idx[, .N, keyby = .(published_year)]
dt_reports_api[, .N, keyby = .(published_year)]
# ## Note:
# ## Both lists includes reports that are published from 2018. However,
# ## many reports are also published in 2017. For example, refer to the
# ## followin URL:
# ## "https://mymarketnews.ams.usda.gov/viewReport/3127"


# ------- Check the availability of USDA API by group -------
create_req(
  paste(
    api_endpoint_base, slug.name_both[5], "?sort=-published_date",
    sep = "/"
  )
) %>%
  get_resp(.) %>%
  httr2::resp_body_json(.) %>%
  jsonlite::toJSON(., pretty = TRUE)

create_req(
  paste(
    api_endpoint_base, slug.name_api.only[5], "?sort=-published_date",
    sep = "/"
  )
) %>%
  get_resp(.) %>%
  httr2::resp_body_json(.) %>%
  jsonlite::toJSON(., pretty = TRUE)

create_req(
  paste(
    api_endpoint_base, slug.name_idx.only[5], "?sort=-published_date",
    sep = "/"
  )
) %>%
  get_resp(.) %>%
  httr2::resp_body_json(.) %>%
  jsonlite::toJSON(., pretty = TRUE)

# ## Note:
# ## 1) USDA API provides reports published before 2018.
# ##    (For example, the first publication year-month of the first try is
# ##    January 1994.)
# ##    which implies that
# ## 2) USDA API seems to provide reports whose `slug_name` is in the list from
# ##    USDA API.
# ## 3) Each report has its own format. Therefore, downloaing all reports by
# ##    using the API is necessary to check detailed information in each report.
