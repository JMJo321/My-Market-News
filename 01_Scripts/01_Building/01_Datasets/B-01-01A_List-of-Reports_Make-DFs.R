# < Description > *
# > Script Group Indicator Number and Name
# # : B-01, List of Reports
# #
# > Script Number(s)
# # : B-01-01A
# #
# > Purpose of the script(s)
# # : Create DFs that include information about reports.

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(httr2)
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
FILE_TO.SAVE <- "MMN_List-of-Reports.RData"
PATH_TO.SAVE <- paste(
  PATH_DATA_ANALYSIS, "List-of-Reports",
  append_date.to.filename(FILE_TO.SAVE),
  sep = "/"
)


# ------- Define parameter(s) -------
# # 1. URL for "Report Index All"
api_endpoint_all.reports <-
  "https://mymarketnews.ams.usda.gov/services/v1/public/listPublishedReports/all"
# ## Note:
# ## Note that the version number is different from that of base endpoint.


# ------- Define function(s) -------
# # 1. Functions for data obtained from the USDA API
# # 1.1. Extract data from a list
get_data.from.list <- function (list_, data.field) {
  data <- list_[[data.field]]
  return (data)
}

# # 1.2. Extract data in a list type from a list
get_list.from.list <- function (list_, data.field) {
  # ## Extract selected data
  tmp_list <- lapply(list_, get_data.from.list, data.field = data.field)
  # ## Simplify the list created above
  list_to.return <- NULL
  for (i in c(1:length(tmp_list))) {
    tmp_length <- length(tmp_list[[i]])
    if (tmp_length == 1) {
      tmp_data <- tmp_list[[i]]
      names(tmp_data) <- reports_id[i]
    } else {
      tmp_data <- tmp_list[i]
      names(tmp_data) <- reports_id[i]
    }
    list_to.return <- c(list_to.return, tmp_data)
  }
  # ## Return the list simplified
  return (list_to.return)
}


# # 2. Function(s) for data obtained from the index of reports
# # 2.1. Create a DT only with a row from a string
get_dt.from.str <- function (str) {
  # ## Modify the string
  tmp_row <- str_trim(str, side = "both")
  tmp_row_title <-
    str_extract(tmp_row, "\\-[0-9]{2}\\s{2,}.+$") %>%
      str_replace(., "^.+\\-[0-9]{2}", "") %>%
      str_trim(., side = "left")
  tmp_row_title_modified <- str_replace_all(tmp_row_title, "\\s{2,}", " ")
  tmp_row_modified <- paste0(
    str_sub(
      tmp_row, start = 1, end = str_count(tmp_row) - str_count(tmp_row_title)
    ),
    tmp_row_title_modified
  )
  tmp_row_split <-
    str_split(tmp_row_modified, "\\s{2,}") %>%
      unlist(.)
  # ## Create a DT from the string
  dt <-
    rbind(tmp_row_split) %>%
      data.table(.)
  names(dt) <- c(
    "slug_id", "slug_name", "ext", "published_date", "published_date_msec",
    "report_begin_date", "report_end_date", "report_title"
  )
  # ## Return the DT
  return (dt)
}


# ------------------------------------------------------------------------------
# Create DFs that include information of MMN Reports
# ------------------------------------------------------------------------------
# ------- Create a list of reports by using the USDA API -------
# # 1. Download data
rest_reports <- create_api.req(api_endpoint_report)
resp_reports <- get_api.resp(rest_reports)
list_reports <- resp_body_json(resp_reports)


# # 2. Create a TBL from the data downloaded
# # 2.1. Check the complexity of the data
tidyjson::json_complexity(list_reports) %>%
  as.data.table(.) %>%
  .[, .N, keyby = .(complexity)]
# ## Note:
# ## Several reports have sub-list(s). To be specific, `markets`, `market_type`,
# ## `office`, and `sectionNames` are a list that include a list.

# # 2.2. Create a TBL
# # 2.2.1. Extract data from the data in JSON
# # 2.2.1.1. Data not in list type
reports_id <-
  lapply(list_reports, get_data.from.list, data.field = "slug_id") %>%
    as.character(.)
reports_name <-
  lapply(list_reports, get_data.from.list, data.field = "slug_name") %>%
    as.character(.)
reports_title <-
  lapply(list_reports, get_data.from.list, data.field = "report_title") %>%
    as.character(.)
reports_date <-
  lapply(list_reports, get_data.from.list, data.field = "published_date") %>%
    as.character(.) %>%
    str_extract(., ".+?\\s") %>%
    str_trim(., side = "right") %>%
    as.Date(., format = "%m/%d/%Y")
# # 2.2.1.2. Data in list type
reports_markets <-
  get_list.from.list(list_reports, data.field = "markets")
reports_market.types <-
  get_list.from.list(list_reports, data.field = "market_types")
reports_offices <-
  get_list.from.list(list_reports, data.field = "offices")

# # 2.2.2. Create a TBL
tbl_reports_api <- tibble::tibble(
  slug_id = reports_id,
  slug_name = reports_name,
  report_title = reports_title,
  published_date = reports_date,
  markets = reports_markets,
  market_types = reports_market.types,
  offices = reports_offices
)
# ## Note:
# ## The two data fields `hasCorrectionsInLastThreeDays` and `sectionNames`
# ## do not include useful info. for making a summary of reports.


# ------- Create a list of reports by using the index of reports -------
# # 1. Download data in a string
resp_reports_idx <-
  request(api_endpoint_all.reports) %>%
    req_perform(.) %>%
    resp_body_string(.) %>%
    as.character(.)


# # 2. Create a DT from the data downloaded
# # 2.1. Modify the data
resp_reports_idx_modified <-
  str_replace(resp_reports_idx, '^\\"', "") %>%
    str_replace(., '\\"$', "") %>%
    str_replace(., "^.+?\\\\n\\\\n", "") %>% # ## To drop the first line
    str_split(., "\\\\n") %>%
    unlist(.)

# # 2.2. Create a DT
dt_reports_idx <-
  lapply(
    resp_reports_idx_modified[2:length(resp_reports_idx_modified)],
    # ## To drop column names
    get_dt.from.str
  ) %>%
    rbindlist(.)


# ------------------------------------------------------------------------------
# Modify the two DFs created above
# ------------------------------------------------------------------------------
# ------- Modify the DF created by using USDA API -------
# # 1. Modify existing column(s)
# # (Not Applicable)


# # 2. Add column(s)
# # 2.1. Add a column showing the published year-month, year, and month
tbl_reports_api$published_year.month <-
  zoo::as.yearmon(tbl_reports_api$published_date)
tbl_reports_api$published_year <- year(tbl_reports_api$published_date)
tbl_reports_api$published_month <- month(tbl_reports_api$published_date)


# # 3. Reorder columns
col.order <- c(
  "slug_id", "slug_name",
  "published_date", "published_year.month", "published_year", "published_month",
  "report_title", "markets", "market_types", "offices"
)
tbl_reports_api <- tbl_reports_api[, col.order]



# ------- Modify the DF created from "Report Index All" -------
# # 1. Modify existing column(s)
# # 1.1. For `slug_name`, change type from small to capital letters
dt_reports_idx[, slug_name := toupper(slug_name)]

# # 1.2. For `published_date`, change type from character to datetime
dt_reports_idx[, published_date := as.POSIXct(published_date, tz = "MST")]
stopifnot(attr(dt_reports_idx$published_date[1], "tzone") == "MST")
# To check TZ.

# # 1.3. For `published_date_msec`, drop it
dt_reports_idx[, published_date_msec := NULL]


# # 2. Add column(s)
# # 2.1. Add columns showing the published year-month, year, and month
dt_reports_idx[, published_year.month := zoo::as.yearmon(published_date)]
dt_reports_idx[, published_year := year(published_date)]
dt_reports_idx[, published_month := month(published_date)]


# # 3. Reorder columns
col.order <- c(
  "slug_id", "slug_name", "ext",
  "published_date", "published_year.month", "published_year", "published_month",
  "report_title", "report_begin_date", "report_end_date"
)
setcolorder(dt_reports_idx, col.order)


# ------------------------------------------------------------------------------
# Save the DFs created above
# ------------------------------------------------------------------------------
# ------- Save the DFs in .RData format -------
save(tbl_reports_api, dt_reports_idx, file = PATH_TO.SAVE)
