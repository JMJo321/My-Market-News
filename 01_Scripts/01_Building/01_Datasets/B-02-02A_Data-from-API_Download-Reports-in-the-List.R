# < Description > *
# > Script Group Indicator Number and Name
# # : B-02, Data from API
# #
# > Script Number(s)
# # : B-02-02A
# #
# > Purpose of the script(s)
# # : Get reports, and then save the response.

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(tibble)
library(stringr)


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
# # 1. Path(s) to load necessary data
FILE_TO.LOAD <- "MMN_Lists-from-MMN-API.RData"
DIR_TO.LOAD <- paste(PATH_DATA_INTERMEDIATE, "MMN-API/Lists", sep = "/")


# # 2. Path(s) to save data
DIR_TO.SAVE <- paste(PATH_DATA_RAW, "MMN-API", sep = "/")


# ------- Define parameter(s) -------
YEAR_BEGIN_REPORT.SEARCH <- 1990
# ## Note:
# ## The value of this variable has to be small enough.
YEAR_END_REPORT.SEARCH <- 2021


# ------- Define function(s) -------
# # 1. Get API responses, and then save the responses
# # 1.1. Helper functions
# # 1.1.1. Get an API response including reports for a specific year
help_get.report_only.for.a.specific.year <- function (slug.name_str, year_) {
  resp <- get_api.resp_report(slug.name_str, year_)
  result.length <-
    httr2::resp_body_json(resp) %>%
      .$stats %>%
      .$returnedRows
  if (is.null(result.length) == TRUE) {
    # When there is no data for a `slug.name`, there would be no element named
    # as `returnedRows`.
    result.length_modified <- 0
  } else {
    result.length_modified <- result.length
  }
  if (result.length_modified == 0) {
    resp_to.return <- NA
  } else {
    resp_to.return <- resp
  }
  return (resp_to.return)
}
# # 1.1.2. Get API responses including reports for a range of years
help_get.reports_in.resp <-
  function (slug.name_str, year_begin_int = NULL, year_end_int = NULL) {
    if (is.null(year_begin_int) & is.null(year_end_int)) {
      reports_resp <-
        get_api.resp_report(slug.name_str) %>%
          list(.)
      names(reports_resp) <- slug.name_str
    } else {
      years <- c(year_begin_int:year_end_int)
      reports_resp <- lapply(
        years,
        help_get.report_only.for.a.specific.year, slug.name_str = slug.name_str
      )
      names(reports_resp) <- as.character(years)
    }
    return (reports_resp)
}
# # 1.1.3. Covert API responses to TBLs
help_get.reports_in.tbl <- function (reports_in.resp) {
  reports_tibble <- lapply(reports_in.resp, transform_report_resp.to.tbl)
  return (reports_tibble)
}

# # 1.2. Download reports, and save it.
save_reports <-
  function (
    slug.name_str, year_begin_int = NULL, year_end_int = NULL, dir_str
  ) {
    # ## Create objects that will be used later
    slug.name_modified <- toupper(slug.name_str) %>% str_replace(., "\\_", "-")
    path_resp <-
      paste0(slug.name_modified, "_API-Responses", ".RData") %>%
        append_date.to.filename(.) %>%
        paste(dir_str, "Responses", ., sep = "/")
    path_tbl <-
      paste0(slug.name_modified, "_Tibble", ".RData") %>%
        append_date.to.filename(.) %>%
        paste(dir_str, "Tibbles", ., sep = "/")
    obj.name_resp <-
      tolower(slug.name_str) %>%
        paste("resp", ., sep = "_")
    obj.name_tbl <-
      tolower(slug.name_str) %>%
        paste("tbl", ., sep = "_")
    # ## Get API responses, and transform their data type
    if (is.null(year_begin_int) & is.null(year_end_int)) {
      assign(obj.name_resp, help_get.reports_in.resp(slug.name_str))
      assign(obj.name_tbl, get(obj.name_resp) %>% help_get.reports_in.tbl(.))
    } else {
      assign(
        obj.name_resp,
        help_get.reports_in.resp(slug.name_str, year_begin_int, year_end_int)
      )
      assign(obj.name_tbl, get(obj.name_resp) %>% help_get.reports_in.tbl(.))
    }
    # ## Save data
    save(list = obj.name_resp, file = path_resp)
    save(list = obj.name_tbl, file = path_tbl)
}


# ------------------------------------------------------------------------------
# Download reports by using USDA API, and then save the reports
# ------------------------------------------------------------------------------
# ------- Download and save reports obtained from USDA API -------
# # 1. Load the list of reports
load_most.recent.data(DIR_TO.LOAD, FILE_TO.LOAD)


# # 2. Download reports, and then save them.
slug.names <- dt_report$slug_name
timestamp()
for (name in slug.names) {
  save_reports(
    slug.name_str = name,
    year_begin_int = YEAR_BEGIN_REPORT.SEARCH,
    year_end_int = YEAR_END_REPORT.SEARCH,
    dir_str = DIR_TO.SAVE
  )
  # ## Note:
  # ## It seems that there is no benefit from getting reports year by year.
  # save_reports(
  #   slug.name_str = name,
  #   year_begin_int = NULL,
  #   year_end_int = NULL,
  #   dir_str = DIR_TO.SAVE
  # )
  cat(
    paste0(
      "Completed: ", which(slug.names == name),
      " out of ", length(slug.names), ".\n"
    )
  )
  timestamp()
  Sys.sleep(0.5)
}
