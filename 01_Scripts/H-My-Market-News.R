# < Description > *
# > Script Group Indicator Number and Name:
# # H, MMN
# #
# > Script Number(s):
# # (Not Applicable)
# #
# > Purpose of the script(s):
# # The Header Script for My Market News Data Project.


# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Clear the worksapce
# ------------------------------------------------------------------------------
rm(list = setdiff(ls(), c("PATH_PROJ")))


# ------------------------------------------------------------------------------
# Set working directory
# ------------------------------------------------------------------------------
# ------- To set working directory -------
# (In each script)


# ------------------------------------------------------------------------------
# Define path(s), parameter(s) and function(s)
# ------------------------------------------------------------------------------
# ------- Define path(s) -------
# # 1. Add basic paths
# # 1.1. Paths for scripts
PATH_SCRIPTS <- "01_Scripts"
PATH_SCRIPTS_BUILDING <- paste(PATH_SCRIPTS, "01_Building", sep = "/")
PATH_SCRIPTS_BUILDING_DATASETS <-
  paste(PATH_SCRIPTS_BUILDING, "01_Datasets", sep = "/")
PATH_SCRIPTS_BUILDING_APPS <-
  paste(PATH_SCRIPTS_BUILDING, "02_Apps", sep = "/")
PATH_SCRIPTS_ANALYSIS <- paste(PATH_SCRIPTS, "02_Analysis", sep = "/")

# # 1.2. Paths for data
PATH_DATA <- "02_Data"
PATH_DATA_RAW <- paste(PATH_DATA, "01_RAW", sep = "/")
PATH_DATA_INTERMEDIATE <- paste(PATH_DATA, "02_INTERMEDIATE", sep = "/")
PATH_DATA_ANALYSIS <- paste(PATH_DATA, "03_ANALYSIS", sep = "/")

# # 1.3. Paths for R Shiny apps
PATH_APPS <- "03_Apps"


# # 2. Create Directories
list_path <- c(
  PATH_SCRIPTS,
  PATH_SCRIPTS_BUILDING,
  PATH_SCRIPTS_BUILDING_DATASETS,
  PATH_SCRIPTS_BUILDING_APPS,
  PATH_SCRIPTS_ANALYSIS,
  PATH_DATA,
  PATH_DATA_RAW,
  PATH_DATA_INTERMEDIATE,
  PATH_DATA_ANALYSIS,
  PATH_APPS
)
for (path in list_path) {
  if (!dir.exists(path)) {
    dir.create(path)
  }
}


# ------- Define parameter(s) -------
# # 1. For USDA MMN API
# # 1.1. Authorization Information
api_key <- "pEhHNZ2t9plBQbuD/1B4Z4n9PvB3g8uc"
api_password <- ""

# # 1.2. API endpoint
api_endpoint_base <- "https://marsapi.ams.usda.gov/services/v1.2"
api_endpoint_report <-
  "https://marsapi.ams.usda.gov/services/v1.2/reports"
api_endpoint_office <-
  "https://marsapi.ams.usda.gov/services/v1.2/offices"
api_endpoint_market.type <-
  "https://marsapi.ams.usda.gov/services/v1.2/marketTypes"
api_endpoint_commodity <-
  "https://marsapi.ams.usda.gov/services/v1.2/commodities"

# # 1.3. Status codes
list_api.status.code <- list(
  "200" = "Your request was successful.",
  "202" = "Your request is processing.",
  "400" = "Your request did not follow the correct syntax.",
  "401" = "You are not authorized to make this request.",
  "404" = "Your request was not found and/or does not exist.",
  "429" = "You have made too many requests.",
  "500" = paste0(
    "The server has encountered an unexpected condition,",
    " and the request cannot be completed"
  )
)


# # 2. (...)
reg.exp_c <- "|_|"
reg.exp_split <- "\\|\\_\\|"


# ------- Define function(s) -------
# # 1. Help utilize USDA API
# # 1.1. Common use
# # 1.1.1. Create an API request
create_api.req <- function (api.endpoint_str) {
  req <-
    httr2::request(api.endpoint_str) %>%
      httr2::req_auth_basic(., username = api_key, password = api_password)
  return (req)
}
# # 1.1.2. Get the response of an API request
get_api.resp <- function (api.request) {
  resp <- httr2::req_perform(api.request)
}

# # 1.2. Only for reports
# # 1.2.1. Get the response of an API request that includes query
# # 1.2.1.1. Create a query
create_query_published.date <- function (published.year_int) {
  date_begin <- paste("01", "01", published.year_int, sep = "/")
  date_end <- paste("12", "31", published.year_int, sep = "/")
  range <- paste(date_begin, date_end, sep = ":")
  query <- paste("?q", "published_date", range, sep = "=")
  return (query)
}
# # 1.2.1.2. Create an API endpoint including a query
create_api.endpoint <- function (slug.name_str, published.year_int) {
  api.endpoint <- paste(api_endpoint_report, slug.name_str, sep = "/")
  api.endpoint_w.query <-
    paste0(
      api.endpoint,
      create_query_published.date(published.year_int = published.year_int)
    )
  return (api.endpoint_w.query)
}
# # 1.2.1.3. From creating an API endpoint to getting the response of it
get_api.resp_report <- function (slug.name_str, published.year_int) {
  # ## Create an API endpoint
  api.endpoint <- create_api.endpoint(
    slug.name_str = slug.name_str,
    published.year_int = published.year_int
  )
  # ## Create an API request by using the endpoint
  req <- create_api.req(api.endpoint_str = api.endpoint)
  # ## Get the response of the API request
  resp <- get_api.resp(req)
  # ## Return the response
  return (resp)
}
# ## Note:
# ## The function is intended to retrieve data only for a specific year. Refer
# ## to following information:
# ## "Do I have to register to use the MARS API?
# ## It is preferred that you register to use the MARS API. With registration,
# ## MARS API provides each logged-in user their unique API key which allows
# ## you to take full advantage of My Market News' capabilities and provides a
# ## layer of security for both your system and ours.  Additionally, by
# ## registering, USDA Market News is able to monitor what data is being
# ## pulled, therefore allowing us to provide better services. Currently there
# ## is a sample key you can use without registering but it will be removed
# ## sometime in the future.
# ## Unregistered users are limited to pulling 5000 rows of data per request.
# ## Registered users can pull 100,000 at a time."

# # 1.2.2. Transform a response into TBL
transform_report_resp.to.tbl <- function (resp) {
  tbl <-
    httr2::resp_body_json(resp) %>%
      .$results %>%  # These two steps are necessary to extract `results` only
      jsonlite::toJSON(., auto_unbox = TRUE) %>%  # Generate a JSON string
      jsonlite::fromJSON(.) %>%
      as_tibble(.)
  return (tbl)
}
# # 1.2.3. Transform a TBL[DT] created from a API response into DT[TBL]
# # 1.2.3.1. From TBL to DT
# ## Helper functions
# ## For a column, from list to str
help_list.to.str <- function (list_) {
  length_ <- length(list_)
  dtype <- class(list_)
  if (length_ == 0) {
    str_ <- NA
  } else {
    str_ <-
      as.character(list_) %>%
        str_c(., collapse = reg.exp_c) %>%
        paste(paste0("[", dtype, "]"), ., sep = reg.exp_c)
  }
  return (str_)
}
# ------- #
# ## For a TBL, from list to str
help_convert_list.to.str.in.tbl <- function (col.name_str, tbl) {
  select <- tbl[[col.name_str]]  # A list
  str_from.list <-
    lapply(select, help_list.to.str) %>%
      unlist(.)
  tbl[[col.name_str]] <- str_from.list
  return (tbl)
}
# ------- #
# ## Deal with column(s) including DF
# TODO: Complete this part.
# row.num_tbl <- nrow(tbl)
# col <- cols_to.transform_df[1]
# select <- tbl[[col]]  # A DF with no variables
#
# help_df.to.str <- function (df, row.num_tbl) {
#   length_ <- length(df)
#   if (length_ == 0) {  # No column implies no data.
#     str_ <- rep(NA, times = row.num_tbl)
#   } else {
#     str_ <-
#       as.list(df) %>%
#         lapply(., str_c, collapse = reg.exp_c) %>%
#         unlist(.)
#   }
#   return (str_)
# }
#
# tbl[[col]] <- str_from.df
# ------- #
# ## Make a function by using the helper functions
transform_tbl.to.dt <- function (tbl) {
  # ## Collect info. about columns
  col.names <- names(tbl)
  data.types <- NULL
  for (name in col.names) {
    type <- class(tbl[[name]])
    data.types <- c(data.types, type)
  }
  # ## Identify columns whose data type must be converted to character
  cols_to.transform_list <-
    lapply(data.types, str_detect, pattern = "list") %>%
      as.logical(.) %>%
      col.names[.]
  cols_to.transform_df <-
    lapply(data.types, str_detect, pattern = "data\\.frame") %>%
      as.logical(.) %>%
      col.names[.]
  # ## Conversion: list to str
  for (col in cols_to.transform_list) {
    tbl <- help_convert_list.to.str.in.tbl(col, tbl)
  }
  # ## Conversion: DF to str
  # TODO: Code for `cols_to.transform_df`
  # ## Transform the TBL into a DT
  dt <- setDT(tbl)
  # ## Return the DT
  return (dt)
}
# # 1.2.3.1. From DT to TBL
# TODO: Complete this function.
# lapply(
#   select, function (x) {
#     as.character(x) %>%
#       str_c(., collapse = "|_|") %>%
#       str_split(., "\\|\\_\\|") %>%
#       unlist(.)
#   }
# )


# # 2. Functions for miscellaneous tasks
# # 2.1. Append date to a filename
append_date.to.filename <- function (filename.incl.ext_str) {
  date <- Sys.time() %>% format(., "%Y-%m-%d")
  filename.wo.ext <- str_replace(filename.incl.ext_str, "\\.[:alpha:]+$", "")
  ext <- str_extract(filename.incl.ext_str, "\\.[:alpha:]+$")
  filename_w.date <- paste0(paste(filename.wo.ext, date, sep = "_"), ext)
  return (filename_w.date)
}
