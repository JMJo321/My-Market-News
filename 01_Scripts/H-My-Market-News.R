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

# # 1.2. Paths for data
PATH_DATA <- "02_Data"

# # 1.3. Paths for R Shiny apps
PATH_APPS <- "03_Apps"


# # 2. Create Directories
list_path <- c(
  PATH_SCRIPTS,
  PATH_DATA,
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
api_endpoint_base <- "https://marsapi.ams.usda.gov/services/v1.2/reports"


# ------- Define function(s) -------
# # 1. Create an API request
create_req <- function (api.endpoint) {
  req <-
    httr2::request(api.endpoint) %>%
      httr2::req_auth_basic(., username = api_key, password = api_password)
  return (req)
}

get_resp <- function (api.request) {
  resp <- httr2::req_perform(api.request)
}


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