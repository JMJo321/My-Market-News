# < Description > *
# > Script Group Indicator Number and Name
# # : B-02, Data from API
# #
# > Script Number(s)
# # : B-02-02A
# #
# > Purpose of the script(s)
# # : Load reports in DT format, and then create a DT including summary info.

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(openxlsx)
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
# # 1. Path(s) to load necessary script(s) and dataset(s)
# # 1.1. Path for the list of reports
DIR_TO.LOAD_LISTS <- paste(PATH_DATA_INTERMEDIATE, "MMN-API/Lists", sep = "/")
FILE_TO.LOAD_LISTS <- "MMN_Lists-from-MMN-API.RData"

# # 1.2. Path as which TBLs are stored
DIR_TO.LOAD_REPORTS <- paste(PATH_DATA_RAW, "MMN-API/Tibbles", sep = "/")

# # 1.3. Path for the script including a data dictionary in list type
DIR_TO.LOAD_DIC <- PATH_SCRIPTS
FILE_TO.LOAD_DIC <- "D-My-Market-News.R"
PATH_TO.LOAD_DIC <- paste(DIR_TO.LOAD_DIC, FILE_TO.LOAD_DIC, sep = "/")


# # 2. Path(s) to save dataset(s)
# # 2.1. Path for report data
DIR_TO.SAVE_REPORTS <-
  paste(PATH_DATA_INTERMEDIATE, "MMN-API/Reports", sep = "/")
FILE_TO.SAVE_REPORTS <- "MMN_Reports.RData"
PATH_TO.SAVE_REPORTS <- paste(
  DIR_TO.SAVE_REPORTS,
  append_date.to.filename(FILE_TO.SAVE_REPORTS),
  sep = "/"
)

# # 2.2. Path for summary data
SHEET.NAME_DETAIL <- "DETAIL"
SHEET.NAME_VAR <- "VARIABLES"
FILE_TO.SAVE_SUMMARY <- "MMN_Summary-of-Reports.xlsx"
PATH_TO.SAVE_SUMMARY <- paste(
  DIR_TO.SAVE_REPORTS,
  append_date.to.filename(FILE_TO.SAVE_SUMMARY),
  sep = "/"
)


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# # 1. To load TBLs, and then convert them into DTs
load_report.data <- function (slug.name) {
  # ## Load a .RData file that includes TBLs for the given `slug_name`
  obj.name <-
    tolower(slug.name) %>%
      paste0("tbl_", .)
  file.to.load <-
    str_replace(slug.name, "_", "-") %>%
      toupper(.) %>%
      paste0(., "_Tibble.RData")
  load_most.recent.data(DIR_TO.LOAD_REPORTS, file.to.load)
  # ## Transform TBLs in the list to DTs
  list_ <-
    get(obj.name) %>%
      lapply(., transform_tbl.to.dt)
  # ## Create a selector that excludes empty DTs
  select <-
    lapply(
      list_,
      function (x) names(x) %>%
        str_detect(., "^tbl$", negate = TRUE) %>%
        all(.)
    ) %>%
      as.vector(., mode = "logical")
  # ## Create a combined DT
  dt <- rbindlist(list_[select])
  # ## Modify the DT created
  dt_modified <- dt[, .N, by = names(dt)][, N := NULL]
  # ## Note:
  # ## There are duplicated observations.
  # ## (e.g., for `MD_DA840`, observations with `report_date` == "12/29/1997")
  # ## Return the modified DT
  return (dt_modified)
}


# # 2. To extract `slug_name` from a DT
# ## Note:
# ## For several reports, there exist discripancies between `slug_name` in the
# ## list and that in the actual data.
get_slug.name.in.dt <- function (dt) {
  slug.names <- dt$"slug_name" %>%
    unique(.) %>%
    as.vector(., mode = "character")
  return (slug.names)
}


# # 3. For tasks associated with variable names
# # 3.1. To get variable names of a DT
get_var.names <- function (dt) {
  tmp_slug.names <-
    dt$"slug_name" %>%
      unique(.)
  tmp_slug.Names <-
    dt$"slug_Name" %>%
      unique(.)
  # In several reports, `slug_Name` is used, instead of `slug_name`
  slug.names <-
    c(tmp_slug.names, tmp_slug.Names) %>%
      unique(.)
  dt_ <- setDT(NULL)
  for (name in slug.names) {
    tmp_dt <- data.table(slug_name = name)
    cols_to.add <-
      names(dt) %>%
        str_detect(
          .,
          "(^slug_name$)|(^slug_name_report$)|(^slug_Name$)",
          negate = TRUE
        ) %>%
        names(dt)[.] %>%
        tolower(.)
    tmp_dt[, (cols_to.add) := TRUE]
    dt_ <- rbind(dt_, tmp_dt)
  }
  return (dt_)
}

# # 4. To rename variable names
# ## Note:
# ## There are columns that include the same information but have different
# ## names (e.g., `average_price` vs `avg_price`). This fact implies the
# ## necessity of normalization in variable names.
rename_vars_for.consistency <- function (dt) {
  dt_ <- copy(dt)
  var.names_old <- names(dt_)
  var.names_old_lower <- tolower(var.names_old)
  var.names_new <- copy(var.names_old_lower)
  var.names_old_select <-
    var.names_old_lower[var.names_old_lower %in% names(list_change.var.names)]
  if (length(var.names_old_select) != 0) {
    for (name in var.names_old_select) {
      idx <- which(var.names_new == name)
      var.names_new[idx] <- list_change.var.names[[name]]
    }
  }
  names(dt_) <- var.names_new
  return (dt_)
}


# # 5. To create a DT that includes the summary of variables in reports
create_dt_summary <- function (slug.name) {
  select <-
    dt_var_renamed[slug_name == slug.name, .SD, .SDcols = cols_to.extract] %>%
      as.vector(., mode = "logical")
  select_excl.na <- !is.na(select)
  cols_to.extract_exist <- cols_to.extract[select_excl.na]
  cols_to.extract_not.exist <- cols_to.extract[!select_excl.na]
  dt <- dts_report_renamed[[slug.name]][
    , .SD, .SDcols = cols_to.extract_exist
  ][
    , .N, by = cols_to.extract_exist
  ][
    , N := NULL
  ]
  dt[, slug_name := slug.name]
  dt[, (cols_to.extract_not.exist) := NA]
  setcolorder(dt, c("slug_name", cols_to.extract))
  return (dt)
}


# # 5. To create a DT that includes information about publication frequency
# ## Note:
# ## 1) The variable `report_date` means the date for which a report includes
# ## price- or quantity-related information.
# ## 2) The variable `published_date` means the date on which a report is
# ## published. Note that for a report, the date is the most recent pulished
# ## date. Therefore, if a correction is released, then `published_date`
# ## does not include the date on which the report is publicated initially.
# ## 3) `report_date` <= `published_date` because corrected reports can be
# ## released (e.g., For `AMS_1235`, corrected data for 2018-01-13 is released
# ## on 2021-12-08.).
get_publication.info <- function (dt) {
  dt_to.return <- setDT(NULL)
  if (dt[, .N] == 0) { # For reports with no data
    return (dt_to.return)
    break
  }

  names_ <- names(dt)
  if ("report_date" %in% names_) {
    col <- "report_date"
    select <- c("slug_name", col)
  } else {
    col <- "published_date"
    select <- c("slug_name", col)
  }
  # ## Note:
  # ## Several reports do not include column `report_date`. For those reports,
  # ## use `published_date`.

  dt_selected <- dt[, .SD, .SDcols = select]
  setnames(dt_selected, old = col, new = "report_date")
  dt_selected[
    ,
    report_date :=
      str_extract(report_date, "[0-9]{2}/[0-9]{2}/[0-9]{4}") %>%
        as.Date(., format = "%m/%d/%Y")
  ] %>%
    .[, .N, keyby = .(slug_name, report_date)] %>%
    .[, N := NULL]
  dt_selected[, report_year.month := zoo::as.yearmon(report_date)]
  dt_selected[, report_year := year(report_date)]

  slug.names <- dt_selected[, .N, by = .(slug_name)]$slug_name
  for (sn in slug.names) {
    year.months <- dt_selected[
      slug_name == sn, .N, keyby = .(report_year.month)
    ]$report_year.month
    year.month_first <- year.months[1]
    year.month_last <- year.months[length(year.months)]
    N_year.month <-
      dt_selected[slug_name == sn, .N, by = .(report_year.month)][, .N]
    avg.pub.num_per.year.month <-
      dt_selected[
        slug_name == sn,
        .N,
        by = .(report_date, report_year.month)
      ] %>%
        .[, .N, by = .(report_year.month)] %>%
        .$N %>%
        sum(.) / N_year.month

    tmp_dt <- data.table(
      tmp_slug.name = sn,
      report_year.month_first = year.month_first,
      report_year.month_last = year.month_last,
      publication.number_per.month =
        round(avg.pub.num_per.year.month, digits = 1)
    )
    tmp_dt[
      ,
      frequency_days := floor((365 / 12) / avg.pub.num_per.year.month)
    ]
    dt_to.return <- rbind(dt_to.return, tmp_dt)
  }
  return (dt_to.return)
}


# ------------------------------------------------------------------------------
# Make a DT showing whether a variable name is in a specific report.
# ------------------------------------------------------------------------------
# ------- Load data files downloaded by using B0-02-02A -------
# # 1. Load the list of reports
# # 1.1. Load the list of reports
load_most.recent.data(DIR_TO.LOAD_LISTS, FILE_TO.LOAD_LISTS)

# # 1.2. Make a vector that includes `slug_name`
slug.names <- dt_report$slug_name
names(slug.names) <- slug.names


# # 2. Create a list that includes reports (in DT date type)
dts_report <- lapply(slug.names, load_report.data)
rm(list = ls()[str_detect(ls(), "^tbl\\_")])
# ## Note:
# ## Remove TBLs loaded from the memory.


# # 3. Conduct simple tests
# # 3.1. Create objects that will be used later
list_test <- lapply(dts_report, get_slug.name.in.dt)
tbl_test <- tibble(slug_name_report = names(list_test))
tbl_test$slug_name <- list_test
dt_test <- unnest_longer(tbl_test, slug_name) %>% setDT(.)

# # 3.2. Do tests
dt_test[, .N, by = .(slug_name_report)][N > 1, .N] != 0
dt_test[slug_name_report != slug_name]
# ## Note:
# ## Several `slug_name_report` have multiple `slug_name`. Using `slug_name`(s)
# ## in each DT is necessary to identify the exact report.
dt_test[, .N, by = .(slug_name)][N > 1]
dt_test[is.na(slug_name), .N]
dt_test[!is.na(slug_name_report) & is.na(slug_name)]
# ## Note:
# ## Those `slug_name_report`s have no data.


# ------- Create a DT showing whether a var. name is in a report -------
# # 1. Create a DT
# # 1.1. Create a DT
dt_var <-
  lapply(dts_report, get_var.names) %>%
    rbindlist(., fill = TRUE, idcol = "slug_name_report")
vars <-
  names(dt_var) %>%
    sort(.)
length(vars)
# ## Note:
# ## There are 193 variables.


# # 2. Research on variable names
sapply(
  vars[str_detect(vars, "price")],
  function (x) dt_var[get(x) == TRUE, .(slug_name)][, .N]
)
tmp_slug.name <- dt_var[price_min == T]$slug_name
dt_var[slug_name %in% tmp_slug.name][price_max == T, .N]
dt_var[slug_name %in% tmp_slug.name][mostly_low_price == T, .N]
dt_var[slug_name %in% tmp_slug.name][mostly_high_price == T, .N]
tmp_slug.name <- dt_var[avg_price_min == T]$slug_name
dt_var[slug_name %in% tmp_slug.name][avg_price_max == T, .N]
dt_var[slug_name %in% tmp_slug.name][avg_price == T, .N]
tmp_slug.name <- dt_var[price_unit == T]$slug_name
dt_var[slug_name %in% tmp_slug.name][price_min == T, .N]
dt_var[slug_name %in% tmp_slug.name][avg_price == T, .N]
# ## Note: Those results imply the necessity of normalization in variable names.

sapply(
  vars[str_detect(vars, "avg_weight")],
  function (x) dt_var[get(x) == TRUE, .(slug_name)][, .N]
)
sapply(
  vars[str_detect(vars, "category")],
  function (x) dt_var[get(x) == TRUE, .(slug_name)][, .N]
)
sapply(
  vars[str_detect(vars, "commodity")],
  function (x) dt_var[get(x) == TRUE, .(slug_name)][, .N]
)
sapply(
  vars[str_detect(vars, "crop")],
  function (x) dt_var[get(x) == TRUE, .(slug_name)][, .N]
)
sapply(
  vars[str_detect(vars, "market")],
  function (x) dt_var[get(x) == TRUE, .(slug_name)][, .N]
)
sapply(
  vars[str_detect(vars, "group")],
  function (x) dt_var[get(x) == TRUE, .(slug_name)][, .N]
)
sapply(
  vars[str_detect(vars, "acre")],
  function (x) dt_var[get(x) == TRUE, .(slug_name)][, .N]
)
sapply(
  vars[str_detect(vars, "number")],
  function (x) dt_var[get(x) == TRUE, .(slug_name)][, .N]
)
sapply(
  vars[str_detect(vars, "vol")],
  function (x) dt_var[get(x) == TRUE, .(slug_name)][, .N]
)
sapply(
  vars[str_detect(vars, "breakdown")],
  function (x) dt_var[get(x) == TRUE, .(slug_name)][, .N]
)


# ------- Normalize variable names -------
# # 1. Normalize variable names
# # 1.1. Load a list that includes old and new variable names
source(PATH_TO.LOAD_DIC)

# # 1.2. Rename variables
dts_report_renamed <- lapply(dts_report, rename_vars_for.consistency)

# # 1.3. Create a DT showing whether a variable name is in a specific report
dt_var_renamed <-
  lapply(dts_report_renamed, get_var.names) %>%
    rbindlist(., fill = TRUE, idcol = "slug_name_report")

dt_var_renamed[
  !slug_name %in% dt_var$slug_name,
  .N,
  by = .(slug_name_report, slug_name)
]
vars_renamed <-
  names(dt_var_renamed) %>%
    sort(.)
length(vars_renamed)


# ------------------------------------------------------------------------------
# Create a DT including the summary of variables in reports
# ------------------------------------------------------------------------------
# ------- Create a DT -------
# # 1. Create object(s) that will be used later
# str_test <- "start_date"
# sapply(
#   vars_renamed[str_detect(vars_renamed, str_test)],
#   function (x) dt_var_renamed[get(x) == TRUE, .(slug_name)][, .N]
# )
# dt_var_renamed[get(str_test) == T, .(slug_name)]

cols_to.extract <- c(
  "category", "class", "commodity", "crop", "community",
  "grade", "group",
  "market_type", "market_type_category", "market_location_name",
  "office_name", "origin",
  "package",
  "region", "report_title",
  "sale_type"
)
cols_to.add <- c(
  "avg_price", "avg_price_min", "avg_price_max", "avg_price_weekly",
  "avg_price_monthly", "avg_price_yearly",
  "price_max", "price_min",
  "close_price",
  "mostly_low_price", "mostly_high_price",
  "avg_weight",  # Cattle
  "avg_length", "bale_count", "crop_year", "nass_acres_planted_total", # Cotton
  "volume" # Egg
)


# # 2. Create a DT
dt_summary <-
  lapply(dt_report$slug_name, create_dt_summary) %>%
    rbindlist(.)

# # 2.1. Add columns
# # 2.1.1. Columns selected from `dt_var_renamed`
dt_summary_appended <- merge(
  x = dt_summary,
  y = dt_var_renamed[, .SD, .SDcols = c("slug_name", cols_to.add)],
  by = "slug_name",
  all.x = TRUE
)
# # 2.1.2. Columns showing information about publication frequency
# # 2.1.2.1. Create DT including info. about publication frequency
dt_frequency <-
  lapply(dts_report_renamed, get_publication.info) %>%
    rbindlist(., idcol = "slug_name")
dt_frequency[slug_name != tmp_slug.name, slug_name := tmp_slug.name]
dt_frequency[, tmp_slug.name := NULL]
# # 2.1.2.2. Add columns
dt_summary_appended <- merge(
  x = dt_summary_appended,
  y = dt_frequency,
  by = "slug_name",
  all.x = TRUE
)


# ------------------------------------------------------------------------------
# Export DTs
# ------------------------------------------------------------------------------
# ------- Export DTs -------
# # 1. Export the DT including reports in .RData format
save(
  dts_report_renamed, dt_summary_appended, dt_var_renamed,
  file = PATH_TO.SAVE_REPORTS
)

# # 2. Export the DT including summary information in .xlsx format
# # 2.1. Create a Workbook object
# # 2.1.1. Create a Workbook object
wb <- createWorkbook()
# # 2.1.2. Add sheets
# # 2.1.2.1. For detailed information
addWorksheet(wb, SHEET.NAME_DETAIL)
writeDataTable(wb, sheet = SHEET.NAME_DETAIL, x = dt_summary_appended)
# # 2.1.2.2. For variable names
addWorksheet(wb, SHEET.NAME_VAR)
writeDataTable(wb, sheet = SHEET.NAME_VAR, x = dt_var_renamed)

# # 2.2. Save the Workbook in .xlsx format
stopifnot(
  saveWorkbook(
    wb, file = PATH_TO.SAVE_SUMMARY, overwrite = TRUE, returnValue = TRUE
  )
)
