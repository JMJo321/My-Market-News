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
# (Not Applicable)


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# (...)
# ------------------------------------------------------------------------------
# ------- (...) -------
DIR_TO.LOAD_LISTS <- paste(PATH_DATA_INTERMEDIATE, "MMN-API/Lists", sep = "/")
FILE_TO.LOAD_LISTS <- "MMN_Lists-from-MMN-API.RData"
load_most.recent.data(DIR_TO.LOAD_LISTS, FILE_TO.LOAD_LISTS)



# ------------------------------------------------------------------------------
# (...)
# ------------------------------------------------------------------------------
# ------- (...) -------
DIR_TO.LOAD_REPORTS <- paste(PATH_DATA_RAW, "MMN-API/Tibbles", sep = "/")

slug.names <- dt_report$slug_name
names(slug.names) <- slug.names

load_report.data <- function (slug.name) {
  # ## Load a .RData file
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
  return (dt)
}


dts_report <- lapply(slug.names, load_report.data)
rm(list = ls()[str_detect(ls(), "^tbl\\_")])



get_slug.name.in.dt <- function (dt) {
  slug.names <- dt$"slug_name" %>%
    unique(.) %>%
    as.vector(., mode = "character")
  return (slug.names)
}

list_test <- lapply(dts_report, get_slug.name.in.dt)
tbl_test <- tibble(slug_name_report = names(list_test))
tbl_test$slug_name <- list_test
dt_test <- unnest_longer(tbl_test, slug_name) %>% setDT(.)

dt_test[, .N, by = .(slug_name_report)][N > 1]
# ## Note:
# ## Several `slug_name_report` have multiple `slug_name`.
dt_test[, .N, by = .(slug_name)][N > 1]
dt_test[is.na(slug_name), .N]
dt_test[!is.na(slug_name_report) & is.na(slug_name)]
# ## Note:
# ## Those `slug_name_report`s have no data.
dt_test[slug_name_report != slug_name]







get_var.names <- function (dt) {
  tmp_slug.names <-
    dt$"slug_name" %>%
      unique(.)
  tmp_slug.Names <-
    dt$"slug_Name" %>%
      unique(.)
  slug.names <- c(tmp_slug.names, tmp_slug.Names) %>% unique(.)
  dt_ <- setDT(NULL)
  for (name in slug.names) {
    tmp_dt <- data.table(slug_name = name)
    cols_to.add <-
      names(dt) %>%
        str_detect(., "(^slug_name$)|(^slug_name_report$)|(^slug_Name$)", negate = TRUE) %>%
        names(dt)[.] %>%
        tolower(.)
    tmp_dt[, (cols_to.add) := TRUE]
    dt_ <- rbind(dt_, tmp_dt)
  }
  return (dt_)
}



dt_var <-
  lapply(dts_report, get_var.names) %>%
    rbindlist(., fill = TRUE, idcol = "slug_name_report")

dt_var[slug_name != slug_name_report, .N, by = .(slug_name_report, slug_name)]
dt_test[!slug_name %in% dt_var$slug_name]
dt_test[!slug_name_report %in% dt_var$slug_name_report]
# ## Note:
# ## 612 = 628 - 32 + (6 * 2).




vars <- names(dt_var) %>% sort(.)
length(vars)
# ## Note:
# ## There are 176 variables.

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


# ------------------------------------------------------------------------------
# (...)
# ------------------------------------------------------------------------------
# ------- (...) -------
sapply(
  vars[str_detect(vars, "begin")],
  function (x) dt_var[get(x) == TRUE, .(slug_name)][, .N]
)
dt_var[begin_date == T, .(slug_name)]
dt_var[end_date == T, .(slug_name)]

sapply(
  vars[str_detect(vars, "acre")],
  function (x) dt_var[get(x) == TRUE, .(slug_name)][, .N]
)
dt_var[number == T, .(slug_name)]

sapply(
  vars[str_detect(vars, "number")],
  function (x) dt_var[get(x) == TRUE, .(slug_name)][, .N]
)
dt_var[number == T, .(slug_name)]
dt_var[publish_number == T, .(slug_name)]

sapply(
  vars[str_detect(vars, "vol")],
  function (x) dt_var[get(x) == TRUE, .(slug_name)][, .N]
)

sapply(
  vars[str_detect(vars, "breakdown")],
  function (x) dt_var[get(x) == TRUE, .(slug_name)][, .N]
)
dt_var[`% breakdown` == T, .(slug_name)]

# ------------------------------------------------------------------------------
# (...)
# ------------------------------------------------------------------------------
# ------- (...) -------

slug.names_to.modify <- dt_var[begin_date == T | `beginning date` == T]$slug_name
for (name in slug.names_to.modify) {
  dts_report[[name]] <- dts_report[[name]][
    ,
    c("report_begin_date", "report_end_date") := NULL
  ]
}


DIR_TO.LOAD_DIC <- PATH_SCRIPTS
FILE_TO.LOAD_DIC <- "D-My-Market-News.R"
PATH_TO.LOAD_DIC <- paste(DIR_TO.LOAD_DIC, FILE_TO.LOAD_DIC, sep = "/")
source(PATH_TO.LOAD_DIC)


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

dts_report_renamed <- lapply(dts_report, rename_vars_for.consistency)

dt_var_renamed <-
  lapply(dts_report_renamed, get_var.names) %>%
    rbindlist(., fill = TRUE, idcol = "slug_name_report")

dt_var_renamed[!slug_name %in% dt_var$slug_name, .N, by = .(slug_name_report, slug_name)]


vars_renamed <- names(dt_var_renamed) %>% sort(.)
length(vars_renamed)




# ------------------------------------------------------------------------------
# (...)
# ------------------------------------------------------------------------------
# ------- (...) -------









str_test <- "gin_type"
sapply(
  vars_renamed[str_detect(vars_renamed, str_test)],
  function (x) dt_var_renamed[get(x) == TRUE, .(slug_name)][, .N]
)
dt_var_renamed[get(str_test) == T, .(slug_name)]

vars_renamed


cols_to.extract <- c(
  "category", "class", "commodity", "crop", "community", "grade", "group",
  "market_type", "market_type_category", "office_name", "origin", "package",
  "region", "sale_type", "gin_type"
)
# slug.name <- names(dts_report_renamed)["LSD_MARS_1877" == names(dts_report_renamed)]
# slug.name <- names(dts_report_renamed)[193]
# slug.name <- dt_var_renamed$slug_name[193]

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

dt_summary <- lapply(dt_report$slug_name, create_dt_summary) %>% rbindlist(.)

fwrite(dt_summary, file = "Summary.csv")


# ------------------------------------------------------------------------------
# (...)
# ------------------------------------------------------------------------------
# ------- (...) -------



# ------------------------------------------------------------------------------
# (...)
# ------------------------------------------------------------------------------
# ------- (...) -------
