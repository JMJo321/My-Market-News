# < Description > *
# > Script Group Indicator Number and Name
# # : B-05, Build-Shiny-Apps
# #
# > Script Number(s)
# # : B-05-01A
# #
# > Purpose of the script(s)
# # : To build a R Shiny app for `category == "Cattle"`.

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(stringr)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(zoo)
library(lubridate)
library(unikn)
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
# # 1.1. Path(s) for data file(s)
# # 1.1.1. Path for Report Data
DIR_TO.LOAD_DATA_MMN <- paste(PATH_DATA_INTERMEDIATE, "MMN-API/Data", sep = "/")
FILE_TO.LOAD_CATTLE <- "MMN_Data_Category-Cattle_For-Shiny-App.RData"
# # 1.1.2. Path for the List of Quality Measures
FILE_TO.LOAD_QUALITY.MEASURE <- "MMN_Data_List-of-Quality-Measures.RData"
# # 1.1.3. Path for the BLS PPI data
DIR_TO.LOAD_DATA_PPI <- paste(PATH_DATA_INTERMEDIATE, "BLS", sep = "/")
FILE_TO.LOAD_PPI <- "BLS_PPI.RDaata"

# # 1.2. Path(s) for R script(s)
# # 1.2.1. Path for the script including default style
FILE_TO.LOAD_STYLE <- "default_style.R"
PATH_TO.LOAD_STYLE <-
  paste(PATH_SCRIPTS_BUILDING_APPS, FILE_TO.LOAD_STYLE, sep = "/")
# # 1.2.2.
FILE_TO.LOAD_UI <- "ui.R"
PATH_TO.LOAD_UI <-
  paste(PATH_SCRIPTS_BUILDING_APPS, FILE_TO.LOAD_UI, sep = "/")
# # 1.2.3.
FILE_TO.LOAD_SERVER_SUMMARY_STATE <- "server_summary_state.R"
PATH_TO.LOAD_SERVER_SUMMARY_STATE <- paste(
  PATH_SCRIPTS_BUILDING_APPS, FILE_TO.LOAD_SERVER_SUMMARY_STATE, sep = "/"
)
# # 1.2.4.
FILE_TO.LOAD_SERVER_SUMMARY_PRODUCT <- "server_summary_product.R"
PATH_TO.LOAD_SERVER_SUMMARY_PRODUCT <- paste(
  PATH_SCRIPTS_BUILDING_APPS, FILE_TO.LOAD_SERVER_SUMMARY_PRODUCT, sep = "/"
)
# # 1.2.5.
FILE_TO.LOAD_SERVER_AUCTION_SINGLE <- "server_auction_single.R"
PATH_TO.LOAD_SERVER_AUCTION_SINGLE <- paste(
  PATH_SCRIPTS_BUILDING_APPS, FILE_TO.LOAD_SERVER_AUCTION_SINGLE, sep = "/"
)
# # 1.2.6.
FILE_TO.LOAD_SERVER_AUCTION_MULTIPLE <- "server_auction_multiple.R"
PATH_TO.LOAD_SERVER_AUCTION_MULTIPLE <- paste(
  PATH_SCRIPTS_BUILDING_APPS, FILE_TO.LOAD_SERVER_AUCTION_MULTIPLE, sep = "/"
)


# ------- Define parameter(s) -------
# # 1. Lists about quality measures
# # 1.1. List mapping a commodity to applicable quality measures
# ## Note:
# ## This list is loaded below.

# # 1.2. List mapping an input ID to a label
list_quality.measures_detail <- list(
  frame =
    list(inputId = "frame", label = "Frame"),
  muscle_grade =
    list(inputId = "muscle.grade", label = "Muscle Grade"),
  quality_grade_name =
    list(inputId = "quality.grade.name", label = "Quality Grade"),
  yield_grade =
    list(inputId = "yield.grade", label = "Yield Grade"),
  dressing =
    list(inputId = "dressing", label = "Dressing"),
  pregnancy_stage =
    list(inputId = "pregnancy.stage", label = "Pregnancy Stage"),
  offspring_weight_est =
    list(inputId = "offspring.weight.est", label = "Offspring Weight Est.")
)


COLOR.PAL_SIGNAL <- usecol(pal = "signal", n = 4)


# ------- Define function(s) -------
# # 1. Quality-measure-related function(s)
# # 1.1. To generate a condition in order to render a reactive HTML
get_condition_uioutput_quality.measure <- function (
  quality.measure.info_list,
  is.in.quality.measures_binary,
  prefix_in.str
) {
  id_input <- quality.measure.info_list[["inputId"]]
  label <- quality.measure.info_list[["label"]]
  varname <- str_replace_all(id_input, "\\.", "_")
  if (is.in.quality.measures_binary) {
    condition <- paste0(
      'selectInput(inputId = "',
      paste(prefix_in.str, id_input, sep = "_"),
      '", label = "',
      label,
      paste0(
        '", choices = dt_cattle[market_type_category %in% "Auction" & ',
        'category %in% input$', prefix_in.str, '_category & ',
        'class %in% input$', prefix_in.str, '_class & ',
        'commodity %in% input$', prefix_in.str, '_commodity, .N, keyby = .('
      ),
      varname,
      ')]$',
      varname,
      ')'
    )
  } else {
    condition <- paste0(
      'selectInput(inputId = "',
      paste(prefix_in.str, id_input, sep = "_"),
      '", label = "', label,
      '", choices = "NA")'
    )
  }
  return(condition)
}

# # 1.2. To generate a condition in order to subset a DT
get_condition_qaulity.measures <- function (quality.measures_) {
  if (length(quality.measures_) == 0) {
    return(NA)
  } else {
    str_replace_all(quality.measures_, "_", ".") %>%
      paste0(quality.measures_, " %in% input$", .) %>%
      paste(., collapse = " & ") %>%
      paste(
        paste0(
          "market_type_category %in% 'Auction' & category %in% input$category",
          " & class %in% input$class & commodity %in% input$commodity"
        ),
        .,
        sep = " & "
      ) %>%
      return(.)
  }
}

# # 2. Price-related function(s)
# # 2.1. To convert prices into real terms
get_real.price <- function (year.month_yearmon, price) {
  converting.factor <-
    dt_ppi[year.month == year.month_yearmon]$value /
      dt_ppi[year.month == as.yearmon("Jan 2022", format = "%b %Y")]$value
  adjusted.price <- round(price * converting.factor, 2)
  return (adjusted.price)
}



help_get.empty.dt.for.given.product.and.date <- function (product_in.list, date_in.date) {
  dt_to.return <- product_in.list %>% as.data.table(.) %>% copy(.)
  dt_to.return[, report_date := date_in.date]
  return (dt_to.return)
}

help_get.empty.dt.for.given.product <- function (product_in.list, dates_in.list) {
  dt_to.return <-
    lapply(
      dates_in.list,
      help_get.empty.dt.for.given.product.and.date,
      product_in.list = product_in.list
    ) %>%
      rbindlist(.)
  return (dt_to.return)
}




# ------------------------------------------------------------------------------
# Load required dataset(s) and/or script(s)
# ------------------------------------------------------------------------------
# ------- Load dataset(s) -------
# # 1. Load DTs including cattle-related data
# # 1.1. DT incl. MMN data for `category == "Cattle"`
load_most.recent.data(DIR_TO.LOAD_DATA_MMN, FILE_TO.LOAD_CATTLE)

# # 1.2. List incl. quality-measure-related info. for each product
load_most.recent.data(DIR_TO.LOAD_DATA_MMN, FILE_TO.LOAD_QUALITY.MEASURE)

# # 1.3. DT incl. BLS PPI
load_most.recent.data(DIR_TO.LOAD_DATA_PPI, FILE_TO.LOAD_PPI)


# ------- Load script(s) -------
source(PATH_TO.LOAD_STYLE)


# ------------------------------------------------------------------------------
# Build R Shiny App(s)
# ------------------------------------------------------------------------------
# ------- Build a UI definition -------
# # 1. Define the layout
source(PATH_TO.LOAD_UI)


# ------- Build a `server` -------
server <- function (input, output, session) {

  source(PATH_TO.LOAD_SERVER_SUMMARY_STATE, local = TRUE)
  source(PATH_TO.LOAD_SERVER_SUMMARY_PRODUCT, local = TRUE)
  source(PATH_TO.LOAD_SERVER_AUCTION_SINGLE, local = TRUE)
  source(PATH_TO.LOAD_SERVER_AUCTION_MULTIPLE, local = TRUE)




  cols_extract <- c(
  "slug_id", "slug_name", "market_type_category",
  "category", "class", "commodity",
  "report_date", "report_year", "report_yearmonth",
  "price_unit", "avg_price_min", "avg_price_max", "avg_price",
  "avg_weight", "head_count",
  "total_weight_min", "total_weight_max", "total_weight",
  "summary.report_pub.period", "summary.report_location", "product"
  )
  dt_summary.report <- dt_cattle[
    market_type_category %in% "Summary" &
        summary.report_pub.period %in% "Weekly" &
        price_unit %in% "Per Cwt"
  ][
    ,
    product := paste(commodity, class, category, sep = ", ")
  ][
    ,
    .SD, .SDcols = cols_extract
  ] %>%
    unique(.)



  cols_by <- c(
    "slug_name", "report_date", "price_unit",
    "summary.report_pub.period", "summary.report_location",
    "product"
  )
  dt_summary.report[
    ,
    dummy_weight.sum := sum(avg_weight, na.rm = TRUE),
    by = cols_by
  ][
    ,
    dummy_weight := avg_weight / dummy_weight.sum
  ][
    ,
    `:=` (
      weighted.avg.price_min =
        weighted.mean(avg_price_min, dummy_weight, na.rm = TRUE),
      weighted.avg.price_max =
        weighted.mean(avg_price_max, dummy_weight, na.rm = TRUE),
      weighted.avg.price =
        weighted.mean(avg_price, dummy_weight, na.rm = TRUE)
    ),
    by = cols_by
  ][
    ,
    `:=` (
      qty_head.count = sum(head_count, na.rm = TRUE),
      qty_total.weight_min = sum(total_weight_min, na.rm = TRUE),
      qty_total.weight_max = sum(total_weight_max, na.rm = TRUE),
      qty_total.weight = sum(total_weight, na.rm = TRUE)
    ),
    by = cols_by
  ]




  cols_unique <- c(
    "slug_id", "slug_name", "market_type_category",
    "category", "class", "commodity", "product", "report_date",
    "summary.report_pub.period", "summary.report_location", "price_unit",
    "weighted.avg.price_min", "weighted.avg.price_max", "weighted.avg.price",
    "qty_total.weight_min", "qty_total.weight_max", "qty_total.weight",
    "qty_head.count"
  )
  dt_summary.report_unique <-
    dt_summary.report[, .N, keyby = cols_unique][, N := NULL]


  cols_to.extend <- c(
    "slug_id", "slug_name", "market_type_category",
    "summary.report_pub.period", "summary.report_location",
    "product", "category", "class", "commodity",
    "price_unit"
  )

  dt_summary.report_dates <-
    dt_summary.report_unique[, .N, keyby = .(report_date)][, N :=  NULL]
  dt_summary.report_product <-
    dt_summary.report_unique[, .N, keyby = cols_to.extend][, N := NULL]



  dates_in.list <-
    dt_summary.report_dates$report_date %>%
      as.list(.)
  list.of.dt_products <- lapply(
    1:dt_summary.report_product[, .N],
    function (x) {as.list(dt_summary.report_product[x])}
  )


  dt_empty <- lapply(
    list.of.dt_products,
    help_get.empty.dt.for.given.product,
    dates_in.list = dates_in.list
  ) %>%
    rbindlist(.)
  # dt_empty <- parallel::mclapply(
  #   list.of.dt_products,
  #   help_get.empty.dt.for.given.product,
  #   dates_in.list = dates_in.list,
  #   mc.cores = 4
  # ) %>%
  #   rbindlist(.)

  dt_summary.report_extended <- merge(
    x = dt_empty,
    y = dt_summary.report_unique,
    by = names(dt_empty),
    all.x = TRUE
  )


  cols_to.update <-
    names(dt_summary.report_extended) %>%
      str_detect(., "(total.weight)|(head.count)") %>%
      names(dt_summary.report_extended)[.]
  # ## Note:
  # ## Because `geom_area` is used to illustrate q'ty-related info., NA values
  # ## should be replaced with 0s.
  for (col in cols_to.update) {
    dt_summary.report_extended[is.na(get(col)), (col) := 0]
  }

  dt_summary.report_extended[
    ,
    `:=` (
      report_yearmonth = as.yearmon(report_date),
      report_year = year(report_date)
    )
  ]

  values <- reactiveValues(
    interval.length = 100,
    multiple_market.location.selected = NULL,
    # market.location_selected = NULL,
    figure.type = NULL,
    state_product.selected = NULL,
    product_location.selected = NULL
  )
}  # End of server


# ------- Build a Shiny app object, and then execute it -------
app <- shinyApp(ui = ui, server = server)
runApp(app, launch.browser = TRUE)
