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
library(plotly)
library(zoo)
library(lubridate)
library(ggplot2)
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
# # 1.1. Path for Report Data
DIR_TO.LOAD_CATTLE <- paste(PATH_DATA_INTERMEDIATE, "MMN-API/Data", sep = "/")
FILE_TO.LOAD_CATTLE <- "MMN_Data_Category-Cattle.RData"

# # 1.2. Path for the script including default style
FILE_TO.LOAD_STYLE <- "default_style.R"
PATH_TO.LOAD_STYLE <-
  paste(PATH_SCRIPTS_BUILDING_APPS, FILE_TO.LOAD_STYLE, sep = "/")


# ------- Define parameter(s) -------
# TODO: Make a function that generates the list below
# # 1. Lists about quality measures
# # 1.1. List mapping a commodity to applicable quality measures
list_quality.measures <- list(
  Cattle = list(
    Heifers = list(
      `Feeder Cattle` = c("frame", "muscle_grade"),
      `Slaughter Cattle` = c("quality_grade_name", "yield_grade", "dressing"),
      `Feeder Dairy Calves` = "quality_grade_name"
    )
  )
)

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


# ------- Define function(s) -------
# # 1. Quality-measure-related function(s)
# # 1.1. To generate a condition in order to render a reactive HTML
get_condition_uioutput_quality.measure <- function (
  quality.measure.info_list,
  is.in.quality.measures_binary
) {
  id_input <- quality.measure.info_list[["inputId"]]
  label <- quality.measure.info_list[["label"]]
  varname <- str_replace_all(id_input, "\\.", "_")
  if (is.in.quality.measures_binary) {
    condition <- paste0(
      'selectInput(inputId = "',
      id_input,
      '", label = "',
      label,
      paste0(
        '", choices = dt_cattle[market_type_category %in% "Auction" & ',
        'category %in% input$category & class %in% input$class & commodity %in%',
        ' input$commodity, .N, keyby = .('
      ),
      varname,
      ')]$',
      varname,
      ')'
    )
  } else {
    condition <- paste0(
      'selectInput(inputId = "', id_input, '", label = "', label,
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


# ------------------------------------------------------------------------------
# Load required dataset(s) and/or script(s)
# ------------------------------------------------------------------------------
# ------- Load dataset(s) -------
load_most.recent.data(DIR_TO.LOAD_CATTLE, FILE_TO.LOAD_CATTLE)


# ------- Load script(s) -------
source(PATH_TO.LOAD_STYLE)


# ------------------------------------------------------------------------------
# Build R Shiny App(s)
# ------------------------------------------------------------------------------
# ------- Build a UI definition -------
# # 1. Define the layout
tabpanels <- tabPanel(
  title = "Auction",
  sidebarPanel(
    width = 6,
    tags$h3("Product"),
    tags$h5("Select a Category, Class, and Commodity"),
    uiOutput("ui_category"),
    uiOutput("ui_class"),
    uiOutput("ui_commodity"),
    tags$hr(),
    tags$h3("Quality Measures"),
    tags$h5(
      "Quality Measure(s) with NA is(are) irrelevant to the Product Selected."
    ),
    uiOutput("ui_quality.measures_frame"),
    uiOutput("ui_quality.measures_muscle.grade"),
    uiOutput("ui_quality.measures_quality.grade.name"),
    uiOutput("ui_quality.measures_yield.grade"),
    uiOutput("ui_quality.measures_dressing"),
    uiOutput("ui_quality.measures_pregnancy.stage"),
    uiOutput("ui_quality.measures_offspring.weight.est"),
    uiOutput("ui_load.data")
  ),
  sidebarPanel(
    width = 6,
    tags$h3("Options For Plotting"),
    uiOutput("ui_interval.length"),
    uiOutput("ui_figure.type"),
    uiOutput("ui_data.level"),
    uiOutput("ui_market.location_state"),
    uiOutput("ui_market.location_city.with.state"),
    uiOutput("ui_add.market.location"),
    uiOutput("ui_market.location_selected"),
    uiOutput("ui_weight.bracket"),
    uiOutput("ui_time.period"),
    uiOutput("ui_qty"),
    uiOutput("ui_price"),
    uiOutput("ui_test")
  ),
  mainPanel(
    plotlyOutput(outputId = "figure_price"),
    plotlyOutput(outputId = "figure_qty"),
    dataTableOutput(outputId = "table"),
    width = 12
  )
)

# # 2. Build a `ui`
ui <- fluidPage(
  dashboardPage(
    header = dashboardHeader(disable = TRUE),
    sidebar = dashboardSidebar(disable = TRUE),
    body = dashboardBody(
      # ## Apply custom CSS
      tags$head(
        tags$style(HTML(default_style)),
        tabsetPanel(tabpanels)
      ),
      title = NULL,
      skin = "black"
    )
  )
)


# ------- Build a `server` -------
server <- function (input, output, session) {
  # # 1. Create reactive object(s)
  # # 1.1. Create a reactive object including quality measures
  quality.measures <- eventReactive(
  c(
    input$category,
    input$class,
    input$commodity
  ), {
    list_quality.measures[[input$category]][[input$class]][[input$commodity]]
  })

  # # 1.2. Create a `reactiveValues` object including reactive object(s) that
  # #      will be used later
  # # 1.2.0. Initialize the object
  values <- reactiveValues(
    interval.length = 100,
    market.location_selected = NULL,
    figure.type = NULL
  )
  # # 1.2.1. Update the values of the object
  # # 1.2.1.1. For `interval.length`
  observeEvent(
    c(
      input$load.data,
      input$interval.length
    ), {
    values$interval.length <- input$interval.length
  })
  # ## Note:
  # ## The value stored will be used to add a column including weight brackets.
  # # 1.2.2. For `market.location_selected`
  observeEvent(
    c(
      input$load.data,
      input$add.market.location
    ), {
    if (
      !is.null(input$figure.type) &
        !is.null(input$data.level) &
        !is.null(input$market.location_city.with.state)
    ) {
      if (!input$market.location_city.with.state %in% "NA") {
        values$market.location_selected <- c(
          values$market.location_selected,
          input$market.location_city.with.state
        )
      }
    } else {
      values$market.location_selected <- NULL
    }
  })
  observeEvent(
    c(
      input$load.data,
      input$data.level,
      input$market.location_selected
    ), {
    if (is.null(input$data.level)) {
      values$market.location_selected <- NULL
    }
      values$market.location_selected <- input$market.location_selected
    },
    ignoreInit = TRUE,
    ignoreNULL = FALSE
  )
  # ## Note:
  # ## Update `values$market.location_selected` when an item is deleted from
  # ## `values$market.location_selected`.

  observeEvent(
    c(
      input$load.data,
      input$data.level
    ), {
    values$figure.type <- input$figure.type
  },
    ignoreNULL = FALSE
  )
  observeEvent(
    c(
      input$load.data,
      input$figure.type
    ), {
    if (!is.null(values$figure.type)) {
      if (!values$figure.type %in% input$figure.type) {
        values$market.location_selected <- NULL
      }
    }
  })
  # ## Note:
  # ## Those two `observeEvent` are required to clear the previous selections
  # ## in `values$market.location_selected` when changing the choice for figure
  # ## type.

  # # 1.3. Reactive objects for choice values
  # # 1.3.1. About market locations
  choices_market.location <- eventReactive(
    input$load.data, {
    dt_subset()[
      ,
      .N,
      keyby = .(market_location_state, market_location_city, market_location)
    ]$market_location
  })
  choices_market.location_state <- eventReactive(
    input$load.data, {
    dt_subset()[
      ,
      .N,
      keyby = .(market_location_state)
    ]$market_location_state
  })
  choices_market.location_city.within.state.selected <- eventReactive(
    c(
      input$load.data,
      input$market.location_state
    ), {
    dt_subset()[
      market_location_state %in% input$market.location_state &
        !is.na(market_location_city),
      .N,
      keyby = .(market_location)
    ]$market_location
  })
  # # 1.3.2. About weight brackets
  choices_weight.bracket <- eventReactive(
  c(
    input$load.data,
    values$market.location_selected
  ), {
    dt_base()[
      market_location %in% values$market.location_selected,
      .N,
      keyby = .(brackets)
    ]$brackets
  })
  # # 1.3.3. About time period
  choices_date <- eventReactive(
  c(
    input$load.data,
    input$data.level,
    values$market.location_selected,
    input$weight.bracket
  ), {
    if (!is.null(input$data.level)) {
      if (input$data.level %in% "Aggregated Data") {
        dt_base()[
          market_location %in% values$market.location_selected,
          .N,
          keyby = .(report_date)
        ]$report_date
      } else {
        dt_base()[
          market_location %in% values$market.location_selected &
            brackets %in% input$weight.bracket,
          .N,
          keyby = .(report_date)
        ]$report_date
      }
    } else {
      NULL
    }
  })

  # # 1.4. Create miscellaneous reactive values
  # # 1.4.1. To select qty-related variable
  var_qty <- eventReactive(
    c(
      input$load.data,
      input$figure.type,
      input$data.level,
      input$qty
    ), {
    if (!is.null(input$data.level) & !is.null(input$qty)) {
      qty.data <- if (input$qty %in% "Headcounts") {
        "qty_headcount"
      } else if (input$qty %in% "Total Weights") {
        "qty_weight_total"
      } else {
        "qty_weight_avg"
      }
      if (input$data.level %in% "Aggregated Data") {
        paste0("agg.", qty.data)
      } else {
        qty.data
      }
    } else {
      NULL
    }
  })
  # # 1.4.2. To select price-related variable
  var_price <- eventReactive(
    c(
      input$load.data,
      input$figure.type,
      input$data.level,
      input$price
    ), {
    if (!is.null(input$data.level) & !is.null(input$price)) {
      price.data <- if (input$price %in% "Simple Average") {
        "price_avg_simple"
      } else {
        "price_avg_weighted"
      }
      if (input$data.level %in% "Aggregated Data") {
        paste0("agg.", price.data)
      } else {
        price.data
      }
    } else {
      NULL
    }
  })

  # # 1.5. Create reactive DTs
  # # 1.5.1. Create a DT subsetted based on quality measures
  dt_subset <- eventReactive(
    input$load.data, {
    dt_cattle[
      get_condition_qaulity.measures(quality.measures()) %>%
        parse(text = .) %>%
        eval(.)
    ]
  })
  # # 1.5.2. Create a DT including quantities and prices
  dt_base <- eventReactive(
    c(
      input$load.data,
      input$interval.length
    ), {
    dt <- dt_subset()[
      ,
      brackets := cut(
        avg_weight,
        breaks = seq(0, 10^4, by = as.numeric(values$interval.length)),
        include.lowest = TRUE,
        dig.lab = as.character(10^4) %>% str_length(.)
      )
    ]
    # # 1) Add column(s) containing quantity-related info.
    # # 1-1) Quantity-related info. aggregated at the market location level
    dt[
      ,
      `:=` (
        agg.qty_headcount = sum(head_count, na.rm = TRUE),
        agg.qty_weight_total = sum(head_count * avg_weight, na.rm = TRUE)
      ),
      by = .(report_year, report_yearmonth, report_date, market_location)
    ]
    dt[
      ,
      agg.qty_weight_avg := round(
        agg.qty_weight_total / agg.qty_headcount, digits = 0
      ),
      by = .(report_year, report_yearmonth, report_date, market_location)
    ]
    # # 1-2) Quantity-related info. at the weight bracket level
    dt[
      ,
      `:=` (
        qty_headcount = sum(head_count, na.rm = TRUE),
        qty_weight_total = sum(head_count * avg_weight, na.rm = TRUE)
      ),
      by =
        .(report_year, report_yearmonth, report_date, market_location, brackets)
    ]
    dt[
      ,
      qty_weight_avg := round(qty_weight_total / qty_headcount, digits = 0),
      by =
        .(report_year, report_yearmonth, report_date, market_location, brackets)
    ]
    # # 2) Add column(s) containing price-related info.
    # # 2-1) Price-related info. aggregated at the market location level
    dt[
      ,
      agg.weights := qty_weight_total / sum(qty_weight_total, na.rm = TRUE),
      by = .(report_year, report_yearmonth, report_date, market_location)
    ]
    dt[
      ,
      `:=` (
        agg.price_avg_simple = (
          mean(avg_price, na.rm = TRUE) %>%
            round(., digits = 2)
        ),
        agg.price_avg_weighted = (
          weighted.mean(avg_price, agg.weights, na.rm = TRUE) %>%
            round(., digits = 2)
        )
      ),
      by = .(report_year, report_yearmonth, report_date, market_location)
    ]
    # # 2-2) Price-related info. at the weight bracket level
    dt[
      ,
      weights := qty_weight_total / sum(qty_weight_total, na.rm = TRUE),
      by =
        .(report_year, report_yearmonth, report_date, market_location, brackets)
    ]
    dt[
      ,
      `:=` (
        price_avg_simple = (
          mean(avg_price, na.rm = TRUE) %>%
            round(., digits = 2)
        ),
        price_avg_weighted = (
          weighted.mean(avg_price, weights, na.rm = TRUE) %>%
            round(., digits = 2)
        )
      ),
      by =
        .(report_year, report_yearmonth, report_date, market_location, brackets)
    ]
  })
  # # 1.5.3. Create a DT by subsetting `dt_base` based on market locations
  # #        selected
  dt_for.plot <- eventReactive(
    c(
      input$load.data,
      dt_base(),
      values$market.location_selected,
      input$data.level,
      input$time.period,
      var_qty(),
      var_price(),
      input$weight.bracket
    ), {
    # # 1) Create a DT for making figures
    # # 1-1) Create a DT by subsetting the given DT and then dropping
    # #      duplicated observations
    cols_by.weight.bracket <- c(
      "report_year", "report_yearmonth","report_date",
      "brackets", "market_location",
      "agg.qty_headcount", "agg.qty_weight_total", "agg.qty_weight_avg",
      "qty_headcount", "qty_weight_total", "qty_weight_avg",
      "agg.price_avg_simple", "agg.price_avg_weighted",
      "price_avg_simple", "price_avg_weighted"
    )
    if (!is.null(values$market.location_selected) & !is.null(input$data.level)) {
      dt <- dt_base()[
        market_location %in% values$market.location_selected,
        .N,
        keyby = cols_by.weight.bracket
      ][
        , N := NULL
      ]

      # # 1-2) Generate a DT by melting the DT created above
      id.vars_by.weight.bracket <- c(
        "report_year", "report_yearmonth", "report_date",
        "brackets", "market_location"
      )
      measure.vars_by.weight.bracket <- cols_by.weight.bracket[
        !(cols_by.weight.bracket %in% id.vars_by.weight.bracket)
      ]
      suppressWarnings(
        dt_melted <- melt(
          dt,
          id.vars = id.vars_by.weight.bracket,
          measure.vars = measure.vars_by.weight.bracket
        )
      )

      # # 2) Modify the melted DT
      # # 2-1) Add column(s)
      # # 2-1-1) Add a column categorizing observations based on values of
      # #        the column `variable`
      dt_melted[
        ,
        category_upper := lapply(
          .SD,
          function (x) {
            str_detect(x, "(qty)|(headcount)") %>%
              lapply(
                .,
                function (y) if (y) {return("Quantity")} else {return("Price")}
              ) %>%
              as.vector(., mode = "character")
          }
        ),
        .SDcols = "variable"
      ]
      dt_subset <- if (input$data.level %in% "Detailed Data") {
        dt_melted[
          input$time.period[1] <= report_year &
            report_year <= input$time.period[2] &
            (variable %in% var_price() | variable %in% var_qty()) &
            brackets %in% input$weight.bracket
        ]
      } else {
        dt_melted[
          input$time.period[1] <= report_year &
            report_year <= input$time.period[2] &
            (variable %in% var_price() | variable %in% var_qty())
        ]
      }
      dates.in.dt <- dt_subset[
        , .N, by = .(report_date)
      ]$report_date
      brackets.in.dt <- dt_subset[
        , .N, by = .(brackets)
      ]$brackets
      market.locations.in.dt <- dt_subset[
        , .N, by = .(market_location)
      ]$market_location
      dt_to.merge <-
        expand.grid(
          report_date = dates.in.dt,
          brackets = brackets.in.dt,
          market_location = market.locations.in.dt,
          category_upper = c("Quantity", "Price")
        ) %>%
          setDT(.)
      dt_merged <- merge(
        x = dt_to.merge,
        y = dt_subset,
        by = c("report_date", "brackets", "market_location", "category_upper"),
        all.x = TRUE
      )
      dt_merged[is.na(value), value := 0]
    } else {
      NULL
    }
  })

  # # 1.6. Create reactive ggplot object(s)
  # # 1.6.1. For qty-related figure
  plot_qty <- eventReactive(
    c(
      input$load.data,
      input$figure.type,
      input$qty,
      dt_for.plot()
    ), {
    if (!is.null(dt_for.plot()) & !is.null(input$qty)) {
      n_obs <- nrow(dt_for.plot())
    } else {
      n_obs <- 0
    }
    if (n_obs > 0) {
      if (input$figure.type %in% "By Weight Bracket") {
        by_plot <- "brackets"
        labs <- "Weight\nBrackets"
      } else {
        by_plot <- "market_location"
        labs <- "Market\nLocations"
      }

      if (input$qty %in% "Average Weights") {
        ggplot.obj <-
          ggplot(
            data = dt_for.plot()[
              category_upper == "Quantity" &
                value != 0
            ]
          ) +
            geom_point(
              aes_string(x = "report_date", y = "value", color = by_plot),
              color = "black", alpha = 0.25, size = 1.3
            ) +
            geom_line(
              aes_string(
                x = "report_date", y = "value", color = by_plot, group = by_plot
              ),
              color = "black", alpha = 0.25, lwd = 0.7
            ) +
            geom_line(
              aes_string(x = "report_date", y = "value", color = by_plot),
              lwd = 0.3
            ) +
            geom_point(
                aes_string(x = "report_date", y = "value", color = by_plot),
                size = 0.8
            ) +
            scale_x_date(date_labels = "%b. %Y") +
            scale_y_continuous(label = scales::comma) +
            scale_color_brewer(palette = "Spectral") +
            labs(
              x = "",
              y = "",
              color = labs
            ) +
            theme_minimal()
      } else {
        ggplot.obj <-
          ggplot(
            data = dt_for.plot()[
              category_upper == "Quantity"
            ]
          ) +
            geom_area(
              aes_string(x = "report_date", y = "value", fill = by_plot)
            ) +
            scale_x_date(date_labels = "%b. %Y") +
            scale_y_continuous(label = scales::comma) +
            scale_fill_brewer(palette = "Spectral") +
            labs(
              x = "",
              y = "",
              fill = labs
            ) +
            theme_minimal()
      }
    } else {
      NULL
    }
  })
  # # 1.6.2. For price-related figure
  plot_price <- eventReactive(
    c(
      input$load.data,
      input$figure.type,
      input$price,
      dt_for.plot()
    ), {
    if (!is.null(dt_for.plot()) & !is.null(input$price)) {
      n_obs <- nrow(dt_for.plot())
    } else {
      n_obs <- 0
    }
    if (n_obs > 0) {
      if (input$figure.type %in% "By Weight Bracket") {
        by_plot <- "brackets"
        labs <- "Weight\nBrackets"
      } else {
        by_plot <- "market_location"
        labs <- "Market\nLocations"
      }
      ggplot.obj <-
        ggplot(
          data = dt_for.plot()[
            category_upper == "Price" & !is.na(variable)
          ]
        ) +
          geom_point(
            aes_string(x = "report_date", y = "value", group = by_plot),
            color = "black", alpha = 0.25, size = 1.3
          ) +
          geom_line(
            aes_string(x = "report_date", y = "value", group = by_plot),
            color = "black", alpha = 0.25, lwd = 0.7
          ) +
          geom_line(
            aes_string(x = "report_date", y = "value", color = by_plot),
            lwd = 0.3
          ) +
          geom_point(
            aes_string(x = "report_date", y = "value", color = by_plot),
            size = 0.8
          ) +
          scale_x_date(date_labels = "%b. %Y") +
          scale_y_continuous(label = scales::comma) +
          scale_color_brewer(palette = "Spectral") +
          labs(
            x = "",
            y = "",
            color = labs,
            subtitle = "Price"
          ) +
          theme_minimal()
    } else {
      NULL
    }
  })


  # ------- Create input control(s) -------
  # # 1. For a product
  # # 1.1. For `category`
  output$ui_category <- renderUI({
    selectInput(
      inputId = "category",
      label = "Category",
      choices = dt_cattle[, .N, keyby = .(category)]$category,
      selected = NULL
    )
  })

  # # 1.2. For `class`
  output$ui_class <- renderUI({
    selectInput(
      inputId = "class",
      label = "Class",
      choices = NULL
    )
  })
  observeEvent(
    input$category, {
    updateSelectInput(
      session,
      inputId = "class",
      choices = dt_cattle[
        market_type_category %in% "Auction" & category %in% input$category,
        .N,
        keyby = .(class)
      ]$class,
      selected = NULL
    )
  })
  # ## Note:
  # ## Update choices for `class` based on the selected value for `category`.

  # # 1.3. For `commodity`
  output$ui_commodity <- renderUI({
    selectInput(
      inputId = "commodity",
      label = "Commodity",
      choices = NULL
    )
  })
  observeEvent(
    c(
      input$category,
      input$class
    ), {
    updateSelectInput(
      session,
      inputId = "commodity",
      choices = dt_cattle[
        market_type_category %in% "Auction" & category %in% input$category &
          class %in% input$class,
        .N,
        keyby = .(commodity)
      ]$commodity,
      selected = NULL
    )
  })


  # # 2. For quality measures
  output$ui_quality.measures_frame <- renderUI({
    get_condition_uioutput_quality.measure(
      list_quality.measures_detail[["frame"]],
      "frame" %in% quality.measures()
    ) %>%
      parse(text = .) %>%
      eval(.)
  })
  output$ui_quality.measures_muscle.grade <- renderUI({
    get_condition_uioutput_quality.measure(
      list_quality.measures_detail[["muscle_grade"]],
      "muscle_grade" %in% quality.measures()
    ) %>%
      parse(text = .) %>%
      eval(.)
  })
  output$ui_quality.measures_quality.grade.name <- renderUI({
    get_condition_uioutput_quality.measure(
      list_quality.measures_detail[["quality_grade_name"]],
      "quality_grade_name" %in% quality.measures()
    ) %>%
      parse(text = .) %>%
      eval(.)
  })
  output$ui_quality.measures_yield.grade <- renderUI({
    get_condition_uioutput_quality.measure(
      list_quality.measures_detail[["yield_grade"]],
      "yield_grade" %in% quality.measures()
    ) %>%
      parse(text = .) %>%
      eval(.)
  })
  output$ui_quality.measures_dressing <- renderUI({
    get_condition_uioutput_quality.measure(
      list_quality.measures_detail[["dressing"]],
      "dressing" %in% quality.measures()
    ) %>%
      parse(text = .) %>%
      eval(.)
  })
  output$ui_quality.measures_pregnancy.stage <- renderUI({
    get_condition_uioutput_quality.measure(
      list_quality.measures_detail[["pregnancy_stage"]],
      "pregnancy_stage" %in% quality.measures()
    ) %>%
      parse(text = .) %>%
      eval(.)
  })
  output$ui_quality.measures_offspring.weight.est <- renderUI({
    get_condition_uioutput_quality.measure(
      list_quality.measures_detail[["offspring_weight_est"]],
      "offspring_weight_est" %in% quality.measures()
    ) %>%
      parse(text = .) %>%
      eval(.)
  })
  output$ui_load.data <- renderUI({
    actionButton(
      inputId = "load.data",
      label = "Load Data",
      icon = icon(name = "database", lib = "font-awesome")
    )
  })


  # # 3. For Plotting Options
  # # 3.1. For interval length for weight brackets
  output$ui_interval.length <- renderUI({
    selectInput(
      inputId = "interval.length",
      label = "Interval Length for Weight Brackets",
      choices = seq(50, 200, by = 50),
      selected = 100
    )
  })

  # # 3.2. For figure type
  observeEvent(
    c(
      input$load.data,
      input$interval.length
    ), {
    output$ui_figure.type <- renderUI({
      radioButtons(
        inputId = "figure.type",
        label = "Select a Figure Type",
        choices = c("By Weight Bracket", "By Market Location"),
        selected = character(0),
        inline = TRUE
      )
    })
  },
    ignoreNULL = FALSE
  )

  # # 3.3. For data level
  observeEvent(
    c(
      input$load.data,
      input$figure.type
    ), {
    output$ui_data.level <- renderUI({
      if (is.null(input$figure.type)) {
        radioButtons(
          inputId = "data.level",
          label = "Select a Data Level",
          choices = c("Aggregated Data", "Detailed Data"),
          selected = character(0),
          inline = TRUE
        )
      } else if (input$figure.type %in% "By Market Location") {
        radioButtons(
          inputId = "data.level",
          label = "Select a Data Level",
          choices = c("Aggregated Data", "Detailed Data"),
          selected = character(0),
          inline = TRUE
        )
      } else {
        radioButtons(
          inputId = "data.level",
          label = "Data Level",
          choices = "Detailed Data",
          selected = "Detailed Data",
          inline = TRUE
        )
      }
    })
  },
    ignoreNULL = FALSE
  )

  # # 3.4. For market locations
  # # 3.4.1. For state
  observeEvent(
    c(
      input$load.data,
      input$figure.type
    ), {
    output$ui_market.location_state <- renderUI({
      if (is.null(input$figure.type)) {
        selectInput(
          inputId = "market.location_state",
          label = "Select a State",
          choices = NULL,
          multiple = FALSE
        )
      } else {
        selectInput(
          inputId = "market.location_state",
          label = "Select a State",
          choices = choices_market.location_state(),
          multiple = FALSE
        )
      }
    })
  },
    ignoreNULL = FALSE
  )
  # # 3.4.2. For city
  observeEvent(
    c(
      input$load.data,
      input$figure.type
    ), {
    output$ui_market.location_city.with.state <- renderUI({
      if (is.null(input$figure.type)) {
        selectizeInput(
          inputId = "market.location_city.with.state",
          label = "Select a City",
          choices = NULL,
          multiple = FALSE
        )
      } else {
        selectizeInput(
          inputId = "market.location_city.with.state",
          label = "Select a City",
          choices = choices_market.location_city.within.state.selected(),
          multiple = FALSE
        )
      }
    })
  },
    ignoreNULL = FALSE
  )
  # # 3.4.3. For market locations selected
  output$ui_add.market.location <- renderUI({
    actionButton(
      inputId = "add.market.location",
      label = "Add the Market Location",
      icon = icon(name = "upload", lib = "font-awesome")
    )
  })
  observeEvent(
    c(
      input$load.data,
      input$add.market.location,
      values$data.level
    ), {
    output$ui_market.location_selected <- renderUI({
      if (is.null(input$figure.type)) {
        selectizeInput(
          inputId = "market.location_selected",
          label = "Market Location(s) selected",
          choices = NULL
        )
      } else if (input$figure.type %in% "By Weight Bracket") {
        selectizeInput(
          inputId = "market.location_selected",
          label = "Market Location(s) selected",
          choices = values$market.location_selected,
          selected = values$market.location_selected,
          multiple = FALSE
        )
      } else {
        selectizeInput(
          inputId = "market.location_selected",
          label = "Market Location(s) selected",
          choices = values$market.location_selected,
          selected = values$market.location_selected,
          multiple = TRUE,
          options = list(maxItems = 11)
        )
      }
    })
  },
    ignoreInit = TRUE
  )
  # # 3.4.4. For weight brackets
  observeEvent(
  c(
    input$load.data,
    input$figure.type,
    input$data.level,
    values$market.location_selected
  ), {
    output$ui_weight.bracket <- renderUI({
      if (is.null(input$figure.type) | is.null(input$data.level)) {
        selectizeInput(
          inputId = "weight.bracket",
          label = "Weight Bracket(s)",
          choices = NULL
        )
      } else {
        if (input$figure.type %in% "By Weight Bracket") {
          selectizeInput(
            inputId = "weight.bracket",
            label = "Weight Bracket(s)",
            choices = choices_weight.bracket(),
            multiple = TRUE,
            options = list(maxItems = 11)
          )
        } else if (input$data.level %in% "Aggregated Data") {
          selectizeInput(
            inputId = "weight.bracket",
            label = "Weight Bracket(s)",
            choices = "NA"
          )
        } else {
          selectizeInput(
            inputId = "weight.bracket",
            label = "Weight Bracket(s)",
            choices = choices_weight.bracket(),
            multiple = FALSE
          )
        }
      }
    })
  })
  # # 3.4.5. For time period
  observeEvent(
  choices_date(), {
    output$ui_time.period <- renderUI({
      if (length(choices_date()) > 0) {
        min_ <- choices_date() %>% lubridate::year(.) %>% min(.)
        max_ <- choices_date() %>% lubridate::year(.) %>% max(.)
        sliderInput(
          inputId = "time.period",
          label = "Time Period",
          min = min_,
          max = max_,
          value = c(min_, max_),
          step = 1,
          sep = ""
        )
      } else {
        NULL
      }
    })
  })
  # # 3.4.6. For quantities
  observeEvent(
    c(
      input$figure.type,
      input$data.level
    ), {
    output$ui_qty <- renderUI({
      if (length(choices_date()) > 0) {
        radioButtons(
          inputId = "qty",
          label = "Quantity Data",
          choices = c("Headcounts", "Total Weights", "Average Weights"),
          selected = character(0)
        )
      } else {
        NULL
      }
    })
  })
  # # 3.4.7. For prices
  observeEvent(
    c(
      input$figure.type,
      input$data.level
    ), {
    output$ui_price <- renderUI({
      if (length(choices_date()) > 0) {
        if (
          input$figure.type %in% "By Market Location" &
            input$data.level %in% "Aggregated Data"
        ) {
          radioButtons(
            inputId = "price",
            label = "Price Data",
            choices = c("Simple Average", "Weighted Average"),
            selected = character(0),
            inline = TRUE
          )
        } else {
          radioButtons(
            inputId = "price",
            label = "Price Data",
            choices = "Simple Average",
            inline = TRUE
          )
        }
      } else {
        NULL
      }
    })
  })

  # output$ui_test <- renderUI({
  #   selectInput(
  #     inputId = "test",
  #     label = "Test",
  #     choices = choices_date()
  #   )
  # })


  # ------- Create object(s) for output -------
  # 1. For Table(s)
  output$table <- renderDataTable({
    dt_for.plot()
  })


  # 2. For Figure(s)
  # # 2.1. A figure for prices
  output$figure_price <- renderPlotly({
    if (!is.null(plot_price()) & !is.null(plot_qty())) {
      plot_price()
    } else {
      NULL
    }
  })

  # # 2.2. A figure for quantities
  output$figure_qty <- renderPlotly({
    if (!is.null(plot_price()) & !is.null(plot_qty())) {
      plot_qty()
    } else {
      NULL
    }
  })

}  # End of server


# ------- Build a Shiny app object, and then execute it -------
app <- shinyApp(ui = ui, server = server)
runApp(app, launch.browser = TRUE)
