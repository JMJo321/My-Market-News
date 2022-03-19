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

# # 1.2. Path for the script including functions that will be used below
# DIR_TO.LOAD_SCRIPT <- paste(PATH_SCRIPTS, "01_Building/02_Apps", sep = "/")
# FILE_TO.LOAD_SCRIPT <- "B-04-01A-1_Create-Sample-Figures_Functions-only.R"
# PATH_TO.LOAD_SCRIPT <- paste(DIR_TO.LOAD_SCRIPT, FILE_TO.LOAD_SCRIPT, sep = "/")

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
get_condition_uioutput_quality.measure <- function (quality.measure.info_list) {
  id_input <- quality.measure.info_list[["inputId"]]
  label <- quality.measure.info_list[["label"]]
  varname <- str_replace_all(id_input, "\\.", "_")
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
# source(PATH_TO.LOAD_SCRIPT)
# source(PATH_TO.LOAD_SCRIPT_REACTIVE)
# source(PATH_TO.LOAD_SCRIPT_OUTPUT)


# ------------------------------------------------------------------------------
# Build R Shiny App(s)
# ------------------------------------------------------------------------------
# ------- Build a UI definition -------
tabpanels <- tabPanel(
  title = "Auction",
  sidebarPanel(
    width = 6,
    tags$h3("Product"),
    selectInput(
      inputId = "category",
      label = "Category",
      choices = dt_cattle[, .N, keyby = .(category)]$category,
      selected = NULL
    ),
    selectInput(
      inputId = "class",
      label = "Class",
      choices = NULL
    ),
    selectInput(
      inputId = "commodity",
      label = "Commodity",
      choices = NULL
    ),
    tags$hr(),
    tags$h3("Quality Measures"),
    uiOutput("ui_quality.measures_frame"),
    uiOutput("ui_quality.measures_muscle.grade"),
    uiOutput("ui_quality.measures_quality.grade.name"),
    uiOutput("ui_quality.measures_yield.grade"),
    uiOutput("ui_quality.measures_dressing"),
    uiOutput("ui_quality.measures_pregnancy.stage"),
    uiOutput("ui_quality.measures_offspring.weight.est")
  ),
  sidebarPanel(
    width = 6,
    tags$h3("Options For Plotting"),
    uiOutput("ui_interval.length"),
    uiOutput("ui_figure.type"),
    uiOutput("ui_data.level"),
    uiOutput("ui_market.location_state"),
    uiOutput("ui_market.location_city"),
    uiOutput("ui_market.location_selected"),
    uiOutput("ui_weight.bracket"),
    uiOutput("ui_time.period"),
    uiOutput("ui_qty"),
    uiOutput("ui_price")
  ),
  mainPanel(
    plotlyOutput(outputId = "figure_price"),
    plotlyOutput(outputId = "figure_qty"),
    width = 12
  )
)



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


# ------- Build a server -------
server <- function (input, output, session) {
  # # 1. Create reactive object(s)
  # # 1.1. For UI
  # # 1.1.1. About quality measures
  quality.measures <- eventReactive(
  c(
    input$category,
    input$class,
    input$commodity
  ), {
    list_quality.measures[[input$category]][[input$class]][[input$commodity]]
  })

  # # 1.2. For choice values
  # # 1.2.1. About market locations
  choices_market.location <- eventReactive(
    dt_subset(), {
    dt_subset()[
      ,
      .N,
      keyby = .(market_location_state, market_location_city, market_location)
    ]$market_location
  })
  choices_market.location_state <- eventReactive(
    choices_market.location(), {
    dt_subset()[
      ,
      .N,
      keyby = .(market_location_state)
    ]$market_location_state
  })
  choices_market.location_city <- eventReactive(
    c(
      choices_market.location(),
      input$market.location_state
    ), {
    dt_subset()[
      market_location_state %in% input$market.location_state &
        !is.na(market_location_city),
      .N,
      keyby = .(market_location_city)
    ]$market_location_city
  })
  choices_market.location_city.within.state.selected <- eventReactive(
    c(
      dt_subset(),
      choices_market.location(),
      input$market.location_state
    ), {
    dt_subset()[
      market_location_state %in% input$market.location_state &
        !is.na(market_location_city),
      .N,
      keyby = .(market_location)
    ]$market_location
  })
  # # 1.2.2. About weight bracket
  choices_weight.bracket_for.plot.by.weight.bracket <- eventReactive(
    c(
      dt_base(),
      input$market.location_state,
      input$market.location_city.with.state
    ), {
    if (
      !is.null(input$market.location_state) &
        !is.null(input$market.location_city.with.state)
    ) {
      dt_base()[
        market_location %in% input$market.location_city.with.state,
        .N,
        keyby = .(brackets)
      ]$brackets
    } else {
      NULL
    }
  })
  choices_weight.bracket_for.plot.by.market.location <- eventReactive(
    c(
      dt_base(),
      values$market.location_selected
    ), {
    if (!is.null(values$market.location_selected)) {
      dt_base()[
        market_location %in% values$market.location_selected,
        .N,
        keyby = .(brackets)
      ]$brackets
    } else {
      NULL
    }
  })
  # # 1.2.3. About time period
  dates <- eventReactive(
    c(
      dt_base(),
      input$market.location_state,
      input$market.location_city.with.state,
      input$weight.bracket
    ), {
    dt_base()[
      market_location %in% input$market.location_city.with.state &
        brackets %in% input$weight.bracket,
      .N, keyby = .(report_date)
    ]$report_date
  })

  # # 1.2.4. About price and quantity
  var_price <- eventReactive(
    c(
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
  var_qty <- eventReactive(
    c(
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


  # # 2. Create reactive object(s) for output
  # # 2.1. Create a `reactiveValues` object including reactive object(s) that
  # #      will be used later
  values <- reactiveValues(
    interval.length = 100,
    market.location_selected = NULL
  )
  observeEvent(
    input$interval.length, {
    values$interval.length <- input$interval.length
  })
  observeEvent(
    c(
      input$figure.type,
      input$market.location_city.with.state
    ), {
    if (
      input$figure.type %in% "By Market Location" &
        !is.null(input$data.level)
    ) {
      values$market.location_selected <- c(
        values$market.location_selected,
        input$market.location_city.with.state
      )
    } else {
      values$market.location_selected <- NULL
    }
  })

  # # 2.2. Create a DT subsetted based on quality measures
  dt_subset <- eventReactive(
    c(
      input$frame,
      input$muscle.grade,
      input$quality.grade.name,
      input$yiled.grade,
      input$dressing,
      input$pregnancy.stage,
      input$offspring.weight.est
    ), {
    dt <- dt_cattle[
      get_condition_qaulity.measures(quality.measures()) %>%
        parse(text = .) %>%
        eval(.)
    ]
  })

  # # 2.3. Create a DT that containes prices and quantities
  dt_base <- eventReactive(
    c(
      dt_subset(),
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

  # # 2.4. Create a DT subsetted based on market locations
  dt_selected.market.location <- eventReactive(
    c(
      dt_base(),
      input$figure.type,
      input$market.location_state,
      input$market.location_city.with.state,
      values$market.location_selected
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

    if (
      !is.null(input$figure.type) &
        !is.null(input$market.location_city.with.state) &
        !is.null(input$market.location_state)
    ) {
      subset.condition <- if (input$figure.type %in% "By Weight Bracket") {
        "market_location %in% input$market.location_city.with.state"
      } else {
        "market_location %in% values$market.location_selected"
      }
      dt <- dt_base()[
        parse(text = subset.condition) %>% eval(.),
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
        !cols_by.weight.bracket %in% id.vars_by.weight.bracket
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
    } else {
      NULL
    }
  })

  # # 2.5. Create a DT that will be used to create DT(s) for figure(s)
  dt_for.plot <- eventReactive(
    c(
      dt_selected.market.location(),
      input$time.period,
      input$price,
      input$qty,
      input$weight.bracket
    ), {
    if (
      !is.null(dt_selected.market.location()) &
        !is.null(input$time.period) &
        !is.null(var_price()) &
        !is.null(var_qty()) &
        !is.null(input$weight.bracket)
    ) {
      n_obs <- nrow(dt_selected.market.location())
    } else {
      n_obs <- 0
    }
    if (n_obs > 0) {
      dt_subset <- dt_selected.market.location()[
        input$time.period[1] <= report_year &
          report_year <= input$time.period[2] &
          (variable %in% var_price() | variable %in% var_qty()) &
          brackets %in% input$weight.bracket
      ]
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

  # # 2.6. Create reative object(s) for figure(s)
  # # 2.6.1. Price-related reactive object
  plot_price <- eventReactive(
    dt_for.plot(), {
    if (!is.null(dt_for.plot())) {
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
  # # 2.6.2. Quantity-related reactive object
  plot_qty <- eventReactive(
    dt_for.plot(), {
    if (!is.null(dt_for.plot())) {
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


  # ------- Create input control(s) -------
  # # 1. For a product
  observeEvent(
    input$category, {
    updateSelectInput(
      session,
      "class",
      choices = dt_cattle[
        market_type_category == "Auction" &
          category == input$category,
        .N,
        keyby = .(class)
      ]$class,
      selected = NULL
    )
  })

  observeEvent(
    input$class, {
    updateSelectInput(
      session,
      "commodity",
      choices = dt_cattle[
        market_type_category == "Auction" &
          category == input$category &
          class == input$class,
        .N,
        keyby = .(commodity)
      ]$commodity
    )
  })


  # # 2. For quality measures
  output$ui_quality.measures_frame <- renderUI({
    if ("frame" %in% quality.measures()) {
      get_condition_uioutput_quality.measure(
        list_quality.measures_detail[["frame"]]
      ) %>%
        parse(text = .) %>%
        eval(.)
    } else {
      NULL
    }
  })

  output$ui_quality.measures_muscle.grade <- renderUI({
    if ("muscle_grade" %in% quality.measures()) {
      get_condition_uioutput_quality.measure(
        list_quality.measures_detail[["muscle_grade"]]
      ) %>%
        parse(text = .) %>%
        eval(.)
    } else {
      NULL
    }
  })

  output$ui_quality.measures_quality.grade.name <- renderUI({
    if ("quality_grade_name" %in% quality.measures()) {
      get_condition_uioutput_quality.measure(
        list_quality.measures_detail[["quality_grade_name"]]
      ) %>%
        parse(text = .) %>%
        eval(.)
    } else {
      NULL
    }
  })

  output$ui_quality.measures_yield.grade <- renderUI({
    if ("yield_grade" %in% quality.measures()) {
      get_condition_uioutput_quality.measure(
        list_quality.measures_detail[["yield_grade"]]
      ) %>%
        parse(text = .) %>%
        eval(.)
    } else {
      NULL
    }
  })

  output$ui_quality.measures_dressing <- renderUI({
    if ("dressing" %in% quality.measures()) {
      get_condition_uioutput_quality.measure(
        list_quality.measures_detail[["dressing"]]
      ) %>%
        parse(text = .) %>%
        eval(.)
    } else {
      NULL
    }
  })

  output$ui_quality.measures_pregnancy.stage <- renderUI({
    if ("pregnancy_stage" %in% quality.measures()) {
      get_condition_uioutput_quality.measure(
        list_quality.measures_detail[["pregnancy_stage"]]
      ) %>%
        parse(text = .) %>%
        eval(.)
    } else {
      NULL
    }
  })

  output$ui_quality.measures_offspring.weight.est <- renderUI({
    if ("offspring.weight.est" %in% quality.measures()) {
      get_condition_uioutput_quality.measure(
        list_quality.measures_detail[["offspring.weight.est"]]
      ) %>%
        parse(text = .) %>%
        eval(.)
    } else {
      NULL
    }
  })


  # # 3. For selections
  output$ui_interval.length <- renderUI({
    if (dt_subset()[, .N] > 0) {
      selectInput(
        inputId = "interval.length",
        label = "Interval Length for Weigh Brackets",
        choices = seq(50, 200, by = 50),
        selected = 100
      )
    } else {
      NULL
    }
  })

  output$ui_figure.type <- renderUI({
    if (length(choices_market.location()) > 0) {
      radioButtons(
        inputId = "figure.type",
        label = "Select a Figure Type",
        choices = c("By Weight Bracket", "By Market Location"),
        selected = character(0),
        inline = TRUE
      )
    } else {
      NULL
    }
  })

  output$ui_data.level <- renderUI({
    if (length(choices_market.location()) > 0 & !is.null(input$figure.type)) {
      if (input$figure.type %in% "By Market Location") {
        radioButtons(
          inputId = "data.level",
          label = "Select a Data Level",
          choices = c("Aggregated Data", "Detailed Data"),
          selected = character(0)
        )
      } else {
        radioButtons(
          inputId = "data.level",
          label = "Data Level",
          choices = "Detailed Data",
          selected = NULL
        )
      }
    } else {
      NULL
    }
  })

  observeEvent(
    c(
      input$figure.type,
      input$data.level
    ), {
    output$ui_qty <- renderUI({
      if (
        !is.null(input$market.location_city.with.state) &
          !is.null(input$weight.bracket)
      ) {
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

  output$ui_price <- renderUI({
    if (
      !is.null(input$figure.type) &
        !is.null(input$market.location_state) &
        !is.null(input$weight.bracket)
    ) {
      if (input$figure.type %in% "By Market Location") {
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
          # selected = character(0),
          inline = TRUE
        )
      }
    } else {
      NULL
    }
  })

  observeEvent(
    choices_market.location_state(), {
    output$ui_market.location_state <- renderUI({
      if (!is.null(input$figure.type)) {
        selectizeInput(
          inputId = "market.location_state",
          label = "Select a State",
          choices = choices_market.location_state(),
          multiple = FALSE
        )
      } else {
        NULL
      }
    })
  })
  observeEvent(
    c(
      input$figure.type,
      choices_market.location_city(),
      choices_market.location_city.within.state.selected()
    ), {
    if (!is.null(input$figure.type)) {
      output$ui_market.location_city <- renderUI({
        if (input$figure.type %in% "By Weight Bracket") {
          selectizeInput(
            inputId = "market.location_city.with.state",
            label = "Select a City",
            choices = choices_market.location_city.within.state.selected(),
            multiple = FALSE
          )
        } else {
          selectizeInput(
            inputId = "market.location_city.with.state",
            label = "Select a City",
            choices = choices_market.location_city.within.state.selected(),
            multiple = TRUE,
            size = 11
          )
        }
      })
    } else {
      NULL
    }
  })
  observeEvent(
    values$market.location_selected,{
    output$ui_market.location_selected <- renderUI({
      if (!is.null(values$market.location_selected)) {
        selectizeInput(
          inputId = "market.location_selected",
          label = "Market Location(s) selected",
          choices = values$market.location_selected,
          selected = values$market.location_selected,
          multiple = TRUE,
          size = 11
        )
      } else {
        NULL
      }
    })
  })

  observeEvent(
    c(
      # input$figure.type,
      choices_weight.bracket_for.plot.by.weight.bracket(),
      choices_weight.bracket_for.plot.by.market.location()
    ), {
    output$ui_weight.bracket <- renderUI({
      if (!is.null(input$figure.type)) {
        if (input$figure.type %in% "By Weight Bracket") {
          selectizeInput(
            inputId = "weight.bracket",
            label = "Weight Bracket(s)",
            choices = choices_weight.bracket_for.plot.by.weight.bracket(),
            multiple = TRUE,
            size = 11
          )
        } else {
          selectizeInput(
            inputId = "weight.bracket",
            label = "Weight Bracket(s)",
            choices = choices_weight.bracket_for.plot.by.market.location(),
            multiple = FALSE,
            size = NULL
          )
        }
      } else {
        NULL
      }
    })
  })

  output$ui_time.period <- renderUI({
    if (length(dates()) > 0) {
      min_ <- dates() %>% lubridate::year(.) %>% min(.)
      max_ <- dates() %>% lubridate::year(.) %>% max(.)
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

  # observeEvent(
  #   input$figure.type, {
  #   output$ui_test <- renderUI({
  #     if (!is.null(input$figure.type)) {
  #       selectInput(
  #         inputId = "test",
  #         label = "Test",
  #         choices = values$market.location_selected
  #       )
  #     } else {
  #       NULL
  #     }
  #   })
  # })


  # ------- Create object(s) for output -------
  # # 1. For Table(s)
  output$table <- renderDataTable({
    if (!is.null(input$weight.bracket)) {
      dt_for.plot()
      # dt_base()
    } else {
      NULL
    }
  })


  # # 2. For Figure(s)
  # # 2.1. A figure for prices
  output$figure_price <- renderPlotly({
    if (!is.null(plot_price())) {
      plot_price()
    } else {
      NULL
    }
  })

  # # 2.2. A figure for quantities
  output$figure_qty <- renderPlotly({
    if (!is.null(plot_qty())) {
      plot_qty()
    } else {
      NULL
    }
  })

}


# ------- Build a Shiny app object, and then execute it -------
app <- shinyApp(ui = ui, server = server)
runApp(app, launch.browser = TRUE)
