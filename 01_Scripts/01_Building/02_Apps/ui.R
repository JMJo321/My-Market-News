
tabpanels_summary.report_state <- tabPanel(
  title = "Summary Data: By State",
  sidebarPanel(
    width = 6,
    tags$h3("Market Location"),
    tags$h5("Select a State-level Market Location."),
    uiOutput("ui_summary.report_state_location"),
    tags$hr(),
    tags$h3("Product"),
    tags$h5("Select Product(s)."),
    uiOutput("ui_summary.report_state_category"),
    uiOutput("ui_summary.report_state_class"),
    uiOutput("ui_summary.report_state_commodity"),
    uiOutput("ui_summary.report_state_add.product"),
    tags$hr(),
    uiOutput("ui_summary.report_state_product.selected")
  ),
  sidebarPanel(
    width = 6,
    tags$h3("Plot Options"),
    tags$h5("Change option(s) according to your taste."),
    tags$h5(
      "Price Adjustment (for Inflation)",
      style = "text-align:left;color:black;font-weight:bold"
    ),
    uiOutput("ui_summary.report_state_price.adjustment"),
    uiOutput("ui_summary.report_state_qty.data"),
    uiOutput("ui_summary.report_state_time.period")
  ),
  mainPanel(
    width = 12,
    tags$hr(),
    plotlyOutput(outputId = "figure_summary.report_state_price"),
    plotlyOutput(outputId = "figure_summary.report_state_qty"),
    # dataTableOutput(outputId = "ui_state_test_table"),  # Just for check
    tags$hr(),
    downloadButton(
      outputId = "summary.report_state_download_table",
      label = "Daownload Data"
    ),
    tags$hr()
  )
)
tabpanels_summary.report_product <- tabPanel(
  title = "Summary Data: By Product",
  sidebarPanel(
    width = 6,
    tags$h3("Product"),
    tags$h5("Select a Product."),
    uiOutput("ui_summary.report_product_category"),
    uiOutput("ui_summary.report_product_class"),
    uiOutput("ui_summary.report_product_commodity"),
    tags$hr(),
    tags$h3("Market Location"),
    tags$h5("Select Market Location(s)."),
    uiOutput("ui_summary.report_product_location"),
    uiOutput("ui_summary.report_product_add.location"),
    tags$hr(),
    uiOutput("ui_summary.report_product_location.selected"),
    uiOutput("ui_product_test")
  ),
  sidebarPanel(
    width = 6,
    tags$h3("Plot Options"),
    tags$h5("Change option(s) according to your taste."),
    tags$h5(
      "Price Adjustment (for Inflation)",
      style = "text-align:left;color:black;font-weight:bold"
    ),
    uiOutput("ui_summary.report_product_price.adjustment"),
    uiOutput("ui_summary.report_product_qty.data"),
    uiOutput("ui_summary.report_product_time.period")
  ),
  mainPanel(
    width = 12,
    tags$hr(),
    plotlyOutput(outputId = "figure_summary.report_product_price"),
    plotlyOutput(outputId = "figure_summary.report_product_qty"),
    # dataTableOutput(outputId = "test_table"),  # Just for check
    tags$hr(),
    downloadButton(
      outputId = "summary.report_product_download_table",
      label = "Daownload Data"
    ),
    tags$hr()
  )
)
tabpanels_auction.report_single <- tabPanel(
  title = "Auction Data: For a Single Market",
  sidebarPanel(
    width = 6,
    tags$h3("Product"),
    tags$h5("Select Category, Class, and Commodity."),
    uiOutput("ui_auction.report_single_category"),
    uiOutput("ui_auction.report_single_class"),
    uiOutput("ui_auction.report_single_commodity"),
    tags$hr(),
    tags$h3("Quality Measures"),
    tags$h5(
      "Quality Measure(s) with NA is(are) irrelevant to the Product Selected."
    ),
    uiOutput("ui_auction.report_single_quality.measures_frame"),
    uiOutput("ui_auction.report_single_quality.measures_muscle.grade"),
    uiOutput("ui_auction.report_single_quality.measures_quality.grade.name"),
    uiOutput("ui_auction.report_single_quality.measures_yield.grade"),
    uiOutput("ui_auction.report_single_quality.measures_dressing"),
    uiOutput("ui_auction.report_single_quality.measures_pregnancy.stage"),
    uiOutput("ui_auction.report_single_quality.measures_offspring.weight.est")
  ),
  sidebarPanel(
    width = 6,
    tags$h3("Market Location"),
    tags$h5("Select a Market Location."),
    uiOutput("ui_auction.report_single_market.location_state"),
    uiOutput("ui_auction.report_single_market.location_city.with.state"),
    uiOutput("ui_auction.report_single_make.plots"),
    uiOutput("ui_auction.report_single_test"),
    tags$hr(),
    tags$h3("Options For Plotting"),
    uiOutput("ui_auction.report_single_price.adjustment"),
    uiOutput("ui_auction.report_single_interval.length"),
    uiOutput("ui_auction.report_single_qty"),
    uiOutput("ui_auction.report_single_time.period")
  ),
  mainPanel(
    width = 12,
    plotlyOutput(outputId = "auction.report_single_figure_price"),
    plotlyOutput(outputId = "auction.report_single_figure_qty"),
    dataTableOutput(outputId = "auction.report_single_table"),
    dataTableOutput(outputId = "ui_auction.report_single_test_table"),
    tags$hr(),
    downloadButton(
      outputId = "auction.report_single_download_table",
      label = "Daownload Data"
    ),
    tags$hr()
  )
)
tabpanels_auction.report_multiple <- tabPanel(
  title = "Auction Data: For Multiple Markets",
  sidebarPanel(
    width = 6,
    tags$h3("Product"),
    tags$h5("Select Category, Class, and Commodity."),
    uiOutput("ui_auction.report_multiple_category"),
    uiOutput("ui_auction.report_multiple_class"),
    uiOutput("ui_auction.report_multiple_commodity"),
    tags$hr(),
    tags$h3("Quality Measures"),
    tags$h5(
      "Quality Measure(s) with NA is(are) irrelevant to the Product Selected."
    ),
    uiOutput("ui_auction.report_multiple_quality.measures_frame"),
    uiOutput("ui_auction.report_multiple_quality.measures_muscle.grade"),
    uiOutput("ui_auction.report_multiple_quality.measures_quality.grade.name"),
    uiOutput("ui_auction.report_multiple_quality.measures_yield.grade"),
    uiOutput("ui_auction.report_multiple_quality.measures_dressing"),
    uiOutput("ui_auction.report_multiple_quality.measures_pregnancy.stage"),
    uiOutput("ui_auction.report_multiple_quality.measures_offspring.weight.est")
  ),
  sidebarPanel(
    width = 6,
    tags$h3("Market Locations"),
    tags$h5("Select Market Locations."),
    uiOutput("ui_auction.report_multiple_market.location_state"),
    uiOutput("ui_auction.report_multiple_market.location_city.with.state"),
    uiOutput("ui_auction.report_multiple_add.location"),
    tags$hr(),
    uiOutput("ui_auction.report_multiple_location.selected"),
    # tags$hr(),
    uiOutput("ui_auction.report_multiple_make.plots"),
    uiOutput("ui_auction.report_multiple_test"),
    tags$hr(),
    tags$h3("Options For Plotting"),
    uiOutput("ui_auction.report_multiple_price.adjustment"),
    uiOutput("ui_auction.report_multiple_qty"),
    uiOutput("ui_auction.report_multiple_time.period")
  ),
  mainPanel(
    width = 12,
    plotlyOutput(outputId = "auction.report_multiple_figure_price"),
    plotlyOutput(outputId = "auction.report_multiple_figure_qty"),
    dataTableOutput(outputId = "ui_auction.report_multiple_test_table")
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
        tabsetPanel(
          tabpanels_summary.report_state,
          tabpanels_summary.report_product,
          tabpanels_auction.report_single,
          tabpanels_auction.report_multiple
        )
      ),
      title = NULL,
      skin = "black"
    )
  )
)

