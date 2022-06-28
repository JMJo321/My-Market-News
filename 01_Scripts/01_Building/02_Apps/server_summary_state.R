




product <- eventReactive(
  c(
    input$summary.report_state_location,
    input$summary.report_state_category,
    input$summary.report_state_class,
    input$summary.report_state_commodity
  ), {
  paste(
    input$summary.report_state_commodity,
    input$summary.report_state_class,
    input$summary.report_state_category,
    sep = ", "
  )
})

dt_summary.report_state_for.plot <- eventReactive(
  c(
    input$summary.report_state_location,
    input$summary.report_state_product.selected,
    # summary.report_state_dates(),
    input$summary.report_state_time.period
  ), {
  if (
    !is.null(input$summary.report_state_location) &
      !is.null(input$summary.report_state_product.selected) &
      !is.null(input$summary.report_state_time.period)
      # !is.null(summary.report_state_dates())
  ) {
    dt_summary.report_extended[
      (  # Location
        summary.report_location %in% input$summary.report_state_location
      ) &
        (  # Product
          product %in% input$summary.report_state_product.selected
        ) &
        (  # Time range
          input$summary.report_state_time.period[1] <= report_date &
          report_date <= input$summary.report_state_time.period[2]
        )
    ][
      ,
      weighted.avg.price_real :=
        mapply(get_real.price, report_yearmonth, weighted.avg.price) %>%
          as.vector(., mode = "numeric")
    ]
  } else {
    NULL
  }
})

plot_summary.report_state_price <- eventReactive(
  c(
    input$summary.report_state_location,
    input$summary.report_state_product.selected,
    input$summary.report_state_time.period,
    dt_summary.report_state_for.plot(),
    input$summary.report_state_price.adjustment
  ), {
  if (input$summary.report_state_price.adjustment == FALSE) {
    summary.report_state_price.var <- "weighted.avg.price"
  } else {
    summary.report_state_price.var <- "weighted.avg.price_real"
  }

  if (length(input$summary.report_state_product.selected) > 4) {
    color.option <- 'scale_color_brewer(palette = "Spectral")'
  } else {
    color.option <- 'scale_color_manual(values = COLOR.PAL_SIGNAL)'
  }

  if (!is.null(dt_summary.report_state_for.plot())) {
    plot_ <-
      ggplot(data = dt_summary.report_state_for.plot()[!is.na(get(summary.report_state_price.var))]) +
        geom_point(
          aes_string(x = "report_date", y = summary.report_state_price.var),
          color = "black", alpha = 0.25, size = 1.3
        ) +
        geom_point(
          aes_string(
            x = "report_date",
            y = summary.report_state_price.var,
            color = "product"
          ),
          size = 0.8
        ) +
        geom_line(
          aes_string(
            x = "report_date",
            y = summary.report_state_price.var,
            group = "product"
          ),
          color = "black", alpha = 0.25, lwd = 0.7
        ) +
        geom_line(
          aes_string(
            x = "report_date",
            y = summary.report_state_price.var,
            color = "product"
          ),
          lwd = 0.3
        ) +
        scale_x_date(date_labels = "%b. %Y") +
        eval(parse(text = color.option)) +
        labs(
          x = "",
          y = "Per Cwt",
          color = "Product(s)"
        ) +
        theme_minimal()
  } else {
    NULL
  }
})




plot_summary.report_state_qty <- eventReactive(
  c(
    input$summary.report_state_location,
    input$summary.report_state_product.selected,
    input$summary.report_state_time.period,
    dt_summary.report_state_for.plot(),
    input$summary.report_state_qty.data
  ), {
  if (!is.null(dt_summary.report_state_for.plot())) {
    if (input$summary.report_state_qty.data %in% "Headcounts") {
      summary.report_state_qty.var <- "qty_head.count"
      labs_y.axis <- "Headcounts"
    } else {
      summary.report_state_qty.var <- "qty_total.weight"
      labs_y.axis <- "Cwt"
    }

    if (length(input$summary.report_state_product.selected) > 4) {
      color.option <- 'scale_fill_brewer(palette = "Spectral")'
    } else {
      color.option <- 'scale_fill_manual(values = COLOR.PAL_SIGNAL)'
    }

    plot_ <-
      ggplot(data = dt_summary.report_state_for.plot()) +
        geom_area(
          aes_string(
            x = "report_date",
            y = summary.report_state_qty.var,
            fill = "product"
          ),
          alpha = 0.5
        ) +
        geom_line(
          aes_string(
            x = "report_date",
            y = summary.report_state_qty.var,
            group = "product"
          ),
          position = "stack",
          color = "black", alpha = 0.2, lwd = 0.5
        ) +
        scale_x_date(date_labels = "%b. %Y") +
        scale_y_continuous(labels = scales::comma) +
        eval(parse(text = color.option)) +
        labs(
          x = "",
          y = labs_y.axis,
          fill = "Product(s)"
        ) +
        theme_minimal()
  } else {
    NULL
  }
})


observeEvent(
  input$summary.report_state_location, {
  values$state_product.selected <- NULL
})
observeEvent(
  input$summary.report_state_add.product, {
  values$state_product.selected <- c(
    values$state_product.selected,
    product()
  )
})
observeEvent(
  input$summary.report_state_product.selected, {
  values$state_product.selected <-
      input$summary.report_state_product.selected
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE
)
# # ## Note:
# # ## Update `values$state_product.selected` when an item is deleted from
# # ## `values$state_product.selected`.


output$test_table <- renderDataTable({
  if (!is.null(dt_summary.report_extended)) {
    dt_summary.report_state_for.plot()
  } else {
    NULL
  }
})

output$figure_summary.report_state_price <- renderPlotly({
  if (!is.null(plot_summary.report_state_price())) {
    plot_summary.report_state_price()
  } else {
    NULL
  }
})

output$figure_summary.report_state_qty <- renderPlotly({
  if (!is.null(plot_summary.report_state_qty())) {
    plot_summary.report_state_qty()
  } else {
    NULL
  }
})

summary.report_state_dates <- eventReactive(
  c(
    input$summary.report_state_location,
    input$summary.report_state_product.selected
  ), {
  if (!is.null(dt_summary.report_extended)) {
    dt_summary.report_extended[
      summary.report_location %in% input$summary.report_state_location &
        product %in% input$summary.report_state_product.selected,
      .N,
      keyby = .(report_date)
    ]$report_date
  } else {
    NULL
  }
})





# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------
# -----------


output$ui_summary.report_state_location <- renderUI({
  selectInput(
    inputId = "summary.report_state_location",
    label = "State",
    choices = dt_summary.report_extended[
      , .N, keyby = .(summary.report_location)
    ]$summary.report_location,
    selected = dt_summary.report_extended[
      , .N, keyby = .(summary.report_location)
    ]$summary.report_location %>%
      .[1]
  )
})

observeEvent(
  input$summary.report_state_location, {
  output$ui_summary.report_state_category <- renderUI({
    selectInput(
      inputId = "summary.report_state_category",
      label = "Category",
      choices = dt_summary.report_extended[
        summary.report_location %in% input$summary.report_state_location,
        .N, keyby = .(category)
      ]$category,
      selected = dt_summary.report_extended[
        summary.report_location %in% input$summary.report_state_location,
        .N, keyby = .(category)
      ]$category %>%
        .[1]
    )
  })
})

output$ui_summary.report_state_class <- renderUI({
  selectInput(
    inputId = "summary.report_state_class",
    label = "Class",
    choices = NULL
  )
})
observeEvent(
  input$summary.report_state_category, {
  updateSelectInput(
    session,
    inputId = "summary.report_state_class",
    choices = dt_summary.report_extended[
      summary.report_location %in% input$summary.report_state_location &
        category %in% input$summary.report_state_category,
      .N,
      keyby = .(class)
    ]$class,
    selected = dt_summary.report_extended[
      summary.report_location %in% input$summary.report_state_location &
        category %in% input$summary.report_state_category,
      .N,
      keyby = .(class)
    ]$class %>%
      .[1]
  )
})
# ## Note:
# ## Update choices for `class` based on the selected value for `category`.

output$ui_summary.report_state_commodity <- renderUI({
  selectInput(
    inputId = "summary.report_state_commodity",
    label = "Commodity",
    choices = NULL
  )
})
observeEvent(
  c(
    input$summary.report_state_category,
    input$summary.report_state_class
  ), {
  updateSelectInput(
    session,
    inputId = "summary.report_state_commodity",
    choices = dt_summary.report_extended[
      summary.report_location %in% input$summary.report_state_location &
        category %in% input$summary.report_state_category &
        class %in% input$summary.report_state_class,
      .N,
      keyby = .(commodity)
    ]$commodity,
    selected = NULL
  )
})

output$ui_summary.report_state_add.product <- renderUI({
  actionButton(
    inputId = "summary.report_state_add.product",
    label = "Add a Product",
    icon = icon(name = "download", lib = "font-awesome")
  )
})
observeEvent(
  c(
    input$summary.report_state_location,
    input$summary.report_state_add.product
  ), {
  output$ui_summary.report_state_product.selected <- renderUI({
    selectizeInput(
      inputId = "summary.report_state_product.selected",
      label = "Product(s) selected",
      choices = values$state_product.selected,
      selected = values$state_product.selected,
      multiple = TRUE,
      options = list(maxItems = 11)
    )
  })
},
  ignoreInit = TRUE
)


observeEvent(
  c(
    input$summary.report_state_location,
    input$summary.report_state_product.selected,
    summary.report_state_dates()
  ), {
  output$ui_summary.report_state_time.period <- renderUI({
    if (length(summary.report_state_dates()) > 0) {
      min_ <- summary.report_state_dates() %>% min(.)
      max_ <- summary.report_state_dates() %>% max(.)
      sliderInput(
        inputId = "summary.report_state_time.period",
        label = "Time Period",
        min = min_,
        max = max_,
        value = c(min_, max_),
        timeFormat = "%b. %Y"
      )
    } else {
      NULL
    }
  })
})


output$ui_summary.report_state_price.adjustment <- renderUI({
  checkboxInput(
    inputId = "summary.report_state_price.adjustment",
    label = "Ajust Prices",
    value = FALSE
  )
})

output$ui_summary.report_state_qty.data <- renderUI({
  radioButtons(
    inputId = "summary.report_state_qty.data",
    label = "Quantity Data",
    choices = c("Headcounts", "Total Weights"),
    selected = "Headcounts",
    inline = TRUE
  )
})



# output$ui_state_test <- renderUI({
#   selectizeInput(
#     inputId = "test",
#     label = "Test",
#     choices = input$summary.report_state_price.adjustment,
#   )
# })
# output$ui_state_test_table <- renderDataTable({
#   if (!is.null(dt_summary.report)) {
#     dt_summary.report
#   } else {
#     NULL
#   }
# })
# ## Note:
# ## This is just for test purpose.

# -----------



output$summary.report_state_download_table <- downloadHandler(
  filename = function () {"USDA-My-Market-News_Summary-Report_State.csv"},
  content = function (file) {
    write.csv(dt_summary.report_state_for.plot(), file, row.names = FALSE)
  }
)