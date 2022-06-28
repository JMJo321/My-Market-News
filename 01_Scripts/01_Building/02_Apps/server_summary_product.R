# -------




# -------
observeEvent(
  c(
    input$summary.report_product_category,
    input$summary.report_product_class,
    input$summary.report_product_commodity
  ), {
  values$product_location.selected <- NULL
})
observeEvent(
  input$summary.report_product_add.location, {
  values$product_location.selected <- c(
    values$product_location.selected,
    input$summary.report_product_location
  )
})
observeEvent(
  input$summary.report_product_location.selected, {
  values$product_location.selected <-
      input$summary.report_product_location.selected
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE
)
# # ## Note:
# # ## Update `values$product_location.selected` when an item is deleted from
# # ## `values$product_location.selected`.



# -------
summary.report_product_product <- eventReactive(
  c(
    input$summary.report_product_category,
    input$summary.report_product_class,
    input$summary.report_product_commodity
  ), {
  paste(
    input$summary.report_product_commodity,
    input$summary.report_product_class,
    input$summary.report_product_category,
    sep = ", "
  )
})

summary.report_product_dates <- eventReactive(
  c(
    summary.report_product_product(),
    input$summary.report_product_location.selected
  ), {
  if (!is.null(dt_summary.report_extended)) {
    dt_summary.report_extended[
      product %in% summary.report_product_product() &
        summary.report_location %in%
          input$summary.report_product_location.selected,
      .N,
      keyby = .(report_date)
    ]$report_date
  } else {
    NULL
  }
})





dt_summary.report_product_for.plot <- eventReactive(
  c(
    summary.report_product_product(),
    input$summary.report_product_location.selected,
    input$summary.report_product_time.period
  ), {
  if (
    !is.null(input$summary.report_product_location.selected) &
      !is.null(summary.report_product_product()) &
      !is.null(input$summary.report_product_time.period)
  ) {
    dt_summary.report_extended[
      (  # Location
        summary.report_location %in% input$summary.report_product_location.selected
      ) &
        (  # Product
          product %in% summary.report_product_product()
        ) &
        (  # Time range
          input$summary.report_product_time.period[1] <= report_date &
          report_date <= input$summary.report_product_time.period[2]
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


cols_by_others <- c(
  "market_type_category",
  "summary.report_pub.period",
  "product", "category", "class", "commodity",
  "report_date", "report_yearmonth", "report_year",
  "price_unit", "weighted.avg.price_others",
  "weighted.avg.price_others_min", "weighted.avg.price_others_max",
  "qty_head.count_others", "qty_total.weight_others",
  "qty_total.weight_others_min", "qty_total.weight_others_max"
)
dt_summary.report_product_for.plot_only.for.others <- eventReactive(
  c(
    summary.report_product_product(),
    input$summary.report_product_location.selected,
    input$summary.report_product_time.period
  ), {
  if (
    !is.null(input$summary.report_product_location.selected) &
      !is.null(summary.report_product_product()) &
      !is.null(input$summary.report_product_time.period)
  ) {
    dt <- dt_summary.report_extended[
      (  # Location
        !summary.report_location %in% input$summary.report_product_location.selected
      ) &
        (  # Product
          product %in% summary.report_product_product()
        ) &
        (  # Time range
          input$summary.report_product_time.period[1] <= report_date &
          report_date <= input$summary.report_product_time.period[2]
        )
    ][
      ,
      dummy_weight.sum := sum(qty_total.weight, na.rm = TRUE),
      by = .(report_date)
    ][
      , dummy_weight := qty_total.weight / dummy_weight.sum
    ][
      ,
      `:=` (
        weighted.avg.price_others_min = weighted.mean(weighted.avg.price_min, dummy_weight, na.rm = TRUE),
        weighted.avg.price_others_max = weighted.mean(weighted.avg.price_max, dummy_weight, na.rm = TRUE),
        weighted.avg.price_others = weighted.mean(weighted.avg.price, dummy_weight, na.rm = TRUE)
      ),
      by = .(report_date)
    ][
      ,
      `:=` (
        qty_head.count_others = sum(qty_head.count, na.rm = TRUE),
        qty_total.weight_others = sum(qty_total.weight, na.rm = TRUE),
        qty_total.weight_others_min = sum(qty_total.weight_min, na.rm = TRUE),
        qty_total.weight_others_max = sum(qty_total.weight_max, na.rm = TRUE)
      ),
      by = .(report_date)
    ][
      , .SD, .SDcols = cols_by_others
    ][
      , .N, by = cols_by_others
    ][
      , N := NULL
    ][
      ,
      `:=` (
        weighted.avg.price_real_others =
        mapply(get_real.price, report_yearmonth, weighted.avg.price_others) %>%
          as.vector(., mode = "numeric"),
        summary.report_location = "Other States"
      )
    ]
    names(dt) <- str_replace(names(dt), "_others", "")
    dt
  } else {
    NULL
  }
})






plot_summary.report_product_price <- eventReactive(
  c(
    summary.report_product_product(),
    input$summary.report_product_location.selected,
    input$summary.report_product_time.period,
    dt_summary.report_product_for.plot(),
    input$summary.report_product_price.adjustment
  ), {
  if (input$summary.report_product_price.adjustment == FALSE) {
    summary.report_product_price.var <- "weighted.avg.price"
  } else {
    summary.report_product_price.var <- "weighted.avg.price_real"
  }

  if (length(input$summary.report_product_location.selected) + 1 > 4) {
    # ## Note:
    # ## `+ 1` is for "Other States".
    color.option <- 'scale_color_brewer(palette = "Spectral")'
  } else {
    color.option <- 'scale_color_manual(values = COLOR.PAL_SIGNAL)'
  }

  if (!is.null(dt_summary.report_product_for.plot())) {
    dt_for.plot <- rbind(
      dt_summary.report_product_for.plot(),
      dt_summary.report_product_for.plot_only.for.others(),
      fill = TRUE
    )

    plot_ <-
      ggplot(
        data = dt_for.plot[!is.na(get(summary.report_product_price.var))]
      ) +
        geom_line(
          aes_string(
            x = "report_date",
            y = summary.report_product_price.var,
            color = "summary.report_location",
            group = "summary.report_location"
          ),
          color = "black", alpha = 0.25, lwd = 0.7
        ) +
        geom_point(
          aes_string(x = "report_date", y = summary.report_product_price.var),
          color = "black", alpha = 0.25, size = 1.3
        ) +
        geom_line(
          aes_string(
            x = "report_date",
            y = summary.report_product_price.var,
            color = "summary.report_location"
          ),
          lwd = 0.3
        ) +
        geom_point(
          aes_string(
            x = "report_date",
            y = summary.report_product_price.var,
            color = "summary.report_location"
          ),
          size = 0.8
        ) +
        scale_x_date(date_labels = "%b. %Y") +
        eval(parse(text = color.option)) +
        labs(
          x = "",
          y = "Per Cwt",
          color = "State(s)"
        ) +
        theme_minimal()
  } else {
    NULL
  }
})


plot_summary.report_product_qty <- eventReactive(
  c(
    summary.report_product_product(),
    input$summary.report_product_location.selected,
    input$summary.report_product_time.period,
    dt_summary.report_product_for.plot(),
    input$summary.report_product_qty.data
  ), {
  if (!is.null(dt_summary.report_product_for.plot())) {
    if (input$summary.report_product_qty.data %in% "Headcounts") {
      summary.report_product_qty.var <- "qty_head.count"
      labs_y.axis <- "Headcounts"
    } else {
      summary.report_product_qty.var <- "qty_total.weight"
      labs_y.axis <- "Cwt"
    }

    if (length(input$summary.report_product_location.selected) + 1 > 4) {
      color.option <- 'scale_fill_brewer(palette = "Spectral")'
    } else {
      color.option <- 'scale_fill_manual(values = COLOR.PAL_SIGNAL)'
    }

    dt_for.plot <- rbind(
      dt_summary.report_product_for.plot(),
      dt_summary.report_product_for.plot_only.for.others(),
      fill = TRUE
    )

    plot_ <-
      ggplot(data = dt_for.plot) +
        geom_line(
          aes_string(
            x = "report_date",
            y = summary.report_product_qty.var,
            group = "summary.report_location"
          ),
          position = "stack",
          color = "black", alpha = 0.4, lwd = 0.5
        ) +
        geom_area(
          aes_string(
            x = "report_date",
            y = summary.report_product_qty.var,
            fill = "summary.report_location"
          ),
          alpha = 0.5
        ) +
        scale_x_date(date_labels = "%b. %Y") +
        scale_y_continuous(labels = scales::comma) +
        eval(parse(text = color.option)) +
        labs(
          x = "",
          y = labs_y.axis,
          fill = "State(s)"
        ) +
        theme_minimal()
  } else {
    NULL
  }
})





# -------
output$ui_summary.report_product_category <- renderUI({
    selectInput(
      inputId = "summary.report_product_category",
      label = "Category",
      choices = dt_summary.report_extended[
        , .N, keyby = .(category)
      ]$category,
      selected = dt_summary.report_extended[
        , .N, keyby = .(category)
      ]$category %>%
        .[1]
    )
  })

output$ui_summary.report_product_class <- renderUI({
  selectInput(
    inputId = "summary.report_product_class",
    label = "Class",
    choices = NULL
  )
})
observeEvent(
  input$summary.report_product_category, {
  updateSelectInput(
    session,
    inputId = "summary.report_product_class",
    choices = dt_summary.report_extended[
      category %in% input$summary.report_product_category,
      .N,
      keyby = .(class)
    ]$class,
    selected = dt_summary.report_extended[
      category %in% input$summary.report_product_category,
      .N,
      keyby = .(class)
    ]$class %>%
      .[1]
  )
})
# ## Note:
# ## Update choices for `class` based on the selected value for `category`.

output$ui_summary.report_product_commodity <- renderUI({
  selectInput(
    inputId = "summary.report_product_commodity",
    label = "Commodity",
    choices = NULL
  )
})
observeEvent(
  c(
    input$summary.report_product_category,
    input$summary.report_product_class
  ), {
  updateSelectInput(
    session,
    inputId = "summary.report_product_commodity",
    choices = dt_summary.report_extended[
      category %in% input$summary.report_product_category &
        class %in% input$summary.report_product_class,
      .N,
      keyby = .(commodity)
    ]$commodity,
    selected = NULL
  )
})


observeEvent(
  summary.report_product_product(), {
  output$ui_summary.report_product_location <- renderUI({
    selectInput(
      inputId = "summary.report_product_location",
      label = "State",
      choices = dt_summary.report_extended[
        product %in% summary.report_product_product(),
        .N,
        keyby = .(summary.report_location)
      ]$summary.report_location,
      selected = dt_summary.report_extended[
        product %in% summary.report_product_product(),
        .N,
        keyby = .(summary.report_location)
      ]$summary.report_location %>%
        .[1]
    )
  })
})


output$ui_summary.report_product_add.location <- renderUI({
  actionButton(
    inputId = "summary.report_product_add.location",
    label = "Add a Market Location",
    icon = icon(name = "download", lib = "font-awesome")
  )
})

observeEvent(
  c(
    input$summary.report_product_location,
    input$summary.report_product_add.location
  ), {
  output$ui_summary.report_product_location.selected <- renderUI({
    selectizeInput(
      inputId = "summary.report_product_location.selected",
      label = "Market Location(s) selected",
      choices = values$product_location.selected,
      selected = values$product_location.selected,
      multiple = TRUE,
      options = list(maxItems = 11)
    )
  })
},
  ignoreInit = TRUE
)



output$ui_summary.report_product_price.adjustment <- renderUI({
  checkboxInput(
    inputId = "summary.report_product_price.adjustment",
    label = "Ajust Prices",
    value = FALSE
  )
})



output$ui_summary.report_product_qty.data <- renderUI({
  radioButtons(
    inputId = "summary.report_product_qty.data",
    label = "Quantity Data",
    choices = c("Headcounts", "Total Weights"),
    selected = "Headcounts",
    inline = TRUE
  )
})



observeEvent(
  c(
    summary.report_product_product(),
    input$summary.report_product_location.selected,
    summary.report_product_dates()
  ), {
  output$ui_summary.report_product_time.period <- renderUI({
    if (length(summary.report_product_dates()) > 0) {
      min_ <- summary.report_product_dates() %>% min(.)
      max_ <- summary.report_product_dates() %>% max(.)
      sliderInput(
        inputId = "summary.report_product_time.period",
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




output$figure_summary.report_product_price <- renderPlotly({
  if (!is.null(plot_summary.report_product_price())) {
    plot_summary.report_product_price()
  } else {
    NULL
  }
})

output$figure_summary.report_product_qty <- renderPlotly({
  if (!is.null(plot_summary.report_product_qty())) {
    plot_summary.report_product_qty()
  } else {
    NULL
  }
})


#
# output$ui_product_test <- renderUI({
#   selectizeInput(
#     inputId = "test",
#     label = "Test",
#     # choices = input$summary.report_product_location.selected
#     choices = summary.report_product_dates()
#   )
# })  # This is just for test purpose.


output$summary.report_product_download_table <- downloadHandler(
  filename = function () {"USDA-My-Market-News_Summary-Report_Product.csv"},
  content = function (file) {
    write.csv(dt_summary.report_product_for.plot(), file, row.names = FALSE)
  }
)