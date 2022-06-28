# -------
auction.report_multiple_quality.measures <- eventReactive(
  c(
    input$auction.report_multiple_category,
    input$auction.report_multiple_class,
    input$auction.report_multiple_commodity
  ), {
  parse(
    text = paste0(
      "list_quality.measures[[input$auction.report_multiple_category]]",
      "[[input$auction.report_multiple_class]]",
      "[[input$auction.report_multiple_commodity]]"
    )
  ) %>%
    eval(.)
})


dt_auction.report_multiple_quality.measures <- eventReactive(
  c(
    input$auction.report_multiple_category,
    input$auction.report_multiple_class,
    input$auction.report_multiple_commodity
  ), {
  if (!is.null(auction.report_multiple_quality.measures())) {
    dt <- dt_cattle[
      market_type_category %in% "Auction" &
        category %in% input$auction.report_multiple_category &
        class %in% input$auction.report_multiple_class &
        commodity %in% input$auction.report_multiple_commodity &
        !is.na(avg_weight) &
        price_unit == "Per Cwt",
      .N,
      keyby = eval(auction.report_multiple_quality.measures())
    ][
      , N := NULL
    ]
    dt
  }
})


auction.report_multiple_condition.for.state <- eventReactive(
  auction.report_multiple_quality.measures(), {
  if (!is.null(auction.report_multiple_quality.measures())) {
    paste(
      'market_type_category %in% "Auction"',
      paste(
        paste0(
          c("category", "class", "commodity"),
          " %in% input$auction.report_multiple_",
          c("category", "class", "commodity"),
          collapse = " & "
        ),
        paste0(
          auction.report_multiple_quality.measures(),
          " %in% input$auction.report_multiple_quality.measures_",
          str_replace_all(
            auction.report_multiple_quality.measures(), "_", "."
          ),
          collapse = " & "
        ),
        sep = " & "
      ),
      "!is.na(avg_weight)",
      'price_unit == "Per Cwt"',
      sep = " & "
    )
  } else {
    NULL
  }
})


dt_auction.report_multiple_subset <- eventReactive(
  c(
    auction.report_multiple_condition.for.state(),
    input$auction.report_multiple_market.location_city.with.state,
    input$auction.report_multiple_make.plots
  ), if (
    !is.null(auction.report_multiple_condition.for.state()) &
      !is.null(input$auction.report_multiple_market.location_city.with.state)
  ) {
    dt_cattle[
      parse(text = auction.report_multiple_condition.for.state()) %>%
        eval(.)
    ][
      market_location %in%
        input$auction.report_multiple_location.selected
    ]
  } else {
    NULL
})


dt_auction.report_multiple_for.plot <- eventReactive(
  c(
    input$auction.report_multiple_make.plots
  ), {
  if (
    !is.null(dt_auction.report_multiple_subset())
  ) {
    dt <- dt_auction.report_multiple_subset()[
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
        .(report_year, report_yearmonth, report_date, market_location, price_unit)
    ]
    # dt[
    #   ,
    #   qty_weight_avg := round(qty_weight_total / qty_headcount, digits = 0),
    #   by =
    #     .(report_year, report_yearmonth, report_date, market_location, brackets, price_unit)
    # ]
    # # 2) Add column(s) containing price-related info.
    # # 2-1) Price-related info. aggregated at the market location level
    dt[
      ,
      agg.weights := qty_weight_total / sum(qty_weight_total, na.rm = TRUE),
      by = .(report_year, report_yearmonth, report_date, market_location, price_unit)
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
      by = .(report_year, report_yearmonth, report_date, market_location, price_unit)
    ]
    dt[
      ,
      `:=` (
        agg.price_avg_simple_real =
          mapply(get_real.price, report_yearmonth, agg.price_avg_simple) %>%
            as.vector(., mode = "numeric"),
        agg.price_avg_weighted_real =
          mapply(get_real.price, report_yearmonth, agg.price_avg_weighted) %>%
            as.vector(., mode = "numeric")
      )
    ]
    # # 2-2) Price-related info. at the weight bracket level
    # dt[
    #   ,
    #   weights := qty_weight_total / sum(qty_weight_total, na.rm = TRUE),
    #   by =
    #     .(report_year, report_yearmonth, report_date, market_location, brackets, price_unit)
    # ]
    # dt[
    #   ,
    #   `:=` (
    #     price_avg_simple = (
    #       mean(avg_price, na.rm = TRUE) %>%
    #         round(., digits = 2)
    #     ),
    #     price_avg_weighted = (
    #       weighted.mean(avg_price, weights, na.rm = TRUE) %>%
    #         round(., digits = 2)
    #     )
    #   ),
    #   by =
    #     .(report_year, report_yearmonth, report_date, market_location, brackets, price_unit)
    # ]
    dt_expanded <- merge(
      x = expand.grid(
        report_date = dt[, .N, by = .(report_date)]$report_date,
        market_location = dt[, .N, by = .(market_location)]$market_location,
        stringsAsFactors = FALSE
      ) %>%
        setDT(.),
      # y = unique(dt),
      y = unique(
        dt[
          ,
          .SD,
          .SDcols = c(
            "slug_id", "slug_name", "report_date", "report_title",
            "published_datetime", "market_type_category", "market_type",
            "market_location_name", "market_location_city",
            "market_location_state", "market_location",
            "category", "class", "commodity",
            "frame", "muscle_grade", "quality_grade_name", "yield_grade",
            "dressing", "pregnancy_stage", "offspring_weight_est",
            "price_unit", "agg.price_avg_simple", "agg.price_avg_simple_real",
            "agg.qty_headcount", "agg.qty_weight_total", "agg.qty_weight_avg"
          )
        ]
      ),
      by = c("report_date", "market_location"),
      all.x = TRUE
    )
    dt_expanded[is.na(agg.qty_headcount), agg.qty_headcount := 0]
    dt_expanded[is.na(agg.qty_weight_total), agg.qty_weight_total := 0]
    dt_expanded
  } else {
    NULL
  }
})



observeEvent(
  c(
    input$auction.report_multiple_category,
    input$auction.report_multiple_class,
    input$auction.report_multiple_commodity,
    input$auction.report_multiple_quality.measures_frame,
    input$auction.report_multiple_quality.measures_muscle.grade,
    input$auction.report_multiple_quality.measures_quality.grade.name,
    input$auction.report_multiple_quality.measures_yield.grade,
    input$auction.report_multiple_quality.measures_dressing,
    input$auction.report_multiple_quality.measures_pregnancy.stage,
    input$auction.report_multiple_quality.measures_offspring.weight.est
  ), {
  values$multiple_market.location.selected <- NULL
})
observeEvent(
  input$auction.report_multiple_add.location, {
  values$multiple_market.location.selected <- c(
    values$multiple_market.location.selected,
    input$auction.report_multiple_market.location_city.with.state
  )
})
observeEvent(
  input$auction.report_multiple_location.selected, {
  values$multiple_market.location.selected <-
      input$auction.report_multiple_location.selected
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE
)



auction.report_multiple_dates <- eventReactive(
  input$auction.report_multiple_make.plots, {
  if (!is.null(dt_auction.report_multiple_for.plot())) {
    dt_auction.report_multiple_for.plot()[
      , .N, keyby = .(report_date)
    ]$report_date
  } else {
    NULL
  }
})



plot_auction.report_multiple_price <- eventReactive(
  c(
    input$auction.report_multiple_make.plots,
    input$auction.report_multiple_price.adjustment,
    input$auction.report_multiple_price,
    input$auction.report_multiple_time.period
  ), {
  if (
    !is.null(dt_auction.report_multiple_for.plot()) &
      !is.null(input$auction.report_multiple_time.period)
  ) {
    if (input$auction.report_multiple_price.adjustment == FALSE) {
      auction.report_multiple_price.var <- "agg.price_avg_simple"
    } else {
      auction.report_multiple_price.var <- "agg.price_avg_simple_real"
    }

    if (
      dt_auction.report_multiple_for.plot()[
        , .N, by = .(market_location)
      ][
        , .N
      ] > 4
    ) {
      color.option <- 'scale_color_brewer(palette = "Spectral")'
    } else {
      color.option <- 'scale_color_manual(values = COLOR.PAL_SIGNAL)'
    }

    ggplot.obj <-
      ggplot(
        data = dt_auction.report_multiple_for.plot()[
          !is.na(get(auction.report_multiple_price.var)) &
            input$auction.report_multiple_time.period[1] <= report_date &
            report_date <= input$auction.report_multiple_time.period[2]
        ]
      ) +
        geom_line(
          aes_string(
            x = "report_date",
            y = auction.report_multiple_price.var,
            group = "market_location"
          ),
          color = "black", alpha = 0.25, lwd = 0.7
        ) +
        geom_line(
          aes_string(
            x = "report_date",
            y = auction.report_multiple_price.var,
            group = "market_location",
            color = "market_location"
          ),
          lwd = 0.3
        ) +
        geom_point(
          aes_string(
            x = "report_date",
            y = auction.report_multiple_price.var
          ),
          color = "black", alpha = 0.25, size = 1.3
        ) +
        geom_point(
          aes_string(
            x = "report_date",
            y = auction.report_multiple_price.var,
            color = "market_location"
          ),
          size = 0.8
        ) +
        scale_x_date(date_labels = "%b. %Y") +
        scale_y_continuous(labels = scales::comma) +
        eval(parse(text = color.option)) +
        labs(
          x = "",
          y = "Per Cwt",
          color = "Market Location(s)"
        ) +
        theme_minimal()
  }
})


plot_auction.report_multiple_qty <- eventReactive(
  c(
    input$auction.report_multiple_make.plots,
    input$auction.report_multiple_price.adjustment,
    input$auction.report_multiple_qty,
    input$auction.report_multiple_time.period
  ), {
  if (
    !is.null(dt_auction.report_multiple_for.plot()) &
      # !is.null(input$auction.report_multiple_interval.length) &
      !is.null(input$auction.report_multiple_time.period)
  ) {
    if (input$auction.report_multiple_qty %in% "Headcounts") {
      auction.report_multiple_qty.var <- "agg.qty_headcount"
      labs_y.axis <- "Headcounts"
    } else {
      auction.report_multiple_qty.var <- "agg.qty_weight_total"
      labs_y.axis <- "Cwt"
    }

    if (dt_auction.report_multiple_for.plot()[, .N, by = .(market_location)][, .N] > 4) {
      color.option <- 'scale_fill_brewer(palette = "Spectral")'
    } else {
      color.option <- 'scale_fill_manual(values = COLOR.PAL_SIGNAL)'
    }


    ggplot.obj <-
      ggplot(
        data = dt_auction.report_multiple_for.plot()[
          input$auction.report_multiple_time.period[1] <= report_date &
            report_date <= input$auction.report_multiple_time.period[2]
        ]
      ) +
        geom_area(
          aes_string(
            x = "report_date",
            y = auction.report_multiple_qty.var,
            fill = "market_location"
          ),
          alpha = 0.5
        ) +
        geom_line(
          aes_string(
            x = "report_date",
            y = auction.report_multiple_qty.var,
            group = "market_location"
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
          fill = "Weight Bracket(s)"
        ) +
        theme_minimal()
  }
})




# -------
output$ui_auction.report_multiple_category <- renderUI({
    selectInput(
      inputId = "auction.report_multiple_category",
      label = "Category",
      choices = dt_cattle[
        market_type_category %in% "Auction", .N, keyby = .(category)
      ]$category,
      selected = NULL
    )
  })

output$ui_auction.report_multiple_class <- renderUI({
  selectInput(
    inputId = "auction.report_multiple_class",
    label = "Class",
    choices = NULL
  )
})
observeEvent(
  input$auction.report_multiple_category, {
  updateSelectInput(
    session,
    inputId = "auction.report_multiple_class",
    choices = dt_cattle[
      market_type_category %in% "Auction" &
        category %in% input$auction.report_multiple_category &
        !is.na(avg_weight),
      .N,
      keyby = .(class)
    ]$class,
    selected = NULL
  )
})

output$ui_auction.report_multiple_commodity <- renderUI({
  selectInput(
    inputId = "auction.report_multiple_commodity",
    label = "Commodity",
    choices = NULL
  )
})
observeEvent(
  c(
    input$auction.report_multiple_category,
    input$auction.report_multiple_class
  ), {
  updateSelectInput(
    session,
    inputId = "auction.report_multiple_commodity",
    choices = dt_cattle[
      market_type_category %in% "Auction" &
        category %in% input$auction.report_multiple_category &
        class %in% input$auction.report_multiple_class &
        !is.na(avg_weight),
      .N,
      keyby = .(commodity)
    ]$commodity,
    selected = NULL
  )
})


help_auction.report_multiple_get.condition_quality.measures <- function (
  var.name_in.str
) {
  if (var.name_in.str == "frame") {
    condition <- "all()"
  } else {
    idx <- which(names(list_quality.measures_detail) == var.name_in.str)
    col.names <- names(list_quality.measures_detail)[1:(idx - 1)]
    col.names_selected <- col.names[
      col.names %in% auction.report_multiple_quality.measures()
    ]
    condition <- if (length(col.names_selected) > 0) {
      paste0(
        col.names_selected,
        " %in% input$auction.report_multiple_quality.measures_",
        str_replace_all(col.names_selected, "_", "."),
        collapse = " & "
      )
    } else {
      "all()"
    }
  }
  return (condition)
}


output$ui_auction.report_multiple_quality.measures_frame <- renderUI({
  if ("frame" %in% auction.report_multiple_quality.measures()) {
    selectInput(
      inputId = "auction.report_multiple_quality.measures_frame",
      label = "Frame",
      choices = NULL
    )
  } else {
    selectInput(
      inputId = "auction.report_multiple_quality.measures_frame",
      label = "Frame",
      # choices = "NA"
      choices = NA
    )
  }
})
observeEvent(
  c(
    input$auction.report_multiple_category,
    input$auction.report_multiple_class,
    input$auction.report_multiple_commodity
  ), {
  if ("frame" %in% auction.report_multiple_quality.measures()) {
    updateSelectInput(
      session,
      inputId = "auction.report_multiple_quality.measures_frame",
      choices = dt_auction.report_multiple_quality.measures()[
        help_auction.report_multiple_get.condition_quality.measures("frame") %>%
          parse(text = .) %>%
          eval(.),
        .N,
        keyby = .(frame)
      ]$frame,
      selected = NULL
    )
  }
})

output$ui_auction.report_multiple_quality.measures_muscle.grade <- renderUI({
  if ("muscle_grade" %in% auction.report_multiple_quality.measures()) {
    selectInput(
      inputId = "auction.report_multiple_quality.measures_muscle.grade",
      label = "Muscle Grade",
      choices = NULL
    )
  } else {
    selectInput(
      inputId = "auction.report_multiple_quality.measures_muscle.grade",
      label = "Muscle Grade",
      # choices = "NA"
      choices = NA
    )
  }
})
observeEvent(
  c(
    input$auction.report_multiple_category,
    input$auction.report_multiple_class,
    input$auction.report_multiple_commodity,
    input$auction.report_multiple_quality.measures_frame
  ), {
  if ("muscle_grade" %in% auction.report_multiple_quality.measures()) {
    updateSelectInput(
      session,
      inputId = "auction.report_multiple_quality.measures_muscle.grade",
      choices = dt_auction.report_multiple_quality.measures()[
        help_auction.report_multiple_get.condition_quality.measures("muscle_grade") %>%
          parse(text = .) %>%
          eval(.),
        .N,
        keyby = .(muscle_grade)
      ]$muscle_grade,
      selected = NULL
    )
  }
})

output$ui_auction.report_multiple_quality.measures_quality.grade.name <-
  renderUI({
    if ("quality_grade_name" %in% auction.report_multiple_quality.measures()) {
      selectInput(
        inputId = "auction.report_multiple_quality.measures_quality.grade.name",
        label = "Quality Grade",
        choices = NULL
      )
    } else {
      selectInput(
        inputId = "auction.report_multiple_quality.measures_quality.grade.name",
        label = "Quality Grade",
        choices = NA
      )
    }
  })
observeEvent(
  c(
    input$auction.report_multiple_category,
    input$auction.report_multiple_class,
    input$auction.report_multiple_commodity,
    input$auction.report_multiple_quality.measures_frame,
    input$auction.report_multiple_quality.measures_muscle.grade
  ), {
  if ("quality_grade_name" %in% auction.report_multiple_quality.measures()) {
    updateSelectInput(
      session,
      inputId = "auction.report_multiple_quality.measures_quality.grade.name",
      choices = dt_auction.report_multiple_quality.measures()[
        help_auction.report_multiple_get.condition_quality.measures(
          "quality_grade_name"
        ) %>%
          parse(text = .) %>%
          eval(.),
        .N,
        keyby = .(quality_grade_name)
      ]$quality_grade_name,
      selected = NULL
    )
  }
})

output$ui_auction.report_multiple_quality.measures_yield.grade <-
  renderUI({
    if ("yield_grade" %in% auction.report_multiple_quality.measures()) {
      selectInput(
        inputId = "auction.report_multiple_quality.measures_yield.grade",
        label = "Yield Grade",
        choices = NULL
      )
    } else {
      selectInput(
        inputId = "auction.report_multiple_quality.measures_yield.grade",
        label = "Yield Grade",
        choices = NA
      )
    }
  })
observeEvent(
  c(
    input$auction.report_multiple_category,
    input$auction.report_multiple_class,
    input$auction.report_multiple_commodity,
    input$auction.report_multiple_quality.measures_frame,
    input$auction.report_multiple_quality.measures_muscle.grade,
    input$auction.report_multiple_quality.measures_quality.grade.name
  ), {
  if ("yield_grade" %in% auction.report_multiple_quality.measures()) {
    updateSelectInput(
      session,
      inputId = "auction.report_multiple_quality.measures_yield.grade",
      choices = dt_auction.report_multiple_quality.measures()[
        help_auction.report_multiple_get.condition_quality.measures("yield_grade") %>%
          parse(text = .) %>%
          eval(.),
        .N,
        keyby = .(yield_grade)
      ]$yield_grade,
      selected = NULL
    )
  }
})

output$ui_auction.report_multiple_quality.measures_dressing <-
  renderUI({
    if ("dressing" %in% auction.report_multiple_quality.measures()) {
      selectInput(
        inputId = "auction.report_multiple_quality.measures_dressing",
        label = "Dressing",
        choices = NULL
      )
    } else {
      selectInput(
        inputId = "auction.report_multiple_quality.measures_dressing",
        label = "Dressing",
        choices = NA
      )
    }
  })
observeEvent(
  c(
    input$auction.report_multiple_category,
    input$auction.report_multiple_class,
    input$auction.report_multiple_commodity,
    input$auction.report_multiple_quality.measures_frame,
    input$auction.report_multiple_quality.measures_muscle.grade,
    input$auction.report_multiple_quality.measures_quality.grade.name,
    input$auction.report_multiple_quality.measures_yield.grade
  ), {
  if ("dressing" %in% auction.report_multiple_quality.measures()) {
    updateSelectInput(
      session,
      inputId = "auction.report_multiple_quality.measures_dressing",
      choices = dt_auction.report_multiple_quality.measures()[
        help_auction.report_multiple_get.condition_quality.measures(
          "dressing"
        ) %>%
          parse(text = .) %>%
          eval(.),
        .N,
        keyby = .(dressing)
      ]$dressing,
      selected = NULL
    )
  }
})

output$ui_auction.report_multiple_quality.measures_pregnancy.stage <-
  renderUI({
    if ("pregnancy_stage" %in% auction.report_multiple_quality.measures()) {
      selectInput(
        inputId = "auction.report_multiple_quality.measures_pregnancy.stage",
        label = "Pregnancy Stage",
        choices = NULL
      )
    } else {
      selectInput(
        inputId = "auction.report_multiple_quality.measures_pregnancy.stage",
        label = "Pregnancy Stage",
        choices = NA
      )
    }
  })
observeEvent(
  c(
    input$auction.report_multiple_category,
    input$auction.report_multiple_class,
    input$auction.report_multiple_commodity,
    input$auction.report_multiple_quality.measures_frame,
    input$auction.report_multiple_quality.measures_muscle.grade,
    input$auction.report_multiple_quality.measures_quality.grade.name,
    input$auction.report_multiple_quality.measures_yield.grade,
    input$auction.report_multiple_quality.measures_dressing
  ), {
  if ("pregnancy_stage" %in% auction.report_multiple_quality.measures()) {
    updateSelectInput(
      session,
      inputId = "auction.report_multiple_quality.measures_pregnancy.stage",
      choices = dt_auction.report_multiple_quality.measures()[
        help_auction.report_multiple_get.condition_quality.measures(
          "pregnancy_stage"
        ) %>%
          parse(text = .) %>%
          eval(.),
        .N,
        keyby = .(pregnancy_stage)
      ]$pregnancy_stage,
      selected = NULL
    )
  }
})

output$ui_auction.report_multiple_quality.measures_offspring.weight.est <-
  renderUI({
    if ("offspring_weight_est" %in% auction.report_multiple_quality.measures()) {
      selectInput(
        inputId = "auction.report_multiple_quality.measures_offspring.weight.est",
        label = "Offspring Weight Est.",
        choices = NULL
      )
    } else {
      selectInput(
        inputId = "auction.report_multiple_quality.measures_offspring.weight.est",
        label = "Offspring Weight Est.",
        choices = NA
      )
    }
  })
observeEvent(
  c(
    input$auction.report_multiple_category,
    input$auction.report_multiple_class,
    input$auction.report_multiple_commodity,
    input$auction.report_multiple_quality.measures_frame,
    input$auction.report_multiple_quality.measures_muscle.grade,
    input$auction.report_multiple_quality.measures_quality.grade.name,
    input$auction.report_multiple_quality.measures_yield.grade,
    input$auction.report_multiple_quality.measures_dressing,
    input$auction.report_multiple_quality.measures_pregnancy.stage
  ), {
  if ("offspring_weight_est" %in% auction.report_multiple_quality.measures()) {
    updateSelectInput(
      session,
      inputId = "auction.report_multiple_quality.measures_offspring.weight.est",
      choices = dt_auction.report_multiple_quality.measures()[
        help_auction.report_multiple_get.condition_quality.measures(
          "offspring_weight_est"
        ) %>%
          parse(text = .) %>%
          eval(.),
        .N,
        keyby = .(offspring_weight_est)
      ]$offspring_weight_est,
      selected = NULL
    )
  }
})


observeEvent(
  c(
    input$auction.report_multiple_category,
    input$auction.report_multiple_class,
    input$auction.report_multiple_commodity,
    input$auction.report_multiple_quality.measures_frame,
    input$auction.report_multiple_quality.measures_muscle.grade,
    input$auction.report_multiple_quality.measures_quality.grade.name,
    input$auction.report_multiple_quality.measures_yield.grade,
    input$auction.report_multiple_quality.measures_dressing,
    input$auction.report_multiple_quality.measures_pregnancy.stage,
    input$auction.report_multiple_quality.measures_offspring.weight.est
  ), {
  output$ui_auction.report_multiple_market.location_state <- renderUI({
    if (!is.null(auction.report_multiple_condition.for.state())) {
      selectInput(
        inputId = "auction.report_multiple_market.location_state",
        label = "Select a State",
        choices = dt_cattle[
          parse(text = auction.report_multiple_condition.for.state()) %>%
            eval(.),
          .N,
          keyby = .(market_location_state)
        ]$market_location_state,
        multiple = FALSE
      )
    } else {
      NULL
    }
  })
})

observeEvent(
  c(
    input$auction.report_multiple_category,
    input$auction.report_multiple_class,
    input$auction.report_multiple_commodity,
    input$auction.report_multiple_quality.measures_frame,
    input$auction.report_multiple_quality.measures_muscle.grade,
    input$auction.report_multiple_quality.measures_quality.grade.name,
    input$auction.report_multiple_quality.measures_yield.grade,
    input$auction.report_multiple_quality.measures_dressing,
    input$auction.report_multiple_quality.measures_pregnancy.stage,
    input$auction.report_multiple_quality.measures_offspring.weight.est,
    input$auction.report_multiple_market.location_state
  ), {
  output$ui_auction.report_multiple_market.location_city.with.state <- renderUI({
    if (
      !is.null(auction.report_multiple_condition.for.state()) &
        !is.null(input$auction.report_multiple_market.location_state)
    ) {
      selectInput(
        inputId = "auction.report_multiple_market.location_city.with.state",
        label = "Select a City",
        choices = dt_cattle[
          parse(text = auction.report_multiple_condition.for.state()) %>%
            eval(.)
        ][
          market_location_state %in%
            input$auction.report_multiple_market.location_state,
          .N,
          keyby = .(market_location)
        ]$market_location,
        multiple = FALSE
      )
    } else {
      NULL
    }
  })
})


output$ui_auction.report_multiple_add.location <- renderUI({
  actionButton(
    inputId = "auction.report_multiple_add.location",
    label = "Add a Market Location",
    icon = icon(name = "download", lib = "font-awesome")
  )
})

observeEvent(
  input$auction.report_multiple_add.location, {
  output$ui_auction.report_multiple_location.selected <- renderUI({
    selectizeInput(
      inputId = "auction.report_multiple_location.selected",
      label = "Market Location(s) selected",
      choices = values$multiple_market.location.selected,
      selected = values$multiple_market.location.selected,
      multiple = TRUE,
      options = list(maxItems = 11)
    )
  })
},
  ignoreInit = TRUE,
  ignoreNULL = FALSE
)




output$ui_auction.report_multiple_make.plots <- renderUI({
    actionButton(
      inputId = "auction.report_multiple_make.plots",
      label = "Press the Button to Generate Plots",
      icon = icon(name = "database", lib = "font-awesome")
    )
  })



output$ui_auction.report_multiple_qty <- renderUI({
  radioButtons(
    inputId = "auction.report_multiple_qty",
    label = "Quantity Data",
    choices = c("Headcounts", "Total Weights"),
    selected = "Headcounts",
    inline = TRUE
  )
})


output$ui_auction.report_multiple_price.adjustment <- renderUI({
  checkboxInput(
    inputId = "auction.report_multiple_price.adjustment",
    label = "Ajust Prices",
    value = FALSE
  )
})








observeEvent(
  c(
    input$auction.report_multiple_make.plots,
    auction.report_multiple_dates()
  ), {
  output$ui_auction.report_multiple_time.period <- renderUI({
    if (length(auction.report_multiple_dates()) > 0) {
      min_ <- auction.report_multiple_dates() %>% min(.)
      max_ <- auction.report_multiple_dates() %>% max(.)
      sliderInput(
        inputId = "auction.report_multiple_time.period",
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






output$auction.report_multiple_figure_price <- renderPlotly({
  if (!is.null(plot_auction.report_multiple_price())) {
    plot_auction.report_multiple_price()
  } else {
    NULL
  }
})

output$auction.report_multiple_figure_qty <- renderPlotly({
  if (!is.null(plot_auction.report_multiple_qty())) {
    plot_auction.report_multiple_qty()
  } else {
    NULL
  }
})


# output$ui_auction.report_multiple_test <- renderUI({
#   selectizeInput(
#     inputId = "auction.report_multiple_test",
#     label = "Test",
#     choices = input$auction.report_multiple_make.plots,
#   )
# })
# output$ui_auction.report_multiple_test_table <- renderDataTable({
  # if (!is.null(dt_auction.report_multiple_for.plot())) {
  #   dt_auction.report_multiple_for.plot()
  # } else {
  #   NULL
  # }
# })
