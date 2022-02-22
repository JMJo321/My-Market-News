# < Description > *
# > Script Group Indicator Number and Name
# # : B-04, Create Sample Figures
# #
# > Script Number(s)
# # : B-04-01A-1
# #
# > Purpose of the script(s)
# # : Define functions utilized in B-04-01A.

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Set working directory, and run header script
# ------------------------------------------------------------------------------
# ------- Set project name -------
# (Not Applicable)


# ------- Set working directory -------
# (Not Applicable)


# ------- Run the header script -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Define path(s), parameter(s) and function(s)
# ------------------------------------------------------------------------------
# ------- Define path(s) -------
# (Not Applicable)


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# (Refer to below)


# ------------------------------------------------------------------------------
# Define functions for creating DT(s)
# ------------------------------------------------------------------------------
# ------- To create a DT including data about a specific commodity -------
get_DT_commodity <- function (
  dt.for.category_dt,
  subsetting.conditions_list,
  interval.for.weight.bracket = 100
) {
  # # 1. Create a DT by subsetting the given DT
  dt_base <- dt.for.category_dt[
    get_subset.condition_in.str(subsetting.conditions_list) %>%
      parse(text = .) %>%
      eval(.)
  ]

  # # 2. Modify the DT created
  # # 2.1. Add column(s)
  # # 2.1.1. Add a column showing weight brackets
  parameter_weight_upper.limit <- 10^4  # An arbitrary large number.
  dt_base[
    ,
    weight_bracket := cut(
      avg_weight,
      breaks = seq(
        0, parameter_weight_upper.limit, by = interval.for.weight.bracket
      ),
      include.lowest = TRUE,
      dig.lab = as.character(parameter_weight_upper.limit) %>%
        str_length(.)
    )
  ]
  # # 2.1.2. Add column(s) containing quantity-related info.
  # # 2.1.2.1. Quantity-related info. aggregated at the market location level
  dt_base[
    ,
    `:=` (
      agg.qty_headcount = sum(head_count, na.rm = TRUE),
      agg.qty_weight_total = sum(head_count * avg_weight, na.rm = TRUE)
    ),
    by = .(report_yearmonth, report_date, market_location_city)
  ]
  dt_base[
    ,
    agg.qty_weight_avg := round(
      agg.qty_weight_total / agg.qty_headcount, digits = 0
    ),
    by = .(report_yearmonth, report_date, market_location_city)
  ]
  # # 2.1.2.2. Quantity-related info. at the weight bracket level
  dt_base[
    ,
    `:=` (
      qty_headcount = sum(head_count, na.rm = TRUE),
      qty_weight_total = sum(head_count * avg_weight, na.rm = TRUE)
    ),
    by = .(report_yearmonth, report_date, market_location_city, weight_bracket)
  ]
  dt_base[
    ,
    qty_weight_avg := round(qty_weight_total / qty_headcount, digits = 0),
    by = .(report_yearmonth, report_date, market_location_city, weight_bracket)
  ]
  # # 2.1.3. Add column(s) containing price-related info.
  # # 2.1.3.1. Price-related info. aggregated at the market location level
  dt_base[
    ,
    agg.weights := qty_weight_total / sum(qty_weight_total, na.rm = TRUE),
    by = .(report_yearmonth, report_date, market_location_city)
  ]
  dt_base[
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
    by = .(report_yearmonth, report_date, market_location_city)
  ]
  # # 2.1.3.2. Price-related info. at the weight bracket level
  dt_base[
    ,
    weights := qty_weight_total / sum(qty_weight_total, na.rm = TRUE),
    by = .(report_yearmonth, report_date, market_location_city, weight_bracket)
  ]
  dt_base[
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
    by = .(report_yearmonth, report_date, market_location_city, weight_bracket)
  ]

  return(dt_base)
}

# ------- To create a DT for making figure(s) -------
# # 1. For figure(s) containing observations by market location
get_DT_for.plot_selected.market.location <- function (
  dt.for.commodity_dt,
  subsetting.conditions
) {
  # # 1. Create a DT for making figures
  # # 1.1. Create a DT by subsetting the given DT and then dropping duplicated
  # #      observations
  cols_by.weight.bracket <- c(
    "report_yearmonth","report_date",
    "weight_bracket",
    "agg.qty_headcount", "agg.qty_weight_total", "agg.qty_weight_avg",
    "qty_headcount", "qty_weight_total", "qty_weight_avg",
    "agg.price_avg_simple", "agg.price_avg_weighted",
    "price_avg_simple", "price_avg_weighted"
  )
  dt_for.plot_selected.market.location <- dt.for.commodity_dt[
    get_subset.condition_in.str(subsetting.conditions) %>%
      parse(text = .) %>%
      eval(.)
  ][
    , .N, keyby = cols_by.weight.bracket
  ][
    , N := NULL
  ]

  # # 1.2. Generate a DT by melting the DT created above
  id.vars_by.weight.bracket <-
    c("report_yearmonth", "report_date", "weight_bracket")
  measure.vars_by.weight.bracket <- cols_by.weight.bracket[
    !cols_by.weight.bracket %in% id.vars_by.weight.bracket
  ]
  suppressWarnings(
    dt_for.plot_selected.market.location_melted <- melt(
      dt_for.plot_selected.market.location,
      id.vars = id.vars_by.weight.bracket,
      measure.vars = measure.vars_by.weight.bracket
    )
  )


  # # 2. Modify the melted DT
  # # 2.1. Add column(s)
  # # 2.1.1. Add a column categorizing observations based on values of
  # #        the column `variable`
  dt_for.plot_selected.market.location_melted[
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

  return(dt_for.plot_selected.market.location_melted)
}


# # 2. For figure(s) containing observations by weight bracket
get_DT_for.plot_selected.weight.bracket <- function (
  dt.for.commodity_dt,
  subsetting.conditions
) {
  # # 1. Create a DT for making figures
  # # 1.1. Create a DT by subsetting the given DT and then dropping duplicated
  # #      observations
  cols_by.market.location <- c(
    "report_yearmonth", "report_date",
    "market_location_city", "market_location_state",
    "agg.qty_headcount", "agg.qty_weight_total", "agg.qty_weight_avg",
    "qty_headcount", "qty_weight_total", "qty_weight_avg",
    "agg.price_avg_simple", "agg.price_avg_weighted",
    "price_avg_simple", "price_avg_weighted"
  )
  dt_for.plot_selected.weight.bracket <- dt.for.commodity_dt[
    get_subset.condition_in.str(subsetting.conditions) %>%
      parse(text = .) %>%
      eval(.)
  ][
    , .N, keyby = cols_by.market.location
  ][
    , N := NULL
  ]

  # # 1.2. Generate a DT by melting the DT created above
  id.vars_by.market.location <- c(
    "report_yearmonth", "report_date",
    "market_location_city", "market_location_state"
  )
  measure.vars_by.market.location <- cols_by.market.location[
    !cols_by.market.location %in% id.vars_by.market.location
  ]
  suppressWarnings(
    dt_for.plot_selected.weight.bracket_melted <- melt(
      dt_for.plot_selected.weight.bracket,
      id.vars = id.vars_by.market.location,
      measure.vars = measure.vars_by.market.location
    )
  )


  # # 2. Modify the melted DT
  # # 2.1. Add column(s)
  # # 2.1.1. Add a column categorizing observations based on values of
  # #        the column `variable`
  dt_for.plot_selected.weight.bracket_melted[
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
  # # 2.1.2. Add a column showing city-state pairs
  dt_for.plot_selected.weight.bracket_melted[
    , market_location := paste(
    market_location_city, market_location_state, sep = ", "
  )
  ]

  return(dt_for.plot_selected.weight.bracket_melted)
}


# ------------------------------------------------------------------------------
# Define function(s) for creating ggplot object(s)
# ------------------------------------------------------------------------------
# ------- Figure(s) for selected market location(s) -------
# # 1. Only for the selected weight bracket
get_ggplot.obj_selected.market.location <- function (
  dt.for.plot_dt,
  begin.of.period_yearmonth, end.of.period_yearmonth,
  quantity.var_str, price.var_str,
  weight.brackets_vector
) {
  plot_selected.market.location <-
    ggplot() +
      geom_line(
        data = dt.for.plot_dt[
          begin.of.period_yearmonth <= report_yearmonth &
            report_yearmonth <= end.of.period_yearmonth &
            variable %in% c(quantity.var_str, price.var_str) &
            weight_bracket %in% weight.brackets_vector
        ],
        aes(x = report_date, y = value, group = weight_bracket),
        color = "black", alpha = 0.25, lwd = 0.7
      ) +
      geom_point(
        data = dt.for.plot_dt[
          begin.of.period_yearmonth <= report_yearmonth &
            report_yearmonth <= end.of.period_yearmonth &
            variable %in% c(quantity.var_str, price.var_str) &
            weight_bracket %in% weight.brackets_vector
        ],
        aes(x = report_date, y = value),
        color = "black", alpha = 0.25, size = 1.3
      ) +
      geom_line(
        data = dt.for.plot_dt[
          begin.of.period_yearmonth <= report_yearmonth &
            report_yearmonth <= end.of.period_yearmonth &
            variable %in% c(quantity.var_str, price.var_str) &
            weight_bracket %in% weight.brackets_vector
        ],
        aes(x = report_date, y = value, color = weight_bracket),
        lwd = 0.3
      ) +
      geom_point(
        data = dt.for.plot_dt[
          begin.of.period_yearmonth <= report_yearmonth &
            report_yearmonth <= end.of.period_yearmonth &
            variable %in% c(quantity.var_str, price.var_str) &
            weight_bracket %in% weight.brackets_vector
        ],
        aes(x = report_date, y = value, color = weight_bracket),
        size = 0.8
      ) +
      facet_grid(category_upper ~ ., scales = "free_y") +
      scale_x_date(date_labels = "%b. %Y") +
      scale_y_continuous(label = scales::comma) +
      scale_color_brewer(palette = "Spectral") +
      labs(
        x = "",
        y = "",
        color = "Weight Brackets"
      ) +
      theme_linedraw() +
      theme(strip.text = element_text(face = "bold"))

  return(plot_selected.market.location)
}


# # 2. Aggregated info. for selected market locations
get_ggplot.obj_selected.market.location_agg.info <- function (
  dt.for.plot_dt,
  begin.of.period_yearmonth, end.of.period_yearmonth,
  quantity.var_str, price.var_str,
  market_location_vector
) {
  plot_selected.weight.bracket_agg.info <-
    ggplot() +
      geom_line(
        data = dt.for.plot_dt[
          begin.of.period_yearmonth <= report_yearmonth &
            report_yearmonth <= end.of.period_yearmonth &
            variable %in% c(quantity.var_str, price.var_str) &
            market_location %in% market_location_vector
        ],
        aes(x = report_date, y = value, group = market_location),
        color = "black", alpha = 0.25, lwd = 0.7
      ) +
      geom_point(
        data = dt.for.plot_dt[
          begin.of.period_yearmonth <= report_yearmonth &
            report_yearmonth <= end.of.period_yearmonth &
            variable %in% c(quantity.var_str, price.var_str) &
            market_location %in% market_location_vector
        ],
        aes(x = report_date, y = value),
        color = "black", alpha = 0.25, size = 1.3
      ) +
      geom_line(
        data = dt.for.plot_dt[
          begin.of.period_yearmonth <= report_yearmonth &
            report_yearmonth <= end.of.period_yearmonth &
            variable %in% c(quantity.var_str, price.var_str) &
            market_location %in% market_location_vector
        ],
        aes(x = report_date, y = value, color = market_location),
        lwd = 0.3
      ) +
      geom_point(
        data = dt.for.plot_dt[
          begin.of.period_yearmonth <= report_yearmonth &
            report_yearmonth <= end.of.period_yearmonth &
            variable %in% c(quantity.var_str, price.var_str) &
            market_location %in% market_location_vector
        ],
        aes(x = report_date, y = value, color = market_location),
        size = 0.8
      ) +
      facet_grid(category_upper ~ ., scales = "free_y") +
      scale_x_date(date_labels = "%b. %Y") +
      scale_y_continuous(label = scales::comma) +
      scale_color_brewer(palette = "Spectral") +
      labs(
        x = "",
        y = "",
        color = "Markets"
      ) +
      theme_linedraw() +
      theme(strip.text = element_text(face = "bold"))

  return(plot_selected.weight.bracket_agg.info)
}


# ------- Figure(s) for selected weight bracket(s) -------
get_ggplot.obj_selected.weight.bracket <-  function (
  dt.for.plot_dt,
  begin.of.period_yearmonth, end.of.period_yearmonth,
  quantity.var_str, price.var_str,
  market_location_vector
) {
  plot_selected.weight.bracket <-
    ggplot() +
      geom_line(
        data = dt.for.plot_dt[
          begin.of.period_yearmonth <= report_yearmonth &
            report_yearmonth <= end.of.period_yearmonth &
            variable %in% c(quantity.var_str, price.var_str) &
            market_location %in% market_location_vector
        ],
        aes(x = report_date, y = value, group = market_location),
        color = "black", alpha = 0.25, lwd = 0.7
      ) +
      geom_point(
        data = dt.for.plot_dt[
          begin.of.period_yearmonth <= report_yearmonth &
            report_yearmonth <= end.of.period_yearmonth &
            variable %in% c(quantity.var_str, price.var_str) &
            market_location %in% market_location_vector
        ],
        aes(x = report_date, y = value),
        color = "black", alpha = 0.25, size = 1.3
      ) +
      geom_line(
        data = dt.for.plot_dt[
          begin.of.period_yearmonth <= report_yearmonth &
            report_yearmonth <= end.of.period_yearmonth &
            variable %in% c(quantity.var_str, price.var_str) &
            market_location %in% market_location_vector
        ],
        aes(x = report_date, y = value, color = market_location),
        lwd = 0.3
      ) +
      geom_point(
        data = dt.for.plot_dt[
          begin.of.period_yearmonth <= report_yearmonth &
            report_yearmonth <= end.of.period_yearmonth &
            variable %in% c(quantity.var_str, price.var_str) &
            market_location %in% market_location_vector
        ],
        aes(x = report_date, y = value, color = market_location),
        size = 0.8
      ) +
      facet_grid(category_upper ~ ., scales = "free_y") +
      scale_x_date(date_labels = "%b. %Y") +
      scale_y_continuous(label = scales::comma) +
      scale_color_brewer(palette = "Spectral") +
      labs(
        x = "",
        y = "",
        color = "Markets"
      ) +
      theme_linedraw() +
      theme(strip.text = element_text(face = "bold"))

  return(plot_selected.weight.bracket)
}


# ------------------------------------------------------------------------------
# Define function(s) for miscellaneous work
# ------------------------------------------------------------------------------
# ------- Extract info. from DT(s) created -------
# # 1. Create a vector of market locations in the DT for a specific commodity
get_market.loctions <- function (dt.for.commodity_dt) {
  # ## Create a DT that include unique city-state pairs
  dt_market.location <- dt.for.commodity_dt[
    , .N, keyby = .(market_location_city, market_location_state)
  ][
    , N := NULL
  ]
  # ## Add a column including city-state info.
  dt_market.location[
    ,
    market_location := paste(
      market_location_city, market_location_state, sep = ", "
    )
  ]
  # ## Create a vector from the DT above
  derived.list_market.location <- dt_market.location[
    , .N, keyby = .(market_location)
  ][
    , N := NULL
  ]$market_location

  return(derived.list_market.location)
}
