library(stringr)
library(data.table)
library(ggplot2)
load("/Users/jmjo/Dropbox/00_JMJo/Projects/My-Market-News/02_Data/02_Intermediate/MMN-API/Data/MMN_Data_Category-Cattle_2022-02-14.RData")


cols_by <- c(
  "category", "class", "commodity", "frame",
  "muscle_grade", "quality_grade_name", "lot_desc", "age", "pregnancy_stage",
  "weight_collect", "offspring_weight_est", "yield_grade"
)
dt_cattle[category == "Cattle", .N, keyby = .(class)]
dt_cattle[category == "Cattle" & class == "Heifers", .N, keyby = .(commodity)]
dt_cattle[
  category == "Cattle" & class == "Heifers" & commodity == "Feeder Cattle",
  .N,
  keyby = cols_by
] %>%
  View(.)


cols_by_ <- c(
  "frame", "muscle_grade", "quality_grade_name", "yield_grade", "dressing",
  "pregnancy_stage", "offspring_weight_est"
)
dt_cattle[
  category == "Cattle" & class == "Heifers" & commodity == "Feeder Cattle",
  .N,
  keyby = cols_by_
]
dt_cattle[
  category == "Cattle" & class == "Heifers" & commodity == "Slaughter Cattle",
  .N,
  keyby = cols_by_
]
dt_cattle[
  category == "Cattle" & class == "Heifers" & commodity == "Feeder Dairy Calves",
  .N,
  keyby = cols_by_
]


# dt_cattle[
#   category == "Cattle" & class == "Heifers" & commodity == "Feeder Cattle",
#   .N,
#   keyby = cols_by_
# ][
#   , .N, keyby = .(lot_desc)
# ]
# dt_cattle[
#   category == "Cattle" & class == "Heifers" & commodity == "Feeder Cattle" &
#     (is.na(lot_desc) | lot_desc == "None"), # TODO: Difference?
#   .N,
#   keyby = cols_by_
# ]


dt_cattle[
  market_type_category == "Auction" &
    category == "Cattle" & class == "Heifers" & commodity == "Feeder Cattle" &
    frame == "Medium and Large" & muscle_grade == "1",
  .N,
  keyby = .(market_location_city)
]

dt_cattle[
  market_type_category == "Auction" &
    category == "Cattle" & class == "Heifers" & commodity == "Feeder Cattle" &
    frame == "Medium and Large" & muscle_grade == "1",
  .N,
  keyby = .(report_date, market_location_state)
][
  ,
  .N,
  keyby = .(report_date)
]

dt_interim_by.bracket <- dt_cattle[
  market_type_category == "Auction" &
    category == "Cattle" & class == "Heifers" & commodity == "Feeder Cattle" &
    frame == "Medium and Large" & muscle_grade == "1" &
    # report_date == as.Date("2018-01-08") &
    market_location_city == "Arcadia"
]
dt_interim_by.bracket[
  ,
  weight.bracket :=
    cut(avg_weight, breaks = seq(0, 10^4, by = 100), include.lowest = TRUE)
]
dt_interim_by.bracket[
  ,
  max.weight := max(avg_weight_max),
  by = .(report_date, weight.bracket)
]
dt_interim_by.bracket[
  ,
  min.weight := max(avg_weight_min),
  by = .(report_date, weight.bracket)
]
dt_interim_by.bracket[
  ,
  total_head.count := sum(head_count, na.rm = TRUE),
  by = .(report_date, weight.bracket)
]
dt_interim_by.bracket[
  ,
  total_weight := sum(head_count * avg_weight, na.rm = TRUE),
  by = .(report_date, weight.bracket)
]
dt_interim_by.bracket[
  ,
  avg.weight := (sum(head_count * avg_weight, na.rm = TRUE) / total_head.count) %>% round(.),
  by = .(report_date, weight.bracket)
]
dt_interim_by.bracket[
  ,
  avg.price := (mean(avg_price, na.rm = TRUE)) %>% round(.),
  by = .(report_date, weight.bracket)
]
# ## TODO: Simple mean? Or weighted mean?

cols_by <- c(
  "report_date",
  "weight.bracket",
  "total_head.count", "total_weight", "avg.weight",
  "avg.price"
)
dt_for.plot <- dt_interim_by.bracket[
  , .N, by = cols_by
][
  , N := NULL
]
dt_for.plot_melted <- melt(
  dt_for.plot,
  id.vars = c("report_date", "weight.bracket"),
  measure.vars = c("total_head.count", "total_weight", "avg.weight", "avg.price")
)
dt_for.plot_melted[
  str_detect(variable, "(weight$)|(count$)"),
  category := "Quantity"
]
dt_for.plot_melted[is.na(category), category := "Price"]


dt_for.plot[report_date == as.Date("2018-01-08")]

plot_by.bracket <-
  ggplot() +
    geom_line(
      data = dt_for.plot_melted[!variable %in% c("total_head.count", "avg.weight")],
      aes(x = report_date, y = value, group = weight.bracket),
      color = "black", alpha = 0.25, lwd = 0.7
    ) +
    geom_line(
      data = dt_for.plot_melted[!variable %in% c("total_head.count", "avg.weight")],
      aes(x = report_date, y = value, color = weight.bracket),
      lwd = 0.3
    ) +
    geom_point(
      data = dt_for.plot_melted[!variable %in% c("total_head.count", "avg.weight")],
      aes(x = report_date, y = value, color = weight.bracket),
      color = "black", size = 1.3
    ) +
    geom_point(
      data = dt_for.plot_melted[!variable %in% c("total_head.count", "avg.weight")],
      aes(x = report_date, y = value, color = weight.bracket),
      size = 1
    ) +
    facet_grid(category ~ ., scales = "free_y") +
    scale_y_continuous(label = scales::comma) +
    scale_color_brewer(palette = "Spectral") +
    labs(
      x = "", y = "", color = "Weight Brackets",
      caption = "Note: Auction, Cattle-Heifers-Feeder Cattle, Frame: Medium and Large, Muscle Grade: 1, Market: Arcadia"
    ) +
    theme_linedraw()



dt_interim_by.location <- dt_cattle[
  market_type_category == "Auction" &
    category == "Cattle" & class == "Heifers" & commodity == "Feeder Cattle" &
    frame == "Medium and Large" & muscle_grade == "1" &
    market_location_city %in% c("Arcadia", "Oklahoma City", "Dalhart")
]
dt_interim_by.location[
  ,
  weight.bracket :=
    cut(avg_weight, breaks = seq(0, 10^4, by = 100), include.lowest = TRUE)
]
dt_interim_by.location[
  ,
  max.weight := max(avg_weight_max),
  by = .(report_date, weight.bracket, market_location_city)
]
dt_interim_by.location[
  ,
  min.weight := max(avg_weight_min),
  by = .(report_date, weight.bracket, market_location_city)
]
dt_interim_by.location[
  ,
  total_head.count := sum(head_count, na.rm = TRUE),
  by = .(report_date, weight.bracket, market_location_city)
]
dt_interim_by.location[
  ,
  total_weight := sum(head_count * avg_weight, na.rm = TRUE),
  by = .(report_date, weight.bracket, market_location_city)
]
dt_interim_by.location[
  ,
  avg.weight := (sum(head_count * avg_weight, na.rm = TRUE) / total_head.count) %>% round(.),
  by = .(report_date, weight.bracket, market_location_city)
]
dt_interim_by.location[
  ,
  avg.price := (mean(avg_price, na.rm = TRUE)) %>% round(.),
  by = .(report_date, weight.bracket, market_location_city)
]
# # ## TODO: Simple mean? Or weighted mean?

cols_by <- c(
  "report_date", "weight.bracket", "market_location_city",
  "total_head.count", "total_weight", "avg.weight",
  "avg.price"
)
dt_for.plot_location <- dt_interim_by.location[
  , .N, by = cols_by
][
  , N := NULL
]
dt_for.plot_location_melted <- melt(
  dt_for.plot_location,
  id.vars = c("report_date", "weight.bracket", "market_location_city"),
  measure.vars = c("total_head.count", "total_weight", "avg.weight", "avg.price")
)
dt_for.plot_location_melted[
  str_detect(variable, "(weight$)|(count$)"),
  category := "Quantity"
]
dt_for.plot_location_melted[is.na(category), category := "Price"]


# dt_for.plot[report_date == as.Date("2018-01-08")]

plot_by.location <-
  ggplot() +
    geom_line(
      data = dt_for.plot_location_melted[!variable %in% c("total_head.count", "avg.weight") & weight.bracket == "(400,500]"],
      aes(x = report_date, y = value, group = market_location_city),
      color = "black", alpha = 0.25, lwd = 0.7
    ) +
    geom_line(
      data = dt_for.plot_location_melted[!variable %in% c("total_head.count", "avg.weight") & weight.bracket == "(400,500]"],
      aes(x = report_date, y = value, color = market_location_city),
      lwd = 0.3
    ) +
    geom_point(
      data = dt_for.plot_location_melted[!variable %in% c("total_head.count", "avg.weight") & weight.bracket == "(400,500]"],
      aes(x = report_date, y = value, color = market_location_city),
      color = "black", size = 1.3
    ) +
    geom_point(
      data = dt_for.plot_location_melted[!variable %in% c("total_head.count", "avg.weight") & weight.bracket == "(400,500]"],
      aes(x = report_date, y = value, color = market_location_city),
      size = 1
    ) +
    facet_wrap(category ~ ., scales = "free_y", ncol = 1) +
    scale_y_continuous(label = scales::comma) +
    # scale_color_brewer(palette = "Spectral") +
    scale_color_manual(values = unikn::usecol(pal = pal_signal)) +
    labs(
      x = "", y = "", color = "Markets",
      caption = "Note: Auction, Cattle-Heifers-Feeder Cattle, Frame: Medium and Large, Muscle Grade: 1, Weight Bracket: (400,500]"
    ) +
    theme_linedraw() +
    theme(strip.text = element_text(face = "bold"))


export_figure.in.png(
  plot_by.bracket,
  filename_str = paste(
    "/Users/jmjo/Dropbox/00_JMJo/Projects/My-Market-News/02_Data/03_Analysis/Sample-Figures",
    "Sample-Figures_Commodity-by-Weight-Bracket.png",
    sep = "/"
  ),
  width_numeric = 45, height_numeric = 25
)

export_figure.in.png(
  plot_by.location,
  filename_str = paste(
    "/Users/jmjo/Dropbox/00_JMJo/Projects/My-Market-News/02_Data/03_Analysis/Sample-Figures",
    "Sample-Figures_Commodity-by-Market-Location.png",
    sep = "/"
  ),
  width_numeric = 45, height_numeric = 25
)
