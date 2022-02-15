library(huxtable)
library(tidyverse)
library(data.table)

load("/Users/jmjo/Dropbox/00_JMJo/Projects/My-Market-News/02_Data/02_Intermediate/MMN-API/Reports/MMN_Reports_2022-01-25.RData")



# -----
dt_summary_appended[, .N, keyby = .(category)]

# ----- Cattle
dt_summary_appended[category == "Cattle", .N, keyby = .(commodity, class)]

dt_summary_appended[category == "Cattle", .N, by = .(slug_name)][, .N]



get_count <- function (conditions_in.str, cols_in.str) {
  dt_summary_appended[
    eval(parse(text = conditions_in.str)),
    .N,
    by = cols_in.str
  ][
    ,
    .N
  ]
}


conditions_in.str <- "category == 'Cattle'"
sapply(
  names(dt_summary_appended), get_count, conditions_in.str = conditions_in.str
)


cols_to.examine <- c(
  "slug_name",
  "commodity",
  "class",
  "market_type",
  "market_type_category",
  "office_name"
)
dt_summary_cattle <- sapply(cols_to.examine, get_count, conditions_in.str = conditions_in.str) %>% data.table(.) %>% transpose(.)
names(dt_summary_cattle) <- cols_to.examine
dt_summary_cattle


condition_cattle <- "category == 'Cattle' & market_type_category == 'Auction'"
cattle_auction_N <-
  dt_summary_appended[
    eval(parse(text = condition_cattle)), .N, by = .(slug_name)
  ][
    , .N
  ]
cattle_auction_category <- "Cattle"
cattle_auction_market.type.category <- "Auction"
cattle_auction_commodity <- dt_summary_appended[eval(parse(text = condition_cattle)), .N, keyby = .(commodity)]$commodity
cattle_auction_class <- dt_summary_appended[eval(parse(text = condition_cattle)), .N, keyby = .(class)]$class
cattle_auction_market.type <- dt_summary_appended[eval(parse(text = condition_cattle)), .N, keyby = .(market_type)]$market_type
cattle_auction_office.name <- dt_summary_appended[eval(parse(text = condition_cattle)), .N, keyby = .(office_name)]$office_name

cattle_auction_cols <- c(
  "cattle_auction_category",
  "cattle_auction_market.type.category",
  "cattle_auction_N",
  "cattle_auction_commodity",
  "cattle_auction_class",
  "cattle_auction_market.type",
  "cattle_auction_office.name"
)
cattle_auction_target.length <-
  lapply(mget(cattle_auction_cols), length) %>% unlist(.) %>% max(.)

help_make.column <- function (vector, target.length) {
  vector_modified <- c(vector, rep("", target.length - length(vector)))
  return (vector_modified)
}

dt_cattle_auction <-
  lapply(
    mget(cattle_auction_cols),
    help_make.column,
    target.length = cattle_auction_target.length
  ) %>%
    as.data.table(.)


ht_cattle_auction <- huxtable(dt_cattle_auction)
contents(ht_cattle_auction)[1, 1:7] <- c("Category", "Market Type Category", "# of Reports", "Commodity", "Class", "Market Type", "Office")
bold(ht_cattle_auction)[1,] <- TRUE
align(ht_cattle_auction)[1,] <- "center"
col_width(ht_cattle_auction)[c(1,3)] <- 10
col_width(ht_cattle_auction)[c(2, 4, 5)] <- 30
col_width(ht_cattle_auction)[6] <- 35
col_width(ht_cattle_auction)[7] <- 15
top_border(ht_cattle_auction)[1,] <- brdr(thickness = 1)
bottom_border(ht_cattle_auction)[c(1,35),] <- brdr(thickness = 1)
right_border(ht_cattle_auction)[, c(1:6)] <- brdr(thickness = 1)
ht_cattle_auction

# to_md(ht_cattle_auction)
# quick_pdf(ht_cattle_auction, file = "test.pdf")
# quick_latex(ht_cattle_auction, file = "test.tex")
# quick_xlsx(ht_cattle_auction, file = "test.xlsx")


# -----
dt_summary_appended[
  , .N, by = .(slug_name, category)
][
  , .N, keyby = .(category)
]

# ## To examine reports with `is.na(category) == TRUE`
dt_summary_appended[is.na(category), .N, by = .(slug_name)][, .N]
dt_summary_appended[is.na(category) & is.na(report_title), .N, by = .(slug_name)][, .N]
# ## Note:
# ## Reports without data
dt_summary_appended[
  is.na(category) & !is.na(report_title), .N, by = .(slug_name)
][
  , .N
]


reg.exp_truck.rate <- "(Truck\\sRate)"
reg.exp_grain <- "(Grain)|(grain)|(Rice)|(Bean)|(Wheat)"
reg.exp_livestock <- "(Livestock)|(Cattle)|(Pig)|(Sheep)|(Cow)"
reg.exp_hay <- "(Hay)"
reg.exp_ornamental <- "(Ornamental)"
reg.exp_sunflower <- "(Sunflower)"
reg.exp_export <- "(Export)"
reg.exp_shipping <- "(Shipping)"
reg.exp_etc <- "(Internet)|(Video)"
reg.exp <- paste(
  reg.exp_truck.rate,
  reg.exp_grain,
  reg.exp_livestock,
  reg.exp_hay,
  reg.exp_ornamental,
  reg.exp_sunflower,
  reg.exp_export,
  reg.exp_shipping,
  reg.exp_etc,
  sep = "|"
)
dt_summary_appended[
  is.na(category) & !is.na(report_title) &
    str_detect(report_title, reg.exp, negate = TRUE),
  .N,
  by = .(slug_name, report_title)
]

reg.exps <- c(
  reg.exp_truck.rate, reg.exp_grain, reg.exp_livestock, reg.exp_hay,
  reg.exp_ornamental, reg.exp_sunflower, reg.exp_export, reg.exp_shipping,
  reg.exp_etc
)
names(reg.exps) <- c(
  "Truck Rate", "Grain", "Livestock", "Hay",
  "Ornamental", "Sunflower", "Export", "Shipping",
  "Etc"
)
count_by.reg.exp <- lapply(
  reg.exps,
  function (x) {
    dt_summary_appended[
      is.na(category) & !is.na(report_title) &
        str_detect(report_title, x),
      .N,
      by = .(slug_name)
    ][
      ,
      .N
    ]
  }
)
count_by.reg.exp %>% unlist(.) %>% sum(.)

dt_summary_appended[
  is.na(category) & !is.na(report_title), .N, by = .(slug_name)
][
  , .N
]
dt_summary_appended[
  is.na(category) & !is.na(report_title) &
    str_detect(report_title, reg.exp_livestock),
  .N,
  by = .(slug_name)
][, .N]
dt_summary_appended[
  is.na(category) & !is.na(report_title) &
    str_detect(report_title, reg.exp_livestock) & str_detect(report_title, reg.exp_etc),
  .N,
  by = .(slug_name)
][, .N]
dt_summary_appended[
  is.na(category) & !is.na(report_title) &
    str_detect(report_title, reg.exp_etc),
  .N,
  by = .(slug_name)
][, .N]


dt_summary_appended[str_detect(report_title, "Truck\\sRate"), .N, keyby = .(commodity)][, .N]
