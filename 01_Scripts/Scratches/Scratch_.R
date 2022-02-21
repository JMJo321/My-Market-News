req <- create_req("https://marsapi.ams.usda.gov/services/v1.2/marketTypes/1000")
resp <- get_resp(req)
httr2::resp_body_string(resp)
# ## Note:
# ## Market Type 1000 에 대한 리포트 출력 => Market 등의 정보를 얻을 수 있다.

req <- create_req("https://marsapi.ams.usda.gov//services/v1.1/public/listPublishedReports/all")
resp <- get_resp(req)
httr2::resp_body_string(resp)
# ## Works!

req <- create_req("https://marsapi.ams.usda.gov//services/v1.2/public/listPublishedReports/all")
resp <- get_resp(req)
httr2::resp_body_string(resp)
# ## NOT Work!


req <- create_req("https://marsapi.ams.usda.gov/services/v1.2/commodities/corn")
resp <- get_resp(req)



req <- create_req("https://marsapi.ams.usda.gov/services/v1.2/reports")
req <- create_api.req("https://marsapi.ams.usda.gov/services/v1.2/reports/1785")
resp <- get_api.resp(req)
httr2::resp_body_json(resp) %>% length(.)
httr2::resp_body_json(resp) %>% .[[1]]
httr2::resp_body_json(resp) %>% .[[115]]
httr2::resp_body_string(resp)
httr2::resp_body_json(resp) %>% tidyjson::json_schema(.)
httr2::resp_body_json(resp) %>% tidyjson::spread_all(.) %>% .[115,]

library(tibble)
httr2::resp_body_string(resp)
httr2::resp_body_json(resp) %>% .$stats
httr2::resp_body_json(resp) %>% .$results %>% jsonlite::toJSON(., auto_unbox = TRUE) %>% jsonlite::fromJSON(.) %>% as_tibble(.)
httr2::resp_body_string(resp) %>% jsonlite::fromJSON(., flatten = FALSE) %>% as_tibble(.)
httr2::resp_body_string(resp) %>% jsonlite::fromJSON(., flatten = TRUE) %>% as_tibble(.)



help_get.report_only.for.a.specific.year(slug.names[98], 2020)
resp <- get_api.resp_report(slug.names[98], 2020)
httr2::resp_body_json(resp) %>% .$stats


list_resp <- as.list(NULL)
for (year in 1995:2021) {
  cat(paste0(year, "\n"))
  tmp_resp <- help_get.report_only.for.a.specific.year(slug.names[98], year)
  list_resp <- c(list_resp, tmp_resp)
}


list_resp <- as.list(NULL)
for (year in 1995:2021) {
  for (month in 1:12) {
    month_ <- str_pad(month, 2, side = "left", pad = "0")
    days <- lubridate::days_in_month(month)
    cat(paste(year, month_, sep = "-") %>% paste0(., "\n"))
    url <- paste0("https://marsapi.ams.usda.gov/services/v1.2/reports/AMS_1703?q=published_date=", month_, "/01/", year, ":", month_, "/", days, "/", year)
    req <-
      httr2::request(url) %>%
        httr2::req_auth_basic(., username = api_key, password = api_password)
    resp <- get_api.resp(req)
    list_resp <- c(list_resp, resp)
  }
}

req <-
  httr2::request("https://marsapi.ams.usda.gov/services/v1.2/reports/MD_DA199") %>%
    httr2::req_auth_basic(., username = api_key, password = api_password)
resp <- get_api.resp(req)
httr2::resp_body_json(resp) %>% .$stats


create_api.endpoint("AMS_1703")
resp <- get_api.resp_report("AMS_2707")
tbl <- transform_report_resp.to.tbl(resp)