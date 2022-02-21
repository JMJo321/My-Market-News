help_drop.na.columns <- function (values_list) {
  return (values_list[!is.na(values_list)])
}

get_subset.condition_in.str <- function (values_list) {
  list_values_na.drop <- help_drop.na.columns(values_list)
  columns <- names(list_values_na.drop)
  values <- unlist(list_values_na.drop)
  tmp_str <- NULL
  for (col in columns) {
    if (is.na(values[col])) {
      tmp_str <- c(tmp_str, paste0("is.na(", col, ") == TRUE"))
    } else {
      tmp_str <- c(tmp_str, paste0(col, " == '", values[col], "'"))
    }
  }
  subset.condition <- str_c(tmp_str, collapse = " & ")
  return (subset.condition)
}

get_unique.values.in.target.column <-
  function (values_list, target.column_str) {
    subset.condition <-
      get_subset.condition_in.str(
        values_list[!names(values_list) %in% target.column_str]
      )
    unique.values <- dt_cattle[
      eval(parse(text = subset.condition)), .N, keyby = target.column_str
    ][
      , .SD, .SDcols = target.column_str
    ] %>%
      unlist(., use.names = FALSE)
    return (unique.values)
}
