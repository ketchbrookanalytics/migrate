


build_matrix <- function(data, rating, metric, method = "dataframe") {

  rating_quo <- dplyr::enquo(rating)
  rating_name <- dplyr::quo_name(rating_quo)

  metric_quo <- dplyr::enquo(metric)
  metric_name <- dplyr::quo_name(metric_quo)

  names <- data %>%
    dplyr::pull(!! rating_quo) %>%
    unique() %>%
    sort()

  vals <- data %>%
    dplyr::pull(!! metric_quo)

  matrix(
    vals,
    nrow = length(names),
    ncol = length(names),
    byrow = TRUE,
    dimnames = list(
      names,
      names
    )
  )

}
