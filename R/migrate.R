migrate <- function(data, date, rating, metric) {

  date_quo <- dplyr::enquo(date)
  date_name <- dplyr::quo_name(date_quo)

  rating_quo <- dplyr::enquo(rating)
  rating_name <- dplyr::quo_name(rating_quo)

  metric_quo <- dplyr::enquo(metric)
  metric_name <- dplyr::quo_name(metric_quo)

  rating_factor_status <- is.factor(
    data %>%
      dplyr::pull(
        !! rating_quo
      )
  )

  if (!rating_factor_status) {

    # Print message to console letting user know that variable is being
    # converted to type `factor`
    cat(
      emo::ji("warning"),
      "Converting ",
      crayon::blue(rating_name),
      " to type `factor`"
    ) %>% message()

    data <- data %>%
      dplyr::mutate(rating_name := as.factor(!! rating_quo))

  }

  data %>%
    dplyr::group_by(
      !! date_quo,
      !! rating_quo
    ) %>%
    dplyr::summarise(
      metric_name := sum(!! metric_quo),
      .groups = "drop"
    )

}
