#' Title
#'
#' @param data an R dataframe
#' @param date
#' @param rating
#' @param metric
#' @param id
#' @param include.new
#' @param exclude.old
#'
#' @return
#' @export
#'
#' @examples
migrate <- function(data, date, rating, metric, id = NULL, include.new = TRUE, exclude.old = FALSE) {

  # Coerce input dataframe to a tibble
  data <- data %>% tibble::as_tibble()

  if (include.new == FALSE | exclude.old == TRUE) {

    if (is.null(id)) {

      cat(
        emo::ji("x"),
        "If `include.new` = FALSE or `exclude.old` = TRUE, then `id` must be specified."
      ) %>% stop()

    }

    # Create variables for tidy evaluation
    id_quo <- dplyr::enquo(id)
    id_name <- dplyr::quo_name(id_quo)

  }

  # Create variables for tidy evaluation
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

  # Convert `rating` variable to type `factor`, if necessary
  if (!rating_factor_status) {

    # Print message to console letting user know that variable is being
    # converted to type `factor`
    cat(
      emo::ji("warning"),
      "Converting ",
      crayon::blue(rating_name),
      " to type `factor`."
    ) %>% message()

    data <- data %>%
      dplyr::mutate(rating_name := as.factor(!! rating_quo))

  }

  # Get the number of unique dates in the data
  num_dates <- data %>%
    dplyr::pull(!! date_quo) %>%
    unique() %>%
    length()

  # Stop execution if there aren't exactly 2 unique dates in the data
  if (num_dates != 2) {

    cat(
      emo::ji("x"),
      "Data must have exactly 2 unique values for ",
      crayon::blue(date_name),
      "; instead ",
      num_dates,
      " unique values were found."
    ) %>% stop()

  }

  # Capture the max date value
  max_date <- data %>%
    dplyr::pull(!! date_quo) %>%
    unique() %>%
    max()

  # Capture the min date value
  min_date <- data %>%
    dplyr::pull(!! date_quo) %>%
    unique() %>%
    max()


  # Exclude credit that didn't exist on the first date if user specifies in
  # `include.new` argument
  if (include.new == FALSE) {

    data <- data %>%
      dplyr::group_by(!! id_quo) %>%
      dplyr::filter(!(dplyr::n() < 2 && date == max_date)) %>%
      dplyr::ungroup()

  }

  # Exclude credit that didn't exist on the last date if user specifies in
  # `exclude.old` argument
  if (exclude.old == TRUE) {

    data <- data %>%
      dplyr::group_by(!! id_quo) %>%
      dplyr::filter(!(dplyr::n() < 2 && date == min_date)) %>%
      dplyr::ungroup()

  }

  # Create the output dataframe with the default arguments
  data %>%
    dplyr::group_by(
      !! date_quo,
      !! rating_quo,
      .drop = FALSE
    ) %>%
    dplyr::summarise(
      metric_name := sum(!! metric_quo),
      .groups = "drop"
    )

}
