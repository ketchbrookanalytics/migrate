#' Summarize the migration of a data frame
#'
#' @description
#' `migrate()` summarizes the transition amount (or percentage) of a continuous variable
#' from each beginning credit risk rating category to each ending credit risk rating,
#' given a data frame input.
#'
#' @param data A data frame or data frame extension (e.g., a tibble or data.table)
#'   containing a minimum of three (3) column variables representing a date, a credit
#'   risk rating, and a (continuous) metric.
#' @param date A symbol or string, representing the column variable of the `data` data
#'   frame argument that contains the two unique date values.
#' @param rating A symbol or string, representing the column variable of the `data` data
#'   frame argument that contains the credit risk rating values.
#' @param metric A symbol or string, representing the column variable of the `data` data
#'   frame argument that contains the continuous metric values.
#' @param percent If `TRUE`, will compute the percentage change in the continuous metric
#'   instead of just using the absolute difference.
#' @param id (Required if `include.new = FALSE` or `exclude.old = TRUE`) A symbol or
#'   string, representing the column variable of the `data` data frame argument that
#'   contains the ID values (e.g., "Customer ID", "Loan ID", etc.).
#' @param include.new If `FALSE`, will remove ID's that only have an observation for the
#'   later date.
#' @param exclude.old If `TRUE`, will remove ID's that only have an observation for the
#'   earlier date.
#'
#' @return
#' A data frame containing three (3) column variables representing the unique
#' combinations of starting & ending credit risk ratings and the migrated difference in
#' the continuous metric.
#'
#' @importFrom rlang :=
#'
#' @export
#'
#' @examples
#' # Return the absolute difference in `principal_balance`
#' mock_credit %>%
#'   migrate(
#'     date = date,
#'     rating = risk_rating,
#'     metric = principal_balance
#'   )
#'
#' # Return the percent difference in `principal_balance` while excluding "new"
#' # `customer_id` values
#' mock_credit %>%
#'   migrate(
#'     date = date,
#'     rating = risk_rating,
#'     metric = principal_balance,
#'     percent = TRUE,
#'     id = customer_id,
#'     include.new = FALSE
#'   )
#'
migrate <- function(data, date, rating, metric, percent = FALSE, id = NULL, include.new = TRUE, exclude.old = FALSE) {

  # Coerce input dataframe to a tibble
  data <- data %>% tibble::as_tibble()

  # Require the `id` argument be completed if either `include.new = FALSE`
  # or `exclude.old = TRUE`
  if (include.new == FALSE | exclude.old == TRUE) {

    if (missing(id)) {

      cat(
        emo::ji("x"),
        "Error: If `include.new` = FALSE or `exclude.old` = TRUE, then `id` must be specified."
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

  # Capture whether or not the `rating` variable in the 'data' dataframe
  # is type "factor"
  rating_factor_status <- is.factor(
    data %>%
      dplyr::pull(!! rating_quo)
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
      "Error: Data must have exactly 2 unique values for ",
      crayon::blue(date_name),
      "; instead ",
      num_dates,
      " unique values were found."
    ) %>% stop()

  }

  # Capture the max date value
  min_date <- data %>%
    dplyr::pull(!! date_quo) %>%
    unique() %>%
    min()

  # Capture the min date value
  max_date <- data %>%
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

  # Pivot the data from long to wide based upon the 'date' column variable
  data <- data %>%
    tidyr::pivot_wider(
      names_from = !! date_quo,
      values_from = c((!! rating_quo), (!! metric_quo))
    )

  # Create a character vector of new column names to apply to the 'data' dataframe,
  # replacing the date values in the column names with "start" and "end"
  col_names <- colnames(data) %>%
    stringr::str_replace(
      pattern = as.character(min_date),
      replacement = "start"
    ) %>%
    stringr::str_replace(
      pattern = as.character(max_date),
      replacement = "end"
    )

  # Overwrite the old column names with the new column names
  colnames(data) <- col_names

  # Quote the new column names for use in tidy evaluation
  rating_start_sym <- paste0(rating_name, "_start") %>% dplyr::sym()
  rating_end_sym <- paste0(rating_name, "_end") %>% dplyr::sym()

  metric_start_sym <- paste0(metric_name, "_start") %>% dplyr::sym()
  metric_end_sym <- paste0(metric_name, "_end") %>% dplyr::sym()

  # Group the data by the rating variables, preserving all factor levels
  data <- data %>%
    dplyr::group_by(
      !! rating_start_sym,
      !! rating_end_sym,
      .drop = FALSE
    )

  # If the user set `percent = TRUE` in function argument...
  if (percent == TRUE) {

    # ...compute the % change between the starting & ending summed values in the
    # metric variable across each group in the 'data' dataframe
    data %>%
      dplyr::summarise(
        !! metric_name := (sum(!! metric_end_sym) - sum(!! metric_start_sym)) / sum(!! metric_start_sym),
        .groups = "drop"
      ) %>%
      # Replace `NaN` values with `Inf` so that they are not dropped with `drop_na()`
      dplyr::mutate(!! metric_name := ifelse(is.nan(!! dplyr::sym(metric_name)), Inf, !! dplyr::sym(metric_name))) %>%
      tidyr::drop_na()

    # If `percent = FALSE`...
  } else {

    # ...compute the straight change between the starting & ending summed values
    # in the metric variable across each group in the 'data' dataframe
    data %>%
      dplyr::summarise(
        !! metric_name := sum(!! metric_end_sym) - sum(!! metric_start_sym),
        .groups = "drop"
      ) %>%
      tidyr::drop_na()

  }


}
