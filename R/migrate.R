#' Summarize the migration of a data frame
#'
#' @description
#' `migrate()` summarizes the transition amount (or percentage) of a continuous variable
#' from each beginning credit risk rating category to each ending credit risk rating,
#' given a data frame input.
#'
#' @param data A data frame or data frame extension (e.g., a tibble or data.table)
#'   containing a minimum of four (4) column variables representing a date, a credit
#'   risk rating, a (continuous) metric, and an ID identifying the credit facility (we
#'   would expect to see most unique values in this column variable appear twice in the
#'   dataset; once at the first date and again at the second date, unless the credit
#'   only existed at one of those two dates).
#' @param date A symbol or string, representing the column variable of the `data` data
#'   frame argument that contains the two unique date values.
#' @param rating A symbol or string, representing the column variable of the `data` data
#'   frame argument that contains the credit risk rating values.
#' @param metric A symbol or string, representing the column variable of the `data` data
#'   frame argument that contains the continuous metric values.
#' @param percent If `TRUE`, will compute the percentage change in the continuous metric
#'   instead of just using the absolute difference.
#' @param method One of c("start", "end"). If "start" (this is the default), this will
#'   migrate the values in the `metric` column variable at the earlier date value from
#'   the `date` column variable. If "start" (this is the default), this will migrate the
#'   values in the `metric` column variable at the later date value from the `date`
#'   column variable.
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
#' # Return the absolute migration in `principal_balance`
#' mock_credit %>%
#'   migrate(
#'     date = date,
#'     rating = risk_rating,
#'     metric = principal_balance
#'   )
#'
#' # Return the percent migration in `principal_balance` while using the "end"
#' # method for which period's metric values to migrate
#' mock_credit %>%
#'   migrate(
#'     date = date,
#'     rating = risk_rating,
#'     metric = principal_balance,
#'     percent = TRUE,
#'     method = "end"
#'   )
#'
migrate <- function(data, date, rating, metric, percent = FALSE, method = "start") {

  # Coerce input dataframe to a tibble
  data <- data %>% tibble::as_tibble()

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
      "Converting",
      crayon::blue(rating_name),
      "to type `factor`.\nTo ensure that your output is ordered correctly, make the",
      crayon::blue(rating_name),
      "column variable in your data frame an ordered",
      "factor before passing it to `migrate()`."
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

  # Pivot the data from long to wide based upon the 'date' column variable
  data <- data %>%
    tidyr::pivot_wider(
      names_from = !! date_quo,
      values_from = c((!! rating_quo), (!! metric_quo))
    ) %>%
    tidyr::drop_na()   # remove any observations that only appeared at one date;
                       # we have no way of plotting them in a migration matrix
                       # since we either don't know what rating they're migrating
                       # to (in the case where it only appears at the earlier date)
                       # or from (in the case where it only appears at the later
                       # date)

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

  # If the `method` argument is not set to one of c("start", "end"), stop
  # the function and return a message
  if (!method %in% c("start", "end")) {

    stop("`method` argument must be set to either \"start\" or \"end\"")

  }

  # If `method` argument is set to "start" (this is the default), summarise
  # using the values for the metric column variable at the earlier date (i.e.,
  # value at the beginning of the migration period)
  if (method == "start") {

    data <- data %>%
      dplyr::summarise(
        !! metric_name := sum(!! metric_start_sym),
        .groups = "drop"
      )

  }

  # If `method` argument is set to "end", summarise using the values for the
  # metric column variable at the later date (i.e., value at the end of the
  # migration period)
  if (method == "end") {

    data <- data %>%
      dplyr::summarise(
        !! metric_name := sum(!! metric_end_sym),
        .groups = "drop"
      )

  }

  # If the user set `percent = TRUE` in function argument...
  if (percent == TRUE) {

    # ...compute the % of the metric column variable value for starting rating
    # class that ended up in the ending rating class
    data %>%
      dplyr::group_by(!! rating_start_sym) %>%
      dplyr::mutate(
        !! metric_name := !! dplyr::sym(metric_name) / sum(!! dplyr::sym(metric_name))
      ) %>%
      dplyr::ungroup() %>%
      # Replace `NaN` values with `Inf` so that they are not dropped with `drop_na()`
      dplyr::mutate(!! metric_name := ifelse(is.nan(!! dplyr::sym(metric_name)), Inf, !! dplyr::sym(metric_name))) %>%
      tidyr::drop_na()

    # otherwise, if `percent == FALSE` (this is the default) return the `data`
    # dataframe
  } else {

    data

  }

}
