#' Summarize the migration of a data frame
#'
#' @description
#' `migrate()` summarizes the transition amount (or percentage) of a continuous variable
#' from each beginning credit risk state category to each ending credit risk state,
#' given a data frame input.
#'
#' @param data A data frame or data frame extension (e.g., a tibble or data.table)
#'   containing a minimum of three (3) column variables representing a date, a credit
#'   risk state, and an ID identifying the credit facility (we would expect to see most
#'   unique values in this column variable appear twice in the dataset; once at the
#'   first date and again at the second date, unless the credit only existed at one of
#'   those two dates).
#' @param date A symbol or string, representing the column variable of the `data` data
#'   frame argument that contains the two unique date values.
#' @param state A symbol or string, representing the column variable of the `data` data
#'   frame argument that contains the credit risk state values.
#' @param id (Optional) a symbol or string, representing the column variable of the
#'   `data` data frame argument that contains the unique identifier to track where a
#'   particular credit facility migrated to/from. If left null, `migrate()` will attempt
#'   to use the first column variable from the data frame provided in the `data` argument.
#' @param metric (Optional) a symbol or string, representing the column variable of the
#'   `data` data frame argument that contains the continuous metric values.
#' @param percent If `TRUE`, will calculate the migration on a percentage basis (rather
#'   than an absolute basis, which is the default).
#' @param method One of c("start", "end"). If "start" (this is the default), this will
#'   migrate the values in the `metric` column variable at the earlier date value from
#'   the `date` column variable. If "start" (this is the default), this will migrate the
#'   values in the `metric` column variable at the later date value from the `date`
#'   column variable. If the `metric` column variable is not defined (i.e., `migrate()`
#'   returns the count of credit facilities that migrated), this argument does nothing.
#' @param rating Deprecated; please use `state` instead.
#'
#' @return
#' A data frame containing three (3) column variables representing the unique
#' combinations of starting & ending credit risk states and the calculated migration
#' observed during the period.
#'
#' @importFrom rlang :=
#'
#' @export
#'
#' @examples
#' # Return the absolute migration of the number of credit facilities
#' migrate(
#'   data = mock_credit,
#'   date = date,
#'   state = risk_rating,
#'   id = customer_id
#' )
#'
#' # Return the percent migration in `principal_balance` while using the "end"
#' # method for which period's metric values to migrate
#' migrate(
#'   data = mock_credit,
#'   date = date,
#'   rating = risk_rating,
#'   id = customer_id,
#'   metric = principal_balance,
#'   percent = TRUE
#' )
#'
migrate <- function(data, date, state, id = NULL, metric = NULL, percent = FALSE, method = "start", rating = NULL) {

  # Coerce input data frame to a tibble
  data <- data %>% tibble::as_tibble()

  # Create variables for tidy evaluation
  date_quo <- dplyr::enquo(date)
  date_name <- dplyr::quo_name(date_quo)

  # If `rating` argument is supplied...
  if (!missing(rating)) {

    # ... warn user that it is deprecated
    warning(
      "argument `rating` is deprecated; please use `state` instead.",
      call. = FALSE
    )

    # ... and create 'state_quo' object
    state_quo <- dplyr::enquo(rating)

  } else {

    # otherwise use the `state` argument to create 'state_quo'
    state_quo <- dplyr::enquo(state)

  }

  state_name <- dplyr::quo_name(state_quo)

  # If the `id` argument is missing, try using the first column
  # variable from the data frame
  if (missing(id)) {

    cat(
      "Using first column",
      crayon::blue(colnames(data)[1]),
      "as `id` column; set this with the `id` argument."
    ) %>% message()

    if (is.numeric(data[,1])) {

      cat(
      "`id` column",
      crayon::blue(colnames(data)[1]),
      "shouldn't be numeric."
      ) %>% stop()

    }

    id_quo <- colnames(data)[1] %>% dplyr::sym()


  } else {

    id_quo <- dplyr::enquo(id)

  }

  id_name <- dplyr::quo_name(id_quo)

  # If the `metric` argument is left null, create a new quoted object
  # called 'count'
  if (missing(metric)) {

    metric_quo <- dplyr::sym("count")

  } else {

    metric_quo <- dplyr::enquo(metric)

  }

  metric_name <- dplyr::quo_name(metric_quo)

  # Capture whether or not the `state` variable in the 'data' data frame
  # is type "factor"
  state_factor_status <- is.factor(
    data %>%
      dplyr::pull(!! state_quo)
  )

  # Convert `state` variable to type `factor`, if necessary
  if (!state_factor_status) {

    # Print message to console letting user know that variable is being
    # converted to type `factor`
    cat(
      "Converting",
      crayon::blue(state_name),
      "to type `factor`.\nTo ensure that your output is ordered correctly, make the",
      crayon::blue(state_name),
      "column variable in your data frame an ordered",
      "factor before passing it to `migrate()`."
    ) %>% message()

    data <- data %>%
      dplyr::mutate(state_name := as.factor(!! state_quo))

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

  # If the user left the `metric` argument NULL, add it as a column variable
  # to the dataframe
  if (missing(metric)) {

    data <- data %>%
      dplyr::mutate(!! metric_name := 1)

  }

  # Pivot the data from long to wide based upon the 'date' column variable
  data <- data %>%
    tidyr::pivot_wider(
      id_cols = !! id_quo,
      names_from = !! date_quo,
      values_from = c((!! state_quo), (!! metric_quo))
    ) %>%
    tidyr::drop_na()   # remove any observations that only appeared at one date;
                       # we have no way of plotting them in a migration matrix
                       # since we either don't know what state they're migrating
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
  state_start_sym <- paste0(state_name, "_start") %>% dplyr::sym()
  state_end_sym <- paste0(state_name, "_end") %>% dplyr::sym()

  metric_start_sym <- paste0(metric_name, "_start") %>% dplyr::sym()
  metric_end_sym <- paste0(metric_name, "_end") %>% dplyr::sym()

  # Group the data by the state variables, preserving all factor levels
  data <- data %>%
    dplyr::group_by(
      !! state_start_sym,
      !! state_end_sym,
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

    # ...compute the % of the metric column variable value for starting state
    # class that ended up in the ending state class
    data %>%
      dplyr::group_by(!! state_start_sym) %>%
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
