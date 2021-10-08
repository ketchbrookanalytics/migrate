#' Build a migration (transition) matrix
#'
#' @description
#' `build_matrix()` creates a credit migration (i.e., "transition") matrix from
#'   summarized data representing each credit risk state & a continuous metric at
#'   two distinct points in time.
#'
#' @param data A data frame or data frame extension (e.g., a tibble or data.table)
#'   containing a minimum of three (3) column variables representing a starting
#'   credit risk state, an ending credit risk state, and a metric containing
#'   values representing the movement (i.e., "transition) in that metric between
#'   the starting credit risk state point in time and the ending credit risk
#'   state point in time. This style of data frame is output by the `migrate()`
#'   function within this package.
#' @param state_start (Optional) A symbol or string, representing the column
#'   variable of the `data` data frame argument that contains the starting credit
#'   risk state values. If left null, function will attempt to find it for you.
#' @param state_end (Optional) A symbol or string, representing the column
#'   variable of the `data` data frame argument that contains the starting credit
#'   risk state values. If left null, function will attempt to find it for you.
#' @param metric (Optional) A symbol or string, representing the column variable
#'   of the `data` data frame argument that contains the metric for which the
#'   grouped difference in value between the starting credit risk state period
#'   and ending credit risk state period was computed.
#'
#' @return
#' A matrix object, where the first (row) dimension represents the starting
#' credit risk state, the second (column) dimension represents the ending credit
#' risk state, and the values within the matrix represent the transitioned
#' amount based upon the values in the `metric` continuous column variable from
#' the `data` data frame.
#'
#' Note: A matrix object can be coerced to a data frame using `as.data.frame()`.
#'
#' @export
#'
#' @examples
#' # Let `build_matrix()` guess which column variables represent `state_start`,
#' # `state_end` and `metric`
#' mock_credit %>%
#'   migrate(
#'     date = date,
#'     state = risk_rating,
#'     id = customer_id,
#'     metric = principal_balance
#'   ) %>%
#'   build_matrix()
#'
#' # Specify which column variables represent `state_start`, `state_end` and
#' # `metric`
#' mock_credit %>%
#'   migrate(
#'     date = date,
#'     state = risk_rating,
#'     id = customer_id,
#'     percent = FALSE
#'   ) %>%
#'   build_matrix(
#'     state_start = risk_rating_start,
#'     state_end = risk_rating_end,
#'     metric = count
#'   )
build_matrix <- function(data, state_start = NULL, state_end = NULL,
                         metric = NULL) {

  # Guess the `state_start` column if not specified in args
  if (missing(state_start)) {

    # Capture the columns in the `data` dataframe that are type `factor` and the
    # phrase "start" is in the column name
    state_start_col <- data %>%
      dplyr::select(where(is.factor) & contains("start"))

    # Ensure there is exactly 1 column in `state_start_col`
    if (ncol(state_start_col) > 1) {

      paste0(
        "Multiple columns of type `factor` with the phrase \"start\" in the ",
        "column variable name were found in `data`"
      ) %>%
        rlang::abort()

    }

    if (ncol(state_start_col) == 0) {

      paste0(
        "No columns of type `factor` with the phrase \"start\" in the column ",
        "variable name were found"
      ) %>%
        rlang::abort()

    }

    # If exactly 1 column of type `factor` with the phrase "start" in the column
    # name was found, use it as the `state_start_sym`
    paste0(
      "Using `",
      colnames(state_start_col),
      "` as the \'state_start\' column variable"
    ) %>%
      rlang::inform()

    state_start_sym <- colnames(state_start_col) %>%
      rlang::sym()

  } else {

    # If `state_start` argument was supplied, quote it for consistency with
    # object name above
    state_start_sym <- rlang::enquo(state_start)

  }


  # Guess the `state_end` column if not specified in args
  if (missing(state_end)) {

    # Capture the columns in the `data` data frame that are type `factor` and
    # the phrase "end" is in the column name
    state_end_col <- data %>%
      dplyr::select(where(is.factor) & contains("end"))

    # Ensure there is exactly 1 column in `state_end_col`
    if (ncol(state_end_col) > 1) {

      paste0(
        "Multiple columns of type `factor` with the phrase \"end\" in the ",
        "column variable name were found in `data`"
      ) %>%
        rlang::abort()

    }

    if (ncol(state_end_col) == 0) {

      paste0(
        "No columns of type `factor` with the phrase \"end\" in the column ",
        "variable name were found"
      ) %>%
        rlang::abort()

    }

    # If exactly 1 column of type `factor` with the phrase "end" in the column
    # name was found, use it as the `state_end_sym`
    paste0(
      "Using `",
      colnames(state_end_col),
      "` as the \'state_end\' column variable"
    ) %>%
      rlang::inform()

    state_end_sym <- colnames(state_end_col) %>%
      rlang::sym()

  } else {

    # If `state_end` argument was supplied, quote it for consistency with
    # object name above
    state_end_sym <- rlang::enquo(state_end)

  }


  # Guess the `metric` column if not specified in args
  if (missing(metric)) {

    # Capture the columns in the `data` data frame that are type `numeric`
    metric_col <- data %>%
      dplyr::select(where(is.numeric))

    # Throw error if there are multiple columns matching the above criteria
    if (ncol(metric_col) > 1) {

        rlang::abort("Multiple columns of type `numeric` were found in `data`")

    }

    # Throw error if there are no columns matching the above criteria
    if (ncol(metric_col) == 0) {

      rlang::abort("No columns of type `numeric` were found in `data`")

    }

    # If exactly 1 column of type `numeric`was found, use it as the `metric_sym`
    paste0(
      "Using `",
      colnames(metric_col),
      "` as the \'metric\' column variable"
    ) %>%
      rlang::inform()

    metric_sym <- colnames(metric_col) %>%
      rlang::sym()

  } else {

    # If `state_end` argument was supplied, quote it for consistency with
    # object name above
    metric_sym <- rlang::enquo(metric)

  }

  # Capture the row names for the matrix
  row_names <- data %>%
    dplyr::pull({{ state_start_sym }}) %>%
    unique() %>%
    sort()

  # Capture the column names for the matrix
  col_names <- data %>%
    dplyr::pull({{ state_end_sym }}) %>%
    unique() %>%
    sort()

  # Capture the values to fill in the matrix with
  vals <- data %>%
    dplyr::pull({{ metric_sym }})

  # Replace any infinite values with NA values
  vals <- ifelse(is.infinite(vals), NA, vals)

  # Output the migration matrix
  matrix(
    vals,
    nrow = length(row_names),
    ncol = length(col_names),
    byrow = TRUE,
    dimnames = list(
      row_names,
      col_names
    )
  )

}
