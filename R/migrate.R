


# Validate `migrate()` arguments
check_args <- function(data, percent, fill_state, verbose) {

  # Make sure the 'data' argument is a valid data frame
  if (!is.data.frame(data)) {

    rlang::abort(
      paste0(
        "`data` argument must be a valid data frame, not an object of type:",
        class(data)
      )
    )

  }

  # Ensure the supplied `percent` argument is logical
  if (!is.logical(percent)) {

    rlang::abort("`percent` argument must be logical TRUE/FALSE")

  }

  # Ensure the supplied `fill_state` argument is logical
  if (!is.null(fill_state)) {

    bad_length <- length(fill_state) != 1L

    bad_type <- !any(
      is.character(fill_state),
      is.factor(fill_state),
      is.numeric(fill_state)
    )

    if (any(bad_length, bad_type)) {

      paste0(
        "`fill_state` argument must be length-one value (of type `character`,",
        " `numeric`, or `factor`)"
      ) %>%
        rlang::abort()

    }

  }

  # Ensure the supplied `verbose` argument is logical
  if (!is.logical(verbose)) {

    rlang::abort("`verbose` argument must be logical TRUE/FALSE")

  }

}



# Convert the `state_name` column to type 'factor', if necessary
coerce_factor <- function(data, state_name) {

  # Capture the values in the `state_name` column as a vector
  state_vec <- data[[state_name]]

  # If the `state` column variable in the `data` data frame is type
  # "factor", capture whether or not it is ordered
  if (is.factor(state_vec)) {

    # If the `state` column variable in the `data` data frame is an
    # un-ordered factor, print a message to the console asking the user
    # to convert it to an ordered factor
    if (!is.ordered(state_vec)) {

      paste0(
        "Please consider converting `",
        state_name,
        "` to an ordered factor before passing it to `migrate()` to ensure ",
        "that the rank-ordering in the final matrix displays correctly"
      ) %>% rlang::warn()

    }

    # If the `state` column variable in the `data` data frame is *not* type
    # "factor", coerce it to type "factor"
  } else {

    # Print messages to console letting user know that variable is being
    # converted to type `factor`
    paste0(
      "Converting `",
      state_name,
      "` to type `factor`"
    ) %>% rlang::warn()

    paste0(
      "To ensure that your output is ordered correctly, convert the `",
      state_name,
      "` column variable in your data frame to an ordered factor before ",
      " passing to `migrate()`"
    ) %>% rlang::warn()

    data <- data %>%
      dplyr::mutate(state_name := as.factor({{ state }}))

  }

  return(data)

}



# Stop execution if there aren't exactly 2 unique time values in the data
check_times <- function(times, time_name) {

  if (length(times) != 2) {

    paste0(
      "There must be exactly 2 unique values in the `",
      time_name,
      "` column variable; ",
      length(times),
      " unique values were found"
    ) %>%
      rlang::abort()

  }

}



check_metric <- function(data, metric, percent) {

  browser()

  # If the `metric` argument is supplied...
  if (!is.null(metric)) {

    # Capture the name of the `metric` argument as a character string
    metric_name <- rlang::enquo(metric) %>%
      rlang::as_label()

    # Stop if the `metric` variable doesn't exist in the `data` data frame,
    if (!metric_name %in% colnames(data)) {

      rlang::abort("`metric` argument must be an unquoted variable in `data`")

    }

    # Stop if the `metric` variable isn't numeric
    if (!is.numeric(data[[metric_name]])) {

      rlang::abort("`metric` argument must be a numeric type variable in `data`")

    }

    metric_fill <- 0

    # ...if the `metric` argument is not supplied
  } else {

    # Set the `metric_name` character string based upon if `percent == TRUE`
    metric_name <- ifelse(percent, "prop", "count")

    # Add a new column to the `data` data frame, where every value is 1
    # This will help us show the transition on a count or percentage basis later
    data[[metric_name]] <- 1L

    metric_fill <- 1L

  }

  return(
    list(
      data = data,
      metric_name = metric_name,
      metric_fill = metric_fill
    )
  )

}



time_message <- function(times) {

  paste0(
    "=== Migrating from: `",
    times[1],
    "` --> `",
    times[2],
    "` ==="
  ) %>%
    rlang::inform()

}



#' Summarize the migration of a data frame
#'
#' @description
#' `migrate()` summarizes the transition amount (or percentage) of a continuous
#' variable from each beginning credit risk state category to each ending credit
#' risk state, given a data frame input.
#'
#' @param data A data frame or data frame extension (e.g., a tibble or
#'   data.table) containing a minimum of three (3) column variables representing
#'   a time, a credit risk state, and an ID identifying the credit facility (we
#'   would expect to see most unique values in this column variable appear twice
#'   in the dataset; once at the first unique `time` value and again at the
#'   second unique `time` value, unless the ID only existed at one of those two
#'   times).
#' @param time The column variable of in the `data` data frame representing the
#'   time point (e.g., a Date) of each observation; this column should contain
#'   two unique values (migration from Time A to Time B)
#' @param state The column variable of the `data` data frame argument that
#'   contains the credit risk state values.
#' @param id The column variable of the `data` data frame argument that contains
#'   the unique identifier to track where a particular credit facility migrated
#'   to/from. If left null, `migrate()` will attempt to use the first column
#'   variable from the data frame provided in the `data` argument.
#' @param metric (Optional) The column variable of type "numeric" in the `data`
#'   data frame argument that contains the continuous metric values to weight
#'   the state migration by
#' @param percent If `FALSE`, will calculate the migration on an absolute basis
#'   (rather than a percentage basis, which is the default)
#' @param verbose If `TRUE`, the function returns an informational message about
#'   the transition period
#'
#' @return
#' A data frame containing three (3) column variables representing the unique
#' combinations of starting & ending credit risk states and the calculated migration
#' observed during the period.
#'
#' @importFrom rlang := .data
#'
#' @export
#'
#' @examples
#' # Return the percent migration of the number of credit facilities
#' migrate(
#'   data = mock_credit,
#'   id = customer_id,
#'   time = date,
#'   state = risk_rating
#' )
#'
#' # Return the absolute migration in `principal_balance`
#' migrate(
#'   data = mock_credit,
#'   id = customer_id,
#'   time = date,
#'   state = risk_rating,
#'   metric = principal_balance,
#'   percent = FALSE
#' )
#'
migrate <- function(data, id, time, state,
                    metric = NULL, percent = TRUE, fill_state = NULL,
                    verbose = TRUE) {

  # Validate function arguments
  check_args(data, percent, fill_state, verbose)

  # Coerce input data frame to a tibble
  data <- data %>%
    tibble::as_tibble()

  # Capture supplied  variable arguments as strings
  id_name <- rlang::enquo(id) %>%
    rlang::as_label()

  time_name <- rlang::enquo(time) %>%
    rlang::as_label()

  state_name <- rlang::enquo(state) %>%
    rlang::as_label()

  # Coerce the `state` column variable to type "factor" (if necessary)
  data <- coerce_factor(data, state_name)

  # Capture the distinct `time` values, sorted ascending
  times <- data[[time_name]] %>%
    unique() %>%
    sort()

  # Stop execution if there aren't exactly 2 unique time values in the data
  check_times(times, time_name)

  # If the `metric` argument is supplied...
  if (!missing(metric)) {

    # Capture the name of the `metric` argument as a character string
    metric_name <- rlang::enquo(metric) %>%
      rlang::as_label()

    # Stop if the `metric` variable doesn't exist in the `data` data frame,
    if (!metric_name %in% colnames(data)) {

      rlang::abort("`metric` argument must be an unquoted variable in `data`")

    }

    # Stop if the `metric` variable isn't numeric
    if (!is.numeric(data[[metric_name]])) {

      rlang::abort("`metric` argument must be a numeric type variable in `data`")

    }

    metric_fill <- 0

    # ...if the `metric` argument is not supplied
  } else {

    # Set the `metric_name` character string based upon if `percent == TRUE`
    metric_name <- ifelse(percent, "prop", "count")

    # Add a new column to the `data` data frame, where every value is 1
    # This will help us show the transition on a count or percentage basis later
    data[[metric_name]] <- 1L

    metric_fill <- 1L

  }

  # Inform the user of the migration time period, unless `verbose = FALSE`
  if (verbose) { time_message(times) }

  # If the user supplied a 'fill_state' value...
  if (!is.null(fill_state)) {

    # ... add the fill state to the factor levels (if it doesn't already exist)
    if (!fill_state %in% levels(data[[state_name]])) {

      data <- data %>%
        dplyr::mutate(
          "{state_name}" := factor(
            x = {{ state }},
            levels = c(
              levels(data[[state_name]]),
              as.character(fill_state)
            ),
            ordered = TRUE
          )
        )

    }

    # ... create the named list to pass to the `values_fill` argument of
    # `tidyr::pivot_wider()`
    fill_state <- list(
      as.character(fill_state),
      metric_fill
    )

    names(fill_state) <- c(state_name, metric_name)

  }

  # Pivot the data from long to wide based upon the 'time' column variable
  data <- data %>%
    tidyr::pivot_wider(
      id_cols = {{ id }},
      names_from = {{ time }},
      values_from = dplyr::all_of(
        c(state_name, metric_name)
      ),
      values_fill = fill_state
    )

  # Remove any NA values across all columns; this will drop observations found
  # at only a single time point, unless the `fill_state` argument is not NULL
  if (nrow(tidyr::drop_na(data)) < nrow(data)) {

    paste0(
      "Removed ",
      (nrow(data) - nrow(tidyr::drop_na(data))),
      " observations due to missingness or IDs only existing at one `time` ",
      "value"
    ) %>%
      rlang::warn()

  }

  data <- data %>%
    tidyr::drop_na()

  # Replace the time values in the column names with "start" and "end"
  colnames(data) <- gsub(
    pattern = as.character(times[1]),
    replacement = "start",
    x = colnames(data)
  )

  colnames(data) <- gsub(
    pattern = as.character(times[2]),
    replacement = "end",
    x = colnames(data)
  )

  # Quote the new column names for use in tidy evaluation
  state_start_name <- paste0(state_name, "_start")

  state_end_name <- paste0(state_name, "_end")

  metric_start_name <- paste0(metric_name, "_start")

  # browser()

  # Group the data by the state variables, preserving all factor levels, and
  # summarize by taking the sum of the starting `metric` value for each
  # group
  data <- data %>%
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(
          c(state_start_name, state_end_name)
        )
      ),
      .drop = FALSE
    ) %>%
    dplyr::summarise(
      "{metric_name}" := sum(.data[[metric_start_name]]),
      .groups = "drop"
    )

  # If the user set `percent = TRUE` in function argument...
  if (percent) {

    # ...compute the % of the metric column variable value for starting state
    # class that ended up in the ending state class
    data %>%
      dplyr::group_by(.data[[state_start_name]]) %>%
      dplyr::mutate(
        "{metric_name}" := .data[[metric_name]] / sum(.data[[metric_name]])
      ) %>%
      dplyr::ungroup() %>%
      # Replace `NaN` values with `Inf` so that they are not dropped with `drop_na()`
      dplyr::mutate(
        "{metric_name}" := ifelse(
          is.nan(.data[[metric_name]]),
          Inf,
          .data[[metric_name]]
        )
      )

    # otherwise, if `percent == FALSE` (this is the default) return the `data`
    # data frame
  } else {

    data

  }

}
