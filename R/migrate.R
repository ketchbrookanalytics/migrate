


# Validate `migrate()` arguments
check_args <- function(data, percent, fill_state, verbose) {

  # Make sure the 'data' argument is a valid data frame
  if (!is.data.frame(data)) {

    cli::cli_abort(
      paste0(
        "`data` argument must be a valid data frame, not an object of type:",
        class(data)
      )
    )

  }

  # Ensure the supplied `percent` argument is logical
  if (!is.logical(percent)) {

    cli::cli_abort("`percent` argument must be logical TRUE/FALSE")

  }

  # Ensure the supplied `fill_state` argument is coercible to factor type
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
      ) |>
        cli::cli_abort()

    }

  }

  # Ensure the supplied `verbose` argument is logical
  if (!is.logical(verbose)) {

    cli::cli_abort("`verbose` argument must be logical TRUE/FALSE")

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

      cli::cli_warn(
        c("!" = glue::glue("Please consider converting `{ state_name }` to an ordered factor before passing it to `migrate()` to ensure that the rank-ordering in the final matrix displays correctly"))
      )

    }

    # If the `state` column variable in the `data` data frame is *not* type
    # "factor", coerce it to type "factor"
  } else {

    # Print messages to console letting user know that variable is being
    # converted to type `factor`
    cli::cli_warn(
      c(
        "!" = glue::glue("Converting `{ state_name }` to type `factor`"),
        "!" = glue::glue("To ensure that your output is ordered correctly, convert the `{ state_name }` column variable in your data frame to an ordered factor before passing to `migrate()`")
      )
    )
    
    data <- data |>
      dplyr::mutate("{ state_name }" := as.factor(state_vec))

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
    ) |>
      cli::cli_abort()

  }

}



time_message <- function(times) {

  paste0(
    "Migrating from ",
    times[1],
    " to ",
    times[2]
  ) |>
    cli::cli_alert_info()

}



# Remove any NA values across all columns; this will drop observations found
# at only a single timepoint, unless the `fill_state` argument is *not* NULL
drop_missing_timepoints <- function(data) {

  out <- data |>
    tidyr::drop_na()

  cli::cli_warn(
    c("!" = glue::glue("Removed { (nrow(data) - nrow(out)) } observations due to missingness or IDs only existing at one `time` value"))
  )

  return(out)

}



# Group the data by the state variables, preserving all factor levels, and
# summarize by taking the sum of the starting `metric` value for each group
migrate_count <- function(data,
                          state_start_name, state_end_name,
                          metric_name, metric_start_name) {

  data |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(
          c(state_start_name, state_end_name)
        )
      ),
      .drop = FALSE
    ) |>
    dplyr::summarise(
      "{metric_name}" := sum(.data[[metric_start_name]]),
      .groups = "drop"
    )

}


# Compute the % of the metric column variable value for starting state class
# that ended up in the ending state class
migrate_percent <- function(data, state_start_name, metric_name) {

  data |>
    dplyr::group_by(.data[[state_start_name]]) |>
    dplyr::mutate(
      "{metric_name}" := .data[[metric_name]] / sum(.data[[metric_name]])
    ) |>
    dplyr::ungroup() |>
    # Replace `NaN` values with `Inf` so that they are not dropped with `drop_na()`
    dplyr::mutate(
      "{metric_name}" := ifelse(
        is.nan(.data[[metric_name]]),
        Inf,
        .data[[metric_name]]
      )
    )

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
#' @param id The column variable of the `data` data frame argument that contains
#'   the unique identifier to track where a particular credit facility migrated
#'   to/from. If left null, `migrate()` will attempt to use the first column
#'   variable from the data frame provided in the `data` argument.
#' @param time The column variable of in the `data` data frame representing the
#'   timepoint (e.g., a Date) of each observation; this column should contain
#'   two unique values (migration from Time A to Time B)
#' @param state The column variable of the `data` data frame argument that
#'   contains the credit risk state values.
#' @param metric (Optional) The column variable of type "numeric" in the `data`
#'   data frame argument that contains the continuous metric values to weight
#'   the state migration by
#' @param percent If `FALSE`, will calculate the migration on an absolute basis
#'   (rather than a percentage basis, which is the default)
#' @param fill_state (Optional) A value (e.g., a character string such as "No
#'   Rating" or "NR") to be used as the *filler* `state` for any `id` values
#'   that only exist at one timepoint in the `data`.
#' @param verbose If `TRUE`, the function returns an informational message about
#'   the transition period
#'
#' @return
#' A data frame containing three (3) column variables representing the unique
#' combinations of starting & ending credit risk states and the calculated
#' migration observed during the period.
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
#' # Provide a filler `state` value when a unique `id` is missing a timepoint
#' migrate(
#'   data = head(mock_credit, n = 995),   # drop the last 5 observations
#'   id = customer_id,
#'   time = date,
#'   state = risk_rating,
#'   fill_state = "NR",
#'   percent = FALSE
#' )
migrate <- function(data, id, time, state,
                    metric = NULL, percent = TRUE, fill_state = NULL,
                    verbose = TRUE) {

  # Validate function arguments
  check_args(data, percent, fill_state, verbose)

  # Coerce input data frame to a tibble
  data <- tibble::as_tibble(data)

  # Capture supplied  variable arguments as strings
  id_name <- rlang::enquo(id) |> rlang::as_label()
  time_name <- rlang::enquo(time) |> rlang::as_label()
  state_name <- rlang::enquo(state) |> rlang::as_label()

  # Coerce the `state` column variable to type "factor" (if necessary)
  data <- coerce_factor(data, state_name)

  # Capture the distinct `time` values, sorted ascending
  times <- data[[time_name]] |>
    unique() |>
    sort()

  # Stop execution if there aren't exactly 2 unique time values in the data
  check_times(times, time_name)

  # If the `metric` argument is supplied...
  if (!missing(metric)) {

    # Capture the name of the `metric` argument as a character string
    metric_name <- rlang::enquo(metric) |>
      rlang::as_label()

    # Stop if the `metric` variable doesn't exist in the `data` data frame,
    if (!metric_name %in% colnames(data)) {

      cli::cli_abort("`metric` argument must be an unquoted variable in `data`")

    }

    # Stop if the `metric` variable isn't numeric
    if (!is.numeric(data[[metric_name]])) {

      cli::cli_abort("`metric` argument must be a numeric type variable in `data`")

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

    fill_state_is_new <- !fill_state %in% levels(data[[state_name]])

    if (verbose) {

      # Determine number of IDs with a missing start or end timepoint
      data_wide <- data |>
        tidyr::pivot_wider(
          id_cols = {{ id }},
          names_from = {{ time }},
          values_from = {{ time }}
        )
  
      n_missing_start <- sum(is.na(data_wide[[2]]))
      n_missing_end <- sum(is.na(data_wide[[3]]))
    
      n_missing <- n_missing_start + n_missing_end

      fill_state_class_type <- ifelse(fill_state_is_new, "new", "existing")

      # Inform the user
      cli::cli_div(theme = list(ul = list(`margin-left` = 2, before = "")))
      cli::cli_alert_info(glue::glue("{ n_missing } IDs have a missing timepoint:"))
      cli::cli_ul(id = "ul_id")
        cli::cli_li(glue::glue("Migrating { n_missing_end } IDs with missing end timepoint to { fill_state_class_type } class '{ fill_state }'"))
        cli::cli_li(glue::glue("Migrating { n_missing_start } IDs with missing start timepoint from { fill_state_class_type } class '{ fill_state }'"))
      cli::cli_end(id = "ul_id")

    }

    # ... add the fill state to the factor levels (if it doesn't already exist)
    if (fill_state_is_new) {

      data <- data |>
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
  data <- data |>
    tidyr::pivot_wider(
      id_cols = {{ id }},
      names_from = {{ time }},
      values_from = dplyr::all_of(
        c(state_name, metric_name)
      ),
      values_fill = fill_state   # this won't do anything if `fill_state = NULL`
    )

  # Remove any NA values across all columns; this will drop observations found
  # at only a single timepoint, unless the `fill_state` argument is *not* NULL
  if (nrow(tidyr::drop_na(data)) < nrow(data)) {

    data <- drop_missing_timepoints(data)

  }

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

  data <- migrate_count(
    data = data,
    state_start_name = state_start_name,
    state_end_name = state_end_name,
    metric_name = metric_name,
    metric_start_name = metric_start_name
  )

  # If the user set `percent = TRUE` in function argument...
  if (percent) {

    migrate_percent(
      data = data,
      state_start_name = state_start_name,
      metric_name = metric_name
    )

    # otherwise, if `percent == FALSE` (this is the default) return the `data`
    # data frame
  } else {

    data

  }

}
