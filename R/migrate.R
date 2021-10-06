#' Summarize the migration of a data frame
#'
#' @description
#' `migrate()` summarizes the transition amount (or percentage) of a continuous
#' variable from each beginning credit risk state category to each ending credit
#' risk state, given a data frame input.
#'
#' @param data A data frame or data frame extension (e.g., a tibble or
#'   data.table) containing a minimum of three (3) column variables representing
#'   a date, a credit risk state, and an ID identifying the credit facility (we
#'   would expect to see most unique values in this column variable appear twice
#'   in the dataset; once at the first date and again at the second date, unless
#'   the credit only existed at one of those two dates).
#' @param date The column variable of type "Date" in the `data` data frame
#'   argument that contains the two unique date values
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
#' # Return the percent migration of the number of credit facilities
#' migrate(
#'   data = mock_credit,
#'   date = date,
#'   state = risk_rating,
#'   id = customer_id
#' )
#'
#' # Return the absolute migration in `principal_balance`
#' migrate(
#'   data = mock_credit,
#'   date = date,
#'   state = risk_rating,
#'   id = customer_id,
#'   metric = principal_balance,
#'   percent = FALSE
#' )
#'
migrate <- function(data, id, date, state, metric = NULL, percent = TRUE,
                    verbose = TRUE, rating = NULL) {

  # Handle Deprecation Warnings
  # If deprecated `rating` argument is supplied...
  if (!missing(rating)) {

    # ... warn user that it is deprecated
    rlang::abort(
      "`rating` argument is deprecated; please use `state` instead",
      call. = FALSE
    )

  }

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

  # Ensure the supplied `verbose` argument is logical
  if (!is.logical(verbose)) {

    rlang::abort("`verbose` argument must be logical TRUE/FALSE")

  }

  # Ensure supplied `date` argument is a "Date"-type column variable in `data`
  is_date <- data %>%
    dplyr::pull({{ date }}) %>%
    inherits('Date')

  if (!is_date) {

    rlang::abort(
      "`date` argument must be a \"Date\"-type column variable in `data`"
    )

  }

  # Coerce input data frame to a tibble
  data <- data %>%
    tibble::as_tibble()

  # Create variables for tidy evaluation
  id_name <- rlang::enquo(id) %>%
    rlang::as_label()

  date_name <- rlang::enquo(date) %>%
    rlang::as_label()

  state_name <- rlang::enquo(state) %>%
    rlang::as_label()

  # Capture whether or not the `state` variable in the 'data' data frame
  # is type "factor"
  state_factor_status <- data %>%
    dplyr::pull({{ state }}) %>%
    is.factor()

  # If the `state` column variable in the `data` data frame is type
  # "factor", capture whether or not it is ordered
  if (state_factor_status) {

    state_ordered_status <- data %>%
      dplyr::pull({{ state }}) %>%
      is.ordered()

    # If the `state` column variable in the `data` data frame is an
    # unordered factor, print a message to the console asking the user
    # to convert it to an ordered factor
    if (!state_ordered_status) {

      paste0(
        "Please consider converting `",
        state_name,
        "` to an ordered factor before passing it to `migrate()` to ensure ",
        "that the rank-ordering in the final matrix displays correctly"
      ) %>% rlang::warn()

    }

  }

  # Convert `state` variable to type `factor`, if necessary
  if (!state_factor_status) {

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

  # Get the number of unique dates in the data
  num_dates <- data %>%
    dplyr::pull({{ date }}) %>%
    unique() %>%
    length()

  # Stop execution if there aren't exactly 2 unique dates in the data
  if (num_dates != 2) {

    paste0(
      "There must be exactly 2 unique values in the `",
      date_name,
      "` column variable; ",
      num_dates,
      " unique values were found"
    ) %>%
      rlang::abort()

  }

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
    if (!is.numeric(dplyr::pull(data, {{ metric }}))) {

      rlang::abort("`metric` argument must be a numeric type variable in `data`")

    }

    # ...if the `metric` argument is not supplied
  } else {

    # Set the `metric_name` character string based upon if `percent == TRUE`
    metric_name <- ifelse(percent, "prop", "count")

    metric <- dplyr::sym(metric_name)

    # Add a new column to the `data` data frame, where every value is 1
    # This will help us show the transition on a count or percentage basis later
    data <- data %>%
      dplyr::mutate("{{ metric }}" := 1)

  }

  # Capture the min date value
  min_date <- data %>%
    dplyr::pull({{ date }}) %>%
    min()

  # Capture the max date value
  max_date <- data %>%
    dplyr::pull({{ date }}) %>%
    max()

  # Inform the user of the migration time period, unless `verbose = FALSE`
  if (verbose) {

    paste0(
      "=== Migrating from: `",
      min_date,
      "` --> ` ",
      max_date,
      "` ==="
    ) %>%
      rlang::inform()

  }

  # Pivot the data from long to wide based upon the 'date' column variable
  data <- data %>%
    tidyr::pivot_wider(
      id_cols = {{ id }},
      names_from = {{ date }},
      values_from = c({{ state }}, {{ metric }})
    )

  # Remove any NA values across all columns; this will also remove observations
  # that only appeared at one `date` value in the data frame; we have no way of
  # allocating such observations in a migration matrix since we either don't
  # know what state they're migrating *to* (in the case where it only appears at
  # the earlier date) or *from* (in the case where it only appears at the later
  # date), as `pivot_wider()` creates NA values for these cases
  if (nrow(tidyr::drop_na(data)) < nrow(data)) {

    paste0(
      "Removing ",
      (nrow(data) - nrow(tidyr::drop_na(data))),
      " observations due to missingness or IDs only existing at one `date` ",
      "value"
    ) %>%
      rlang::warn()

  }

  data <- data %>%
    tidyr::drop_na()

  # Replace the date values in the column names with "start" and "end"
  colnames(data) <- gsub(
    pattern = as.character(min_date),
    replacement = "start",
    x = colnames(data)
  )

  colnames(data) <- gsub(
    pattern = as.character(max_date),
    replacement = "end",
    x = colnames(data)
  )

  # Quote the new column names for use in tidy evaluation
  state_start_name <- rlang::enquo(state) %>%
    rlang::as_label() %>%
    paste0("_start")

  state_end_name <- rlang::enquo(state) %>%
    rlang::as_label() %>%
    paste0("_end")

  metric_start_name <- rlang::enquo(metric) %>%
    rlang::as_label() %>%
    paste0("_start")

  # Group the data by the state variables, preserving all factor levels, and
  # summarize by taking the sum of the starting `metric` value for each
  # group
  data <- data %>%
    dplyr::group_by(
      dplyr::across(
        tidyselect::all_of(
          c(state_start_name, state_end_name)
        )
      ),
      .drop = FALSE
    ) %>%
    dplyr::summarise(
      "{{ metric }}" := sum(.data[[metric_start_name]]),
      .groups = "drop"
    )

  # If the user set `percent = TRUE` in function argument...
  if (percent) {

    # ...compute the % of the metric column variable value for starting state
    # class that ended up in the ending state class
    data %>%
      dplyr::group_by(.data[[state_start_name]]) %>%
      dplyr::mutate(
        "{{ metric }}" := {{ metric }} / sum({{ metric }})
      ) %>%
      dplyr::ungroup() %>%
      # Replace `NaN` values with `Inf` so that they are not dropped with `drop_na()`
      dplyr::mutate("{{ metric }}" := ifelse(is.nan({{ metric }}), Inf, {{ metric }})) %>%
      tidyr::drop_na()

    # otherwise, if `percent == FALSE` (this is the default) return the `data`
    # data frame
  } else {

    data

  }

}
