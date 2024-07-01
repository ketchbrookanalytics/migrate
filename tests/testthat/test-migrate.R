


# migrate() ---------------------------------------------------------------

raw_ct <- migrate(
  data = mock_credit,
  time = date,
  state = risk_rating,
  id = customer_id,
  verbose = FALSE
)

raw_wtd <- migrate(
  data = mock_credit,
  time = date,
  state = risk_rating,
  id = customer_id,
  metric = principal_balance,
  verbose = FALSE
)

test_that("migrate() returns a three-column dataframe", {

  # returns a data frame
  expect_true(
    inherits(raw_wtd, "data.frame")
  )

  expect_true(
    inherits(raw_ct, "data.frame")
  )

  # data frame is exactly 3 columns
  expect_equal(
    ncol(raw_wtd), 3
  )

  expect_equal(
    ncol(raw_ct), 3
  )

  # first & second columns contain correct naming suffixes
  expect_match(
    colnames(raw_ct)[1],
    regexp = "_start$"
  )

  expect_match(
    colnames(raw_wtd)[1],
    regexp = "_start$"
  )

  expect_match(
    colnames(raw_ct)[2],
    regexp = "_end$"
  )

  expect_match(
    colnames(raw_wtd)[2],
    regexp = "_end$"
  )

  expect_true(
    is.numeric(raw_ct |> dplyr::pull(-1))
  )

  expect_true(
    is.numeric(raw_wtd |> dplyr::pull(-1))
  )

  expect_true(
    migrate(
      data = mock_credit,
      time = date,
      state = risk_rating,
      id = customer_id,
      percent = FALSE,
      verbose = FALSE
    ) |> dplyr::pull(-1) |> is.numeric()
  )

  expect_true(
    migrate(
      data = mock_credit,
      time = date,
      state = risk_rating,
      id = customer_id,
      metric = principal_balance,
      percent = FALSE,
      verbose = FALSE
    ) |> dplyr::pull(-1) |> is.numeric()
  )

})


test_that("migrate() expects a data frame in `data` argument", {

  expect_error(
    migrate(
      data = c(1:10),
      time = date,
      state = risk_rating,
      id = customer_id
    ),
    "`data` argument must be a valid data frame"
  )

})


test_that("migrate() is verbose by default", {

  # returns message
  expect_message(
    migrate(
      data = mock_credit,
      time = date,
      state = risk_rating,
      id = customer_id
    )
  )

  # no message returned
  expect_silent(
    migrate(
      data = mock_credit,
      time = date,
      state = risk_rating,
      id = customer_id,
      verbose = FALSE
    )
  )

})


test_that("migrate() outputs row-wise sums of 1 when `percent = TRUE`", {

  # the square root of the number of output rows should equal the sum of the
  # output migrated proportions, since the migrated proportions for each
  # starting `state` should sum to 1
  expect_equal(
    sqrt(nrow(raw_ct)),
    sum(raw_ct$prop)
  )

  expect_equal(
    sqrt(nrow(raw_ct)),
    sum(raw_wtd$principal_balance)
  )

})


test_that("migrate() outputs starting sums when `percent = FALSE`", {

  min_time <- min(mock_credit$date)

  # the total 'count' output should equal the input number of observations at
  # the earliest time (assuming each ID has exactly 2 observations in the data)
  expect_equal(
    mock_credit |> dplyr::filter(date == min_time) |> nrow(),
    migrate(
      data = mock_credit,
      time = date,
      state = risk_rating,
      id = customer_id,
      percent = FALSE,
      verbose = FALSE
    ) |>
      dplyr::pull(count) |>
      sum()
  )

  # similar behavior should take place when using a metric
  expect_equal(

    mock_credit |>
      dplyr::filter(date == min_time) |>
      dplyr::pull(principal_balance) |>
      sum(),

    migrate(
      data = mock_credit,
      time = date,
      state = risk_rating,
      id = customer_id,
      metric = principal_balance,
      percent = FALSE,
      verbose = FALSE
    ) |>
      dplyr::pull(principal_balance) |>
      sum()

  )

})


test_that("migrate() throws a warning if `state` variable is not an ordered factor", {

  # suggest ordering the given factor
  expect_warning(
    mock_credit |>
      dplyr::mutate(risk_rating = factor(risk_rating, ordered = FALSE)) |>
      migrate(
        time = date,
        state = risk_rating,
        id = customer_id,
        verbose = FALSE
      ),
    regexp = "Please consider converting `risk_rating` to an ordered factor"
  )

  # suggest converting to an ordered factor
  expect_warning(
    mock_credit |>
      dplyr::mutate(risk_rating = as.character(risk_rating)) |>
      migrate(
        time = date,
        state = risk_rating,
        id = customer_id,
        verbose = FALSE
      ),
    regexp = "Converting `risk_rating` to type `factor`"
  )

})


test_that("migrate() expects exactly 2 unique time values for each `id` value", {

  # error if > 2 distinct `time` values found
  expect_error(
    mock_credit |>
      # add an extra observation with a different date
      dplyr::bind_rows(
        mock_credit |>
          dplyr::slice(1) |>
          dplyr::mutate(date = as.Date("1900-01-01"))
      ) |>
      migrate(
        time = date,
        state = risk_rating,
        id = customer_id
      ),
    regexp = "There must be exactly 2 unique values"
  )

  # randomly choose an integer representing the number of observations to remove
  # from the tail of the data frame
  rows_to_omit <- runif(1, min = 1, max = 10) |> round(0)

  expect_warning(
    migrate(
      data = mock_credit |> dplyr::slice(-(1:rows_to_omit)),   # remove rows
      time = date,
      state = risk_rating,
      id = customer_id,
      metric = principal_balance,
      percent = FALSE,
      verbose = FALSE
    ),
    regexp = paste0("Removed ", rows_to_omit, " observations")
  )

})


test_that("migrate() throws an error if `metric` argument is supplied incorrectly", {

  expect_error(
    migrate(
      data = mock_credit,
      time = date,
      state = risk_rating,
      id = customer_id,
      metric = var_not_in_df,
      verbose = FALSE
    ),
    regexp = "`metric` argument must be an unquoted variable"
  )

  expect_error(
    migrate(
      data = mock_credit,
      time = date,
      state = risk_rating,
      id = customer_id,
      metric = "principal_balance",
      verbose = FALSE
    ),
    regexp = "`metric` argument must be an unquoted variable"
  )

})


test_that("migrate() throws an error if `metric` argument is not numeric column", {

  expect_error(
    mock_credit |>
      dplyr::mutate(principal_balance = as.character(principal_balance)) |>
      migrate(
        time = date,
        state = risk_rating,
        id = customer_id,
        metric = principal_balance,
        verbose = FALSE
      ),
    regexp = "`metric` argument must be a numeric type variable"
  )

})

test_that("migrate() correctly names third column based upon `metric` argument", {

  # when `percent = TRUE` (default)
  expect_identical(
    colnames(raw_ct)[3],
    "prop"
  )

  expect_identical(
    colnames(raw_wtd)[3],
    "principal_balance"
  )

  # when `percent = FALSE`
  expect_identical(
    colnames(
      migrate(
        data = mock_credit,
        time = date,
        state = risk_rating,
        id = customer_id,
        percent = FALSE,
        verbose = FALSE
      )
    )[3],
    "count"
  )
  # when `percent = FALSE`
  expect_identical(
    colnames(
      migrate(
        data = mock_credit,
        time = date,
        state = risk_rating,
        id = customer_id,
        metric = principal_balance,
        percent = FALSE,
        verbose = FALSE
      )
    )[3],
    "principal_balance"
  )

})

test_that("migrate() coerces 'character'-type `state` columns to type 'factor'", {

  suppressWarnings({
    df_character <- migrate(
      data = dplyr::mutate(mock_credit, risk_rating = as.character(risk_rating)),
      id = customer_id,
      time = date,
      state = risk_rating,
      verbose = FALSE
    )
  })

  expect_identical(
    raw_ct |>
      dplyr::mutate(
        dplyr::across(
          .cols = c(risk_rating_start, risk_rating_end),
          .fns = function(x) as.character(x) |> as.factor()
        )
      ) |>
      dplyr::arrange(
        risk_rating_start,
        risk_rating_end
      ),
    df_character
  )

})

## Tests for `fill_state` argument  ---------------------------------------

# Create mock data with `customer_id` values that only exist at one timepoint.
# In particular, `mock_credit_with_missing` has:
# - 20 customers that have a value only in the first timepoint
# - 10 customers that have a value only in the second timepoint
mock_credit_with_missing <- mock_credit |> 
  # Remove the first 10 rows
  dplyr::slice(-(1:10)) |>
  # Remove the last 20 rows
  dplyr::slice(-((dplyr::n() - 19):dplyr::n()))

test_that("migrate() doesn't remove customers with missing timepoints when `fill_state` is not NULL", {

  migrate_counts_without_missing <- migrate(
    data = mock_credit,
    time = date,
    state = risk_rating,
    id = customer_id,
    percent = FALSE,
    verbose = FALSE
  ) |> 
    dplyr::pull(count) |> 
    sum()

  migrate_counts_with_missing <- migrate(
    data = mock_credit_with_missing,
    time = date,
    state = risk_rating,
    id = customer_id,
    percent = FALSE,
    fill_state = "NR",
    verbose = FALSE
  ) |> 
    dplyr::pull(count) |> 
    sum()

  expect_equal(migrate_counts_without_missing, migrate_counts_with_missing)

})

test_that("migrate() removes customers with missing timepoints when `fill_state` is NULL", {

  migrate_counts_without_fill_state <- suppressWarnings({
    migrate(
      data = mock_credit_with_missing,
      time = date,
      state = risk_rating,
      id = customer_id,
      percent = FALSE,
      verbose = FALSE
    ) |> 
      dplyr::pull(count) |> 
      sum()
  }) 

  migrate_counts_with_fill_state <- migrate(
    data = mock_credit_with_missing,
    time = date,
    state = risk_rating,
    id = customer_id,
    percent = FALSE,
    fill_state = "NR",
    verbose = FALSE
  ) |> 
    dplyr::pull(count) |> 
    sum()

  expect_true(migrate_counts_without_fill_state < migrate_counts_with_fill_state)

})

test_that("migrate() uses the filler state defined in `fill_state`", {

  fill1 <- migrate(
    data = mock_credit_with_missing,
    time = date,
    state = risk_rating,
    id = customer_id,
    percent = FALSE,
    fill_state = "NR",
    verbose = FALSE
  )

  expect_true("NR" %in% fill1$risk_rating_start)
  expect_true("NR" %in% fill1$risk_rating_end)

  fill2 <- migrate(
    data = mock_credit_with_missing,
    time = date,
    state = risk_rating,
    id = customer_id,
    percent = FALSE,
    fill_state = "No Rating",
    verbose = FALSE
  )

  expect_true("No Rating" %in% fill2$risk_rating_start)
  expect_true("No Rating" %in% fill2$risk_rating_end)

})

test_that("migrate() assigns filler state correctly when `fill_state` is not NULL", {

  migrated_data <- migrate(
    data = mock_credit_with_missing,
    time = date,
    state = risk_rating,
    id = customer_id,
    percent = FALSE,
    fill_state = "NR",
    verbose = FALSE
  )

  n_missing_start <- migrated_data |> 
    dplyr::count(risk_rating_start, wt = count) |> 
    dplyr::filter(risk_rating_start == "NR") |> 
    dplyr::pull(n)

  n_missing_end <- migrated_data |> 
    dplyr::count(risk_rating_end, wt = count) |> 
    dplyr::filter(risk_rating_end == "NR") |> 
    dplyr::pull(n)

  # Recall that `mock_credit_with_missing` removed the first 10 and the last 20 rows
  expect_true(n_missing_start == 10)
  expect_true(n_missing_end == 20)

})

test_that("migrate() errors when `fill_state` is not a length one value", {

  expect_error(
    migrate(
      data = mock_credit_with_missing,
      time = date,
      state = risk_rating,
      id = customer_id,
      percent = FALSE,
      fill_state = c("NR", "No Rating"),
      verbose = FALSE
    ),
    reg_exp = "`fill_state` argument must be length-one value (of type `character`, `numeric`, or `factor`)"
  )

})

test_that("migrate() errors when `fill_state` is not of type `character`, `numeric` or `factor`", {

  # Logical
  expect_error(
    migrate(
      data = mock_credit_with_missing,
      time = date,
      state = risk_rating,
      id = customer_id,
      percent = FALSE,
      fill_state = FALSE,
      verbose = FALSE
    ),
    reg_exp = "`fill_state` argument must be length-one value (of type `character`, `numeric`, or `factor`)"
  )

  # Dataframe
  expect_error(
    migrate(
      data = mock_credit_with_missing,
      time = date,
      state = risk_rating,
      id = customer_id,
      percent = FALSE,
      fill_state = iris,
      verbose = FALSE
    ),
    reg_exp = "`fill_state` argument must be length-one value (of type `character`, `numeric`, or `factor`)"
  )

})

test_that("migrate() works when `fill_state` is of type `character`", {

  migrated_data <- migrate(
    data = mock_credit_with_missing,
    time = date,
    state = risk_rating,
    id = customer_id,
    percent = FALSE,
    fill_state = "NR",
    verbose = FALSE
  )

  expect_true(
    inherits(migrated_data, "data.frame")
  )

})

test_that("migrate() works when `fill_state` is of type `numeric`", {

  migrated_data <- migrate(
    data = mock_credit_with_missing,
    time = date,
    state = risk_rating,
    id = customer_id,
    percent = FALSE,
    fill_state = 999,
    verbose = FALSE
  )

  expect_true(
    inherits(migrated_data, "data.frame")
  )

})

test_that("migrate() works when `fill_state` is of type `factor`", {

  migrated_data <- migrate(
    data = mock_credit_with_missing,
    time = date,
    state = risk_rating,
    id = customer_id,
    percent = FALSE,
    fill_state = as.factor("No Rating"),
    verbose = FALSE
  )

  expect_true(
    inherits(migrated_data, "data.frame")
  )

})

test_that("migrate() works when `fill_state` is a value that already exists", {

  migrated_data_without_missing <- migrate(
    data = mock_credit,
    time = date,
    state = risk_rating,
    id = customer_id,
    percent = FALSE,
    verbose = FALSE
  )

  migrated_data_with_missing <- migrate(
    data = mock_credit_with_missing,
    time = date,
    state = risk_rating,
    id = customer_id,
    percent = FALSE,
    fill_state = "CCC",
    verbose = FALSE
  )

  expect_identical(nrow(migrated_data_without_missing), nrow(migrated_data_with_missing))
  expect_identical(sum(migrated_data_without_missing$count),sum(migrated_data_with_missing$count))

})

test_that("migrate() outputs more rows when `fill_state` is a value that doesn't exist", {

  migrated_data_without_missing <- migrate(
    data = mock_credit,
    time = date,
    state = risk_rating,
    id = customer_id,
    percent = FALSE,
    verbose = FALSE
  )

  migrated_data_with_missing <- migrate(
    data = mock_credit_with_missing,
    time = date,
    state = risk_rating,
    id = customer_id,
    percent = FALSE,
    fill_state = "NR",
    verbose = FALSE
  )

  expect_true(nrow(migrated_data_without_missing) < nrow(migrated_data_with_missing))

})

test_that("migrate() sets the value defined in `fill_state` as the greatest factor level", {

  migrated_data <- migrate(
    data = mock_credit_with_missing,
    time = date,
    state = risk_rating,
    id = customer_id,
    percent = FALSE,
    fill_state = "ABC",
    verbose = FALSE
  )

  levels <- levels(migrated_data$risk_rating_start)

  expect_identical("ABC", levels[length(levels)])

})

test_that("migrate() correctly informs missing timepoint migration", {

  messages_new_class <- testthat::capture_messages(
    migrate(
      data = mock_credit_with_missing,
      time = date,
      state = risk_rating,
      id = customer_id,
      percent = FALSE,
      fill_state = "NR",
      verbose = TRUE
    )
  )

  expect_true(any(grepl("30 IDs have a missing timepoint:", messages_new_class)))
  expect_true(any(grepl("Migrating 20 IDs with missing end timepoint to \\*new\\* class 'NR'", messages_new_class)))
  expect_true(any(grepl("Migrating 10 IDs with missing start timepoint from \\*new\\* class 'NR'", messages_new_class)))

  messages_existing_class <- testthat::capture_messages(
    migrate(
      data = mock_credit_with_missing,
      time = date,
      state = risk_rating,
      id = customer_id,
      percent = FALSE,
      fill_state = "CCC",
      verbose = TRUE
    )
  )

  expect_true(any(grepl("30 IDs have a missing timepoint:", messages_existing_class)))
  expect_true(any(grepl("Migrating 20 IDs with missing end timepoint to \\*existing\\* class 'CCC'", messages_existing_class)))
  expect_true(any(grepl("Migrating 10 IDs with missing start timepoint from \\*existing\\* class 'CCC'", messages_existing_class)))

})
