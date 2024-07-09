


# build_matrix() ----------------------------------------------------------

mig <- migrate(
  data = mock_credit,
  id = customer_id,
  time = date,
  state = risk_rating,
  metric = principal_balance,
  verbose = FALSE
)

mat <- mig |>
  build_matrix(
    state_start = risk_rating_start,
    state_end = risk_rating_end,
    metric = principal_balance
  )

test_that("build_matrix() returns a `matrix` object", {

  expect_true(
    inherits(mat, 'matrix')
  )

  # matrix should be square
  expect_equal(
    dim(mat)[1],
    dim(mat)[2]
  )

  # number of columns == number of unique ending states
  expect_equal(
    dim(mat)[1],   # rows
    unique(mig$risk_rating_start) |> length()
  )

  # number of columns == number of unique ending states
  expect_equal(
    dim(mat)[2],   # columns
    unique(mig$risk_rating_end) |> length()
  )

})


test_that("build_matrix() expects a data frame in `data` argument", {

  expect_error(
    build_matrix(
      data = c(1:10),
      state_start = risk_rating_start,
      state_end = risk_rating_end,
      metric = principal_balance
    ),
    "`data` argument must be a valid data frame"
  )

  expect_error(
    build_matrix(
      data = LETTERS[1:10],
      state_start = risk_rating_start,
      state_end = risk_rating_end,
      metric = principal_balance
    ),
    "`data` argument must be a valid data frame"
  )

})


test_that("build_matrix() is verbose when expected", {

  # returns message
  expect_message(
    build_matrix(
      data = mig,
      # state_start = risk_rating_start,
      state_end = risk_rating_end,
      metric = principal_balance
    ),
    regexp = "Using `risk_rating_start` as the 'state_start' column"
  )

  expect_message(
    build_matrix(
      data = mig,
      state_start = risk_rating_start,
      # state_end = risk_rating_end,
      metric = principal_balance
    ),
    regexp = "Using `risk_rating_end` as the 'state_end' column"
  )

  expect_message(
    build_matrix(
      data = mig,
      state_start = risk_rating_start,
      state_end = risk_rating_end,
      # metric = principal_balance
    ),
    regexp = "Using `principal_balance` as the 'metric' column"
  )

  # no message returned
  expect_silent(
    build_matrix(
      data = mig,
      state_start = risk_rating_start,
      state_end = risk_rating_end,
      metric = principal_balance
    )
  )

})


test_that("build_matrix() outputs row-wise sums of 1 when `percent = TRUE`", {

  # the square root of the matrix length should equal the sum of the
  # output migrated proportions, since the migrated proportions for each
  # starting `state` should sum to 1
  expect_equal(
    sqrt(length(mat)),
    sum(mat)
  )

})


test_that("build_matrix() outputs starting sums when `percent = FALSE`", {

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
      build_matrix(
        state_start = risk_rating_start,
        state_end = risk_rating_end,
        metric = count
      ) |> sum()
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
      build_matrix(
        state_start = risk_rating_start,
        state_end = risk_rating_end,
        metric = principal_balance
      ) |> sum()

  )

})


test_that("If build_matrix() guesses arguments, it does so carefully", {

  # multiple "start" columns
  expect_error(
    mig |>
      dplyr::mutate(start_in_colname = as.factor("a")) |>
      build_matrix(),
    regexp = "Multiple columns of type `factor` with the phrase \"start\""
  )

  # no "start" column
  expect_error(
    mig |>
      dplyr::rename(risk_rating_begin = risk_rating_start) |>
      build_matrix(),
    regexp = "No columns of type `factor` with the phrase \"start\""
  )

  # multiple "end" columns
  expect_error(
    mig |>
      dplyr::mutate(end_in_colname = as.factor("a")) |>
      build_matrix(),
    regexp = "Multiple columns of type `factor` with the phrase \"end\""
  )

  # no "end" column
  expect_error(
    mig |>
      dplyr::rename(risk_rating_final = risk_rating_end) |>
      build_matrix(),
    regexp = "No columns of type `factor` with the phrase \"end\""
  )

  # multiple "metric" columns
  expect_error(
    mig |>
      dplyr::mutate(metric_new = 1) |>
      build_matrix(),
    regexp = "Multiple columns of type `numeric`"
  )

  # no "metric" column
  expect_error(
    mig |>
      dplyr::mutate(principal_balance = as.character(principal_balance)) |>
      build_matrix(),
    regexp = "No columns of type `numeric`"
  )

})


