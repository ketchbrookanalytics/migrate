


# migrate() ---------------------------------------------------------------

raw_ct <- migrate(
  data = mock_credit,
  date = date,
  state = risk_rating,
  id = customer_id,
  verbose = FALSE
)

raw_wtd <- migrate(
  data = mock_credit,
  date = date,
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
    is.numeric(raw_ct %>% dplyr::pull(-1))
  )

  expect_true(
    is.numeric(raw_wtd %>% dplyr::pull(-1))
  )

  expect_true(
    migrate(
      data = mock_credit,
      date = date,
      state = risk_rating,
      id = customer_id,
      percent = FALSE,
      verbose = FALSE
    ) %>% dplyr::pull(-1) %>% is.numeric
  )

  expect_true(
    migrate(
      data = mock_credit,
      date = date,
      state = risk_rating,
      id = customer_id,
      metric = principal_balance,
      percent = FALSE,
      verbose = FALSE
    ) %>% dplyr::pull(-1) %>% is.numeric
  )

})


test_that("migrate() expects a data frame in `data` argument", {

  expect_error(
    migrate(
      data = c(1:10),
      date = date,
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
      date = date,
      state = risk_rating,
      id = customer_id
    )
  )

  # no message returned
  expect_silent(
    migrate(
      data = mock_credit,
      date = date,
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

  min_date <- min(mock_credit$date)

  # the total 'count' output should equal the input number of observations at
  # the earliest date (assuming each ID has exactly 2 observations in the data)
  expect_equal(
    mock_credit %>% dplyr::filter(date == min_date) %>% nrow(),
    migrate(
      data = mock_credit,
      date = date,
      state = risk_rating,
      id = customer_id,
      percent = FALSE,
      verbose = FALSE
    ) %>%
      dplyr::pull(count) %>%
      sum()
  )

  # similar behavior should take place when using a metric
  expect_equal(

    mock_credit %>%
      dplyr::filter(date == min_date) %>%
      dplyr::pull(principal_balance) %>%
      sum(),

    migrate(
      data = mock_credit,
      date = date,
      state = risk_rating,
      id = customer_id,
      metric = principal_balance,
      percent = FALSE,
      verbose = FALSE
    ) %>%
      dplyr::pull(principal_balance) %>%
      sum()

  )

})


test_that("migrate() throws an error if deprecated `rating` argument is used", {

  expect_error(
    migrate(
      data = mock_credit,
      date = date,
      rating = risk_rating,   # use `rating` instead of `state`
      id = customer_id
    ),
    regexp = "`rating` argument is deprecated"
  )

})


test_that("migrate() throws an error if `date` argument is not type \"Date\"", {

  expect_error(
    migrate(
      data = mock_credit,
      date = customer_id,   # using a "character"-type column for `date`
      state = risk_rating,
      id = customer_id
    ),
    regexp = "`date` argument must be a \"Date\"-type"
  )

})


test_that("migrate() throws a warning if `state` variable is not an ordered factor", {

  # suggest ordering the given factor
  expect_warning(
    mock_credit %>%
      dplyr::mutate(risk_rating = factor(risk_rating, ordered = FALSE)) %>%
      migrate(
        date = date,
        state = risk_rating,
        id = customer_id,
        verbose = FALSE
      ),
    regexp = "Please consider converting `risk_rating` to an ordered factor"
  )

  # suggest converting to an ordered factor
  expect_warning(
    mock_credit %>%
      dplyr::mutate(risk_rating = as.character(risk_rating)) %>%
      migrate(
        date = date,
        state = risk_rating,
        id = customer_id,
        verbose = FALSE
      ),
    regexp = "Converting `risk_rating` to type `factor`"
  )

})


test_that("migrate() throws an error if there aren't exactly 2 unique dates in the data", {

  expect_error(
    mock_credit %>%
      # add an extra observation with a different date
      dplyr::bind_rows(
        mock_credit %>% dplyr::slice(1) %>% dplyr::mutate(date = as.Date("1900-01-01"))
      ) %>%
      migrate(
        date = date,
        state = risk_rating,
        id = customer_id
      ),
    regexp = "There must be exactly 2 unique values in the `date` column"
  )

})


test_that("migrate() throws an error if `metric` argument is supplied incorrectly", {

  expect_error(
    migrate(
      data = mock_credit,
      date = date,
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
      date = date,
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
    mock_credit %>%
      dplyr::mutate(principal_balance = as.character(principal_balance)) %>%
      migrate(
        date = date,
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
        date = date,
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
        date = date,
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


test_that("migrate() throws a warning if any ID values only exist once", {

  # randomly choose an integer representing the number of observations to remove
  # from the tail of the data frame
  rows_to_omit <- runif(1, min = 1, max = 10) %>% round(0)

  expect_warning(
    migrate(
      data = mock_credit %>% dplyr::slice(-(1:rows_to_omit)),   # remove rows
      date = date,
      state = risk_rating,
      id = customer_id,
      metric = principal_balance,
      percent = FALSE,
      verbose = FALSE
    ),
    regexp = paste0("Removing ", rows_to_omit, " observations")
  )

})
