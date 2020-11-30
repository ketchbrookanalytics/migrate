test_that("migrate() returns a dataframe", {

  raw <- migrate(
    data = mock_credit,
    date = date,
    state = risk_rating,
    id = customer_id,
    metric = principal_balance
  )

  testthat::expect_s3_class(
    object = raw,
    class = "data.frame"
  )

})

test_that
