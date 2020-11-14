test_that("migrate() returns a dataframe", {

  load(here::here("data/mock_credit.rda"))

  raw <- migrate(
    data = mock_credit,
    date = date,
    rating = risk_rating,
    metric = principal_balance
  )

  testthat::expect_s3_class(
    object = raw,
    class = "data.frame"
  )

})
