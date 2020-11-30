test_that("build_matrix() returns an object of classes `matrix` & `array`", {

  raw <- migrate(
    data = mock_credit,
    date = date,
    state = risk_rating,
    id = customer_id,
    metric = principal_balance
  ) %>%
    build_matrix()

  testthat::expect_equal(
    class(raw),
    c("matrix", "array")
  )

})
