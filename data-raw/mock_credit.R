## code to prepare `mock_credit` dataset goes here

library(dplyr)
library(scales)

set.seed(4321)
state_movement <- sample(
  x = c(-2:3),
  size = 500,
  replace = TRUE,
  prob = c(0.01, 0.10, 0.65, 0.19, .04, 0.01)
)

customer_ids <- c(
  paste0(
    "Customer_",
    as.character(
      seq(
        from = 1001,
        to = 1500,
        by = 1
      )
    )
  )
)

dates <- c(
  rep(as.Date("2020-06-30"), 500),
  rep(as.Date("2020-09-30"), 500)
)

start_state <- sample(
  x = c(1:7),
  size = 500,
  replace = TRUE,
  prob = c(0.04, 0.17, 0.21, 0.14, 0.12, 0.1, 0.03)
)

end_state <- start_state + state_movement
end_state <- ifelse(end_state < 1, 1, end_state)
end_state <- ifelse(end_state > 7, 7, end_state)

mock_credit <- data.frame(
  customer_id = rep(customer_ids, 2),
  date = dates,
  risk_rating = c(start_state, end_state),
  principal_balance = rgamma(
    1000,
    shape = 1.8,
    rate = 2
  ) |>
    scales::rescale(to = c(10000, 5000000)) |>
    round(digits = -3)
) |>
  dplyr::mutate(
    risk_rating = dplyr::case_when(
      risk_rating == 1 ~ "AAA",
      risk_rating == 2 ~ "AA",
      risk_rating == 3 ~ "A",
      risk_rating == 4 ~ "BBB",
      risk_rating == 5 ~ "BB",
      risk_rating == 6 ~ "B",
      risk_rating == 7 ~ "CCC"
    )
  ) |>
  dplyr::mutate(risk_rating = factor(
    risk_rating,
    ordered = TRUE,
    levels = c("AAA", "AA", "A", "BBB", "BB", "B", "CCC")
  ))

usethis::use_data(mock_credit, overwrite = TRUE)
