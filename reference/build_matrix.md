# Build a state migration (transition) matrix

`build_matrix()` creates a state transition matrix from summarized data
(i.e., a data frame returned by
[`migrate()`](https://ketchbrookanalytics.github.io/migrate/reference/migrate.md))
representing each unique combination of beginning & ending states and a
numeric metric.

## Usage

``` r
build_matrix(data, state_start = NULL, state_end = NULL, metric = NULL)
```

## Arguments

- data:

  A data frame or data frame extension (e.g., a tibble or data.table)
  containing a minimum of three (3) column variables representing a
  starting credit risk state, an ending credit risk state, and a metric
  containing values representing the movement (i.e., "transition) in
  that metric between the starting credit risk state point in time and
  the ending credit risk state point in time. This style of data frame
  is output by the
  [`migrate()`](https://ketchbrookanalytics.github.io/migrate/reference/migrate.md)
  function within this package.

- state_start:

  (Optional) A symbol or string, representing the column variable of the
  `data` data frame argument that contains the starting credit risk
  state values. If left null, function will attempt to find it for you.

- state_end:

  (Optional) A symbol or string, representing the column variable of the
  `data` data frame argument that contains the starting credit risk
  state values. If left null, function will attempt to find it for you.

- metric:

  (Optional) A symbol or string, representing the column variable of the
  `data` data frame argument that contains the metric for which the
  grouped difference in value between the starting credit risk state
  period and ending credit risk state period was computed.

## Value

A matrix object, where the first (row) dimension represents the starting
credit risk state, the second (column) dimension represents the ending
credit risk state, and the values within the matrix represent the
transitioned amount based upon the values in the `metric` numeric column
variable from the `data` data frame.

Note: A matrix object can be coerced to a data frame using
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

## Examples

``` r
# Let `build_matrix()` guess which column variables represent `state_start`,
# `state_end` and `metric`
mock_credit |>
  migrate(
    time = date,
    state = risk_rating,
    id = customer_id,
    metric = principal_balance
  ) |>
  build_matrix()
#> ℹ Migrating from 2020-06-30 to 2020-09-30
#> ℹ Using `risk_rating_start` as the 'state_start' column variable
#> ℹ Using `risk_rating_end` as the 'state_end' column variable
#> ℹ Using `principal_balance` as the 'metric' column variable
#>             AAA         AA            A         BBB         BB          B
#> AAA 0.814939529 0.18449926 0.0005612145 0.000000000 0.00000000 0.00000000
#> AA  0.070014774 0.63111068 0.1417133794 0.157161170 0.00000000 0.00000000
#> A   0.006651444 0.06533969 0.7059300440 0.173855852 0.04822297 0.00000000
#> BBB 0.000000000 0.00000000 0.1248259489 0.654281908 0.13935108 0.08154107
#> BB  0.000000000 0.00000000 0.0000000000 0.147446202 0.59613689 0.18222712
#> B   0.000000000 0.00000000 0.0000000000 0.005764373 0.09351403 0.66160481
#> CCC 0.000000000 0.00000000 0.0000000000 0.000000000 0.00000000 0.12363465
#>            CCC
#> AAA 0.00000000
#> AA  0.00000000
#> A   0.00000000
#> BBB 0.00000000
#> BB  0.07418978
#> B   0.23911678
#> CCC 0.87636535

# Specify which column variables represent `state_start`, `state_end` and
# `metric`
mock_credit |>
  migrate(
    id = customer_id,
    time = date,
    state = risk_rating,
    percent = FALSE
  ) |>
  build_matrix(
    state_start = risk_rating_start,
    state_end = risk_rating_end,
    metric = count
  )
#> ℹ Migrating from 2020-06-30 to 2020-09-30
#>     AAA AA  A BBB BB  B CCC
#> AAA  24  6  1   0  0  0   0
#> AA    9 59 14   7  0  0   0
#> A     1  8 87  20  4  0   0
#> BBB   0  0 10  60 13  5   0
#> BB    0  0  0   9 50 13   7
#> B     0  0  0   1  7 45  19
#> CCC   0  0  0   0  0  3  18
```
