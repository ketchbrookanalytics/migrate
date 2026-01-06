# migrate

## Using {migrate}

This package is intended to serve as a set of tools to help convert
credit risk data at two timepoints into traditional state transition
matrices. At a higher level, {migrate} is intended to help an analyst
understand how risk moved in their credit portfolio over a time
interval.

## Background

One of the more difficult aspects of making a state migration matrix in
R (or Python, for that matter) is the fact that the output doesn’t
satisfy the structure of a traditional data frame object. Rather, the
output needs to be a *matrix*, which is a data structure that R does
support. In the past, there has been difficulty converting a matrix to
something more visual-friendly. More recently, however, tools like the
[kableExtra](https://cran.r-project.org/package=kableExtra) and
[gt](https://cran.r-project.org/package=gt) packages allow us to present
visually appealing output that extends the structure of a data frame.
Using the matrix-style output of {migrate}’s functions with a visual
formatting package such as the two mentioned above will hopefully help
analysts streamline the presentation of their credit portfolio’s state
migration matrices to an audience.

## Getting Started

If you haven’t done so already, first install {migrate} with the
instructions in the [README
section](https://github.com/ketchbrookanalytics/migrate#Installation).

First, load the package using
[`library()`](https://rdrr.io/r/base/library.html)

``` r
library(migrate)
```

The package has a built-in mock dataset, which can be loaded into the
environment like so:

``` r
data("mock_credit")

head(mock_credit[order(mock_credit$customer_id), ])   # sort by 'customer_id'
```

| customer_id   | date       | risk_rating | principal_balance |
|:--------------|:-----------|:------------|------------------:|
| Customer_1001 | 2020-06-30 | A           |            915000 |
| Customer_1001 | 2020-09-30 | A           |           1328000 |
| Customer_1002 | 2020-06-30 | AAA         |            979000 |
| Customer_1002 | 2020-09-30 | AAA         |            354000 |
| Customer_1003 | 2020-06-30 | BBB         |           1400000 |
| Customer_1003 | 2020-09-30 | BBB         |            356000 |

Note that an important feature of the `mock_credit` dataset is that
there are exactly two (2) unique values in the `date` column variable;
if the `time` argument passed to
[`migrate()`](https://ketchbrookanalytics.github.io/migrate/reference/migrate.md)
has more than two (2) unique values, the function will throw an error.

``` r
unique(mock_credit$date)
#> [1] "2020-06-30" "2020-09-30"
```

To summarize the migration within the data, use the
[`migrate()`](https://ketchbrookanalytics.github.io/migrate/reference/migrate.md)
function

``` r
migrated_df <- migrate(
  data = mock_credit,
  id = customer_id,
  time = date,
  state = risk_rating,
)
#> ℹ Migrating from 2020-06-30 to 2020-09-30
head(migrated_df)
#> # A tibble: 6 × 3
#>   risk_rating_start risk_rating_end   prop
#>   <ord>             <ord>            <dbl>
#> 1 AAA               AAA             0.774 
#> 2 AAA               AA              0.194 
#> 3 AAA               A               0.0323
#> 4 AAA               BBB             0     
#> 5 AAA               BB              0     
#> 6 AAA               B               0
```

To create the state transition matrix, use the
[`build_matrix()`](https://ketchbrookanalytics.github.io/migrate/reference/build_matrix.md)
function

``` r
build_matrix(migrated_df)
#> ℹ Using `risk_rating_start` as the 'state_start' column variable
#> ℹ Using `risk_rating_end` as the 'state_end' column variable
#> ℹ Using `prop` as the 'metric' column variable
#>             AAA         AA          A        BBB         BB          B        CCC
#> AAA 0.774193548 0.19354839 0.03225806 0.00000000 0.00000000 0.00000000 0.00000000
#> AA  0.101123596 0.66292135 0.15730337 0.07865169 0.00000000 0.00000000 0.00000000
#> A   0.008333333 0.06666667 0.72500000 0.16666667 0.03333333 0.00000000 0.00000000
#> BBB 0.000000000 0.00000000 0.11363636 0.68181818 0.14772727 0.05681818 0.00000000
#> BB  0.000000000 0.00000000 0.00000000 0.11392405 0.63291139 0.16455696 0.08860759
#> B   0.000000000 0.00000000 0.00000000 0.01388889 0.09722222 0.62500000 0.26388889
#> CCC 0.000000000 0.00000000 0.00000000 0.00000000 0.00000000 0.14285714 0.85714286
```

Or, to do it all in one shot, use the `|>`

``` r
mock_credit |>
  migrate(
    id = customer_id,
    time = date,
    state = risk_rating,
    metric = principal_balance,
    percent = FALSE,
    verbose = FALSE
  ) |>
  build_matrix(
    state_start = risk_rating_start,
    state_end = risk_rating_end,
    metric = principal_balance
  )
#>          AAA       AA        A      BBB       BB        B      CCC
#> AAA 29042000  6575000    20000        0        0        0        0
#> AA   6445000 58095000 13045000 14467000        0        0        0
#> A     804000  7898000 85330000 21015000  5829000        0        0
#> BBB        0        0 12461000 65315000 13911000  8140000        0
#> BB         0        0        0 11374000 45986000 14057000  5723000
#> B          0        0        0   413000  6700000 47402000 17132000
#> CCC        0        0        0        0        0  2094000 14843000
```

## Handle IDs with observations at a single timepoint

The following code creates a dataframe that features 500 customers with
the following characteristics:

- 470 customers have a value at both timepoints
- 20 customers have a value only at the first timepoint
- 10 customers have a value only at the second timepoint

``` r
mock_credit_with_missing <- mock_credit |>
  # Remove the value at the first timepoint for 10 customers
  dplyr::slice(-(1:10)) |>
  # Remove the value at the last timepoint for 20 customers
  dplyr::slice(-((dplyr::n() - 19):dplyr::n()))
```

Check that the new dataframe has information about 500 customers:

``` r
# Number of unique customer_id values in mock_credit_with_missing
dplyr::n_distinct(mock_credit_with_missing$customer_id)
#> [1] 500
```

By default,
[`migrate()`](https://ketchbrookanalytics.github.io/migrate/reference/migrate.md)
drops observations that belong to IDs found at a single timepoint.
[`migrate()`](https://ketchbrookanalytics.github.io/migrate/reference/migrate.md)
informs such behavior through a warning:

``` r
migrated_data_without_fill_state <- mock_credit_with_missing |>
  migrate(
    id = customer_id,
    time = date,
    state = risk_rating,
    percent = FALSE,
    verbose = FALSE
  )
#> Warning: ! Removed 30 observations due to missingness or IDs only existing at one `time` value
```

Notice that only 470 customers have been migrated:

``` r
migrated_data_without_fill_state |>
  dplyr::pull(count) |>
  sum()
#> [1] 470
```

You can use
[`migrate()`](https://ketchbrookanalytics.github.io/migrate/reference/migrate.md)’s
`fill_state` argument to ensure that no information is lost during the
migration process. When a *filler state* value (e.g., a character string
such as “No Rating” or “NR”) is assigned to `fill_state`, IDs with a
single timepoint are not removed but rather migrated from or to this
*filler state*.

When `verbose = TRUE` a message will provide additional information
about the IDs with missing timepoints:

``` r
migrated_data_with_fill_state <- mock_credit_with_missing |>
  migrate(
    id = customer_id,
    time = date,
    state = risk_rating,
    fill_state = "No Rating",
    percent = FALSE,
    verbose = TRUE
  )
#> ℹ Migrating from 2020-06-30 to 2020-09-30
#> ℹ 30 IDs have a missing timepoint:
#>   • Migrating 20 IDs with missing end timepoint to new class 'No Rating'
#>   • Migrating 10 IDs with missing start timepoint from new class 'No Rating'
```

Check that 500 customers were migrated:

``` r
migrated_data_with_fill_state |>
  dplyr::pull(count) |>
  sum()
#> [1] 500
```

So far we have been using `count` as the metric to easily determine the
amount of customers that migrated in each scenario. The following code
provides an example migration that leverages `principal_balance` as the
metric:

``` r
mock_credit_with_missing |>
  migrate(
    id = customer_id,
    time = date,
    state = risk_rating,
    metric = principal_balance,
    fill_state = "No Rating",
    percent = FALSE,
    verbose = FALSE
  ) |>
  build_matrix(
    state_start = risk_rating_start,
    state_end = risk_rating_end,
    metric = principal_balance
  )
#>                AAA       AA        A      BBB       BB        B      CCC No Rating
#> AAA       27403000  4301000    20000        0        0        0        0   2274000
#> AA         6445000 56333000 11642000 14467000        0        0        0   1762000
#> A           804000  7682000 82044000 19823000  5829000        0        0   3779000
#> BBB              0        0 12461000 62071000 11480000  8140000        0   4275000
#> BB               0        0        0 10930000 44487000 11838000  5723000   1995000
#> B                0        0        0        0  6700000 46412000 14977000   3210000
#> CCC              0        0        0        0        0  2094000 14447000         0
#> No Rating        0        0        0        0        0        0        0         0
```
