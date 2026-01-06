# Summarize the migration of a data frame

`migrate()` summarizes the transition amount (or percentage) of a
numeric variable from each beginning credit risk state category to each
ending credit risk state, given a data frame input.

## Usage

``` r
migrate(
  data,
  id,
  time,
  state,
  metric = NULL,
  percent = TRUE,
  fill_state = NULL,
  verbose = TRUE
)
```

## Arguments

- data:

  A data frame or data frame extension (e.g., a tibble or data.table)
  containing a minimum of three (3) column variables representing a
  time, a credit risk state, and an ID identifying the credit facility
  (we would expect to see most unique values in this column variable
  appear twice in the dataset; once at the first unique `time` value and
  again at the second unique `time` value, unless the ID only existed at
  one of those two times).

- id:

  The column variable of the `data` data frame argument that contains
  the unique identifier to track where a particular credit facility
  migrated to/from. If left null, `migrate()` will attempt to use the
  first column variable from the data frame provided in the `data`
  argument.

- time:

  The column variable of in the `data` data frame representing the
  timepoint (e.g., a Date) of each observation; this column should
  contain two unique values (migration from Time A to Time B)

- state:

  The column variable of the `data` data frame argument that contains
  the credit risk state values.

- metric:

  (Optional) The column variable of type "numeric" in the `data` data
  frame argument that contains the continuous metric values to weight
  the state migration by

- percent:

  If `FALSE`, will calculate the migration on an absolute basis (rather
  than a percentage basis, which is the default)

- fill_state:

  (Optional) A value (e.g., a character string such as "No Rating" or
  "NR") to be used as the *filler* `state` for any `id` values that only
  exist at one timepoint in the `data`.

- verbose:

  If `TRUE`, the function returns an informational message about the
  transition period

## Value

A data frame containing three (3) column variables representing the
unique combinations of starting & ending credit risk states and the
calculated migration observed during the period.

## Examples

``` r
# Return the percent migration of the number of credit facilities
migrate(
  data = mock_credit,
  id = customer_id,
  time = date,
  state = risk_rating
)
#> ℹ Migrating from 2020-06-30 to 2020-09-30
#> # A tibble: 49 × 3
#>    risk_rating_start risk_rating_end   prop
#>    <ord>             <ord>            <dbl>
#>  1 AAA               AAA             0.774 
#>  2 AAA               AA              0.194 
#>  3 AAA               A               0.0323
#>  4 AAA               BBB             0     
#>  5 AAA               BB              0     
#>  6 AAA               B               0     
#>  7 AAA               CCC             0     
#>  8 AA                AAA             0.101 
#>  9 AA                AA              0.663 
#> 10 AA                A               0.157 
#> # ℹ 39 more rows

# Return the absolute migration in `principal_balance`
migrate(
  data = mock_credit,
  id = customer_id,
  time = date,
  state = risk_rating,
  metric = principal_balance,
  percent = FALSE
)
#> ℹ Migrating from 2020-06-30 to 2020-09-30
#> # A tibble: 49 × 3
#>    risk_rating_start risk_rating_end principal_balance
#>    <ord>             <ord>                       <dbl>
#>  1 AAA               AAA                      29042000
#>  2 AAA               AA                        6575000
#>  3 AAA               A                           20000
#>  4 AAA               BBB                             0
#>  5 AAA               BB                              0
#>  6 AAA               B                               0
#>  7 AAA               CCC                             0
#>  8 AA                AAA                       6445000
#>  9 AA                AA                       58095000
#> 10 AA                A                        13045000
#> # ℹ 39 more rows

# Provide a filler `state` value when a unique `id` is missing a timepoint
migrate(
  data = head(mock_credit, n = 995),   # drop the last 5 observations
  id = customer_id,
  time = date,
  state = risk_rating,
  fill_state = "NR",
  percent = FALSE
)
#> ℹ Migrating from 2020-06-30 to 2020-09-30
#> ℹ 5 IDs have a missing timepoint:
#>   • Migrating 5 IDs with missing end timepoint to new class 'NR'
#>   • Migrating 0 IDs with missing start timepoint from new class 'NR'
#> # A tibble: 64 × 3
#>    risk_rating_start risk_rating_end count
#>    <ord>             <ord>           <int>
#>  1 AAA               AAA                24
#>  2 AAA               AA                  5
#>  3 AAA               A                   1
#>  4 AAA               BBB                 0
#>  5 AAA               BB                  0
#>  6 AAA               B                   0
#>  7 AAA               CCC                 0
#>  8 AAA               NR                  1
#>  9 AA                AAA                 9
#> 10 AA                AA                 59
#> # ℹ 54 more rows
```
