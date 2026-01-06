# Mock dataset containing credit statistics by customer at two time intervals. Some customers only exist in one time interval (they either became a customer after the first time interval, or discontinued being a customer before the second time interval).

Mock dataset containing credit statistics by customer at two time
intervals. Some customers only exist in one time interval (they either
became a customer after the first time interval, or discontinued being a
customer before the second time interval).

## Usage

``` r
mock_credit
```

## Format

A data frame with columns:

- customer_id:

  Customer ID for 497 unique customers.

- date:

  Date of the observation; there are two unique dates in the dataset.

- risk_rating:

  Factor representing risk level on a 1-14 scale.

- principal_balance:

  Principal balance outstanding on the loan.

## Source

Developed by Ketchbrook Analytics

## Examples

``` r
if (FALSE) { # \dontrun{
 mock_credit
} # }
```
