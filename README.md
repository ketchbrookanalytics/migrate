
<!-- README.md is generated from README.Rmd. Please edit that file -->

# migrate <img src='man/figures/logo.png' align="right" height="200" />

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/mthomas-ketchbrook/migrate.svg?branch=master)](https://travis-ci.com/mthomas-ketchbrook/migrate)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-orange.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![CRAN
status](https://www.r-pkg.org/badges/version/migrate)](https://CRAN.R-project.org/package=migrate)
[![metacran
downloads](https://cranlogs.r-pkg.org/badges/migrate)](https://cran.r-project.org/package=migrate)
<!-- badges: end -->

The goal of migrate is to provide credit analysts with an easy set of
tools for building *state migration matrices* (also known as *“state
transition matrices”*).

<br>

![](man/figures/gt_tbl.png)

## Methodology

`migrate` provides an easy way to calculate absolute or percentage
migration within a credit portfolio. The above image shows a typical
credit migration matrix using the *absolute* approach; each cell in the
grid represents the total balance in the portfolio at 2020-06-30 that
started at the Risk Rating represented on the left-hand vertical axis
and ended (at 2020-09-30) at the Risk Rating represented on the upper
horizontal axis of the matrix. For example, $6.58M moved from a Risk
Rating **AAA** at 2020-06-30 to a Risk Rating **AA** at 2020-09-30.

While the above, *absolute*, migration example is typically more of a
reporting function, the *percentage* (or probabilistic) methodology is
often more of a statistical credit risk modeling exercise. Currently,
this package only supports the simple “cohort” methodology. This
estimates the probability of moving from state *i* to state *j* in a
single time step, echoing a Markov process. We can visualize this in a
matrix, for a credit portfolio with *N* unique, ordinal states:

![](man/figures/markov_matrix.png)

### Future Plans for `migrate`

Future development plans for this package include building functionality
for the more complex **duration**/**hazard** methodology, including both
the *time-homogeneous* and *non-homogeneous* implementations.

## Installation

You can install the released version of migrate from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("migrate")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mthomas-ketchbrook/migrate")
```

## Example

First, load the package & the mock dataset (as a data frame) using
`library()`

``` r
library(migrate)
data("mock_credit")
```

We can get a look at the data using `dplyr::glimpse()`

``` r
library(dplyr)
glimpse(mock_credit)
#> Rows: 1,000
#> Columns: 4
#> $ customer_id       <chr> "Customer_1001", "Customer_1002", "Customer_1003", "Customer_1004", "Customer_1005", "Customer_1006", "Customer_1007", "Customer_1008", "Customer_1009", "Customer_1010", "Customer_1011", "Customer_1012", "Customer_1013", "Customer_1014", "Customer_1015", "Customer_1016", "Customer_1017", "Customer_1018", "Customer_1019", "Customer_1020", "Customer_1021", "Customer_1022", "Customer_1023", "Customer_1024", "Customer_1025", "Customer_1026", "Customer_1027", "Customer_1028", "Customer_1029", "Customer_1030", "Customer_1031", "Customer_1032", "Customer_1033", "Customer_1034", "Customer_1035", "Customer_1036", "Customer_1037", "Customer_1038", "Customer_1039", "Customer_1040", "Customer_1041", "Customer_1042", "Customer_1043", "Customer_1044", "Customer_1045", "Customer_1046", "Customer_1047", "Customer_1048", "Customer_1049", "Customer_1050", "Customer_1051", "Customer_1052", "Customer_1053", "Customer_1054", "Customer_1055", "Customer_1056", "Customer_1057",...
#> $ date              <date> 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06-30, 2020-06...
#> $ risk_rating       <ord> A, AAA, BBB, BB, AA, BB, CCC, BB, AAA, B, A, A, BBB, A, A, A, AA, CCC, B, A, AA, BBB, A, AA, B, AA, BBB, B, BBB, A, AA, BB, B, BB, BBB, A, CCC, BBB, B, A, BB, A, A, BBB, A, B, BB, BB, BB, B, BBB, CCC, B, BB, A, AA, AA, BBB, AA, A, CCC, BB, AA, B, BB, A, B, A, AAA, B, B, BBB, A, BBB, A, A, AA, CCC, A, AAA, A, A, B, AA, BB, AA, BBB, BBB, BBB, AA, B, BBB, BBB, CCC, BB, A, BB, AAA, AA, BB, BB, A, BBB, BB, AA, AAA, B, AA, BBB, B, BBB, BB, AA, B, BBB, AA, AA, AA, A, A, A, B, B, A, BBB, BBB, B, BBB, AA, B, BB, B, AAA, B, BB, A, A, BB, BBB, BBB, AA, BBB, A, AA, BB, A, A, AA, B, B, BBB, BB, A, BB, B, BB, AA, B, A, BBB, AAA, AA, BB, BB, AA, BB, BB, BB, BBB, B, BBB, AA, AAA, AA, BB, A, AA, AAA, B, A, A, B, A, CCC, BB, BBB, A, BBB, AA, B, AA, BB, AA, AA, A, B, BBB, CCC, BBB, BBB, AAA, B, BBB, AA, BBB, B, BB, A, BB, CCC, BB, CCC, BBB, B, AAA, A, AAA, B, AA, AAA, A, AA, A, BBB, A, AA, B, B, AA, A, A, BBB, A, AA, A, AA, BBB, B, A, AA, A, AAA, BB, BB, BB, A, A, B, BB, A...
#> $ principal_balance <dbl> 915000, 979000, 1400000, 627000, 1403000, 1096000, 396000, 444000, 660000, 348000, 905000, 1054000, 909000, 895000, 2008000, 1281000, 1165000, 1390000, 1557000, 211000, 612000, 844000, 1211000, 693000, 1989000, 1134000, 1569000, 1076000, 514000, 952000, 2511000, 700000, 1200000, 771000, 2090000, 1513000, 155000, 2398000, 1530000, 752000, 22000, 1253000, 878000, 803000, 808000, 351000, 1005000, 1083000, 480000, 1733000, 778000, 1854000, 96000, 2023000, 892000, 1623000, 404000, 1652000, 1601000, 885000, 1387000, 1087000, 316000, 1488000, 992000, 478000, 1066000, 1432000, 375000, 565000, 282000, 1212000, 1616000, 1613000, 357000, 1577000, 1971000, 203000, 1315000, 20000, 284000, 2248000, 2048000, 2105000, 1024000, 875000, 606000, 1176000, 332000, 269000, 1124000, 102000, 913000, 787000, 2990000, 1617000, 259000, 760000, 1756000, 1283000, 2720000, 2386000, 501000, 303000, 315000, 1977000, 807000, 1646000, 1093000, 1346000, 3739000, 74000, 383000, 1001000, 98...
```

Note that an important feature of the dataset is that there are exactly
two (2) unique values in the `date` column variable

``` r
unique(mock_credit$date)
#> [1] "2020-06-30" "2020-09-30"
```

To summarize the migration within the data, use the `migrate()` function

``` r
migrated_df <- migrate(
  data = mock_credit, 
  date = date, 
  state = risk_rating, 
  id = customer_id
)

head(migrated_df)
#> # A tibble: 6 x 3
#>   risk_rating_start risk_rating_end  count
#>   <fct>             <fct>            <dbl>
#> 1 AAA               AAA             0.774 
#> 2 AAA               AA              0.194 
#> 3 AAA               A               0.0323
#> 4 AAA               BBB             0     
#> 5 AAA               BB              0     
#> 6 AAA               B               0
```

To create the state migration matrix, use the `build_matrix()` function

``` r
build_matrix(migrated_df)
#> Using  risk_rating_start  as the 'state_start' column variable
#> 
#> Using  risk_rating_end  as the 'state_end' column variable
#> 
#> Using  count  as the 'metric' column variable
#> 
#>             AAA         AA          A        BBB         BB          B        CCC
#> AAA 0.774193548 0.19354839 0.03225806 0.00000000 0.00000000 0.00000000 0.00000000
#> AA  0.101123596 0.66292135 0.15730337 0.07865169 0.00000000 0.00000000 0.00000000
#> A   0.008333333 0.06666667 0.72500000 0.16666667 0.03333333 0.00000000 0.00000000
#> BBB 0.000000000 0.00000000 0.11363636 0.68181818 0.14772727 0.05681818 0.00000000
#> BB  0.000000000 0.00000000 0.00000000 0.11392405 0.63291139 0.16455696 0.08860759
#> B   0.000000000 0.00000000 0.00000000 0.01388889 0.09722222 0.62500000 0.26388889
#> CCC 0.000000000 0.00000000 0.00000000 0.00000000 0.00000000 0.14285714 0.85714286
```

Or, to do it all in one shot, use the `%>%`

``` r
mock_credit %>% 
  migrate(
    date = date, 
    state = risk_rating, 
    id = customer_id, 
    metric = principal_balance, 
    percent = FALSE
  ) %>% 
  build_matrix()
#> Using  risk_rating_start  as the 'state_start' column variable
#> 
#> Using  risk_rating_end  as the 'state_end' column variable
#> 
#> Using  principal_balance  as the 'metric' column variable
#> 
#>          AAA       AA        A      BBB       BB        B      CCC
#> AAA 29042000  6575000    20000        0        0        0        0
#> AA   6445000 58095000 13045000 14467000        0        0        0
#> A     804000  7898000 85330000 21015000  5829000        0        0
#> BBB        0        0 12461000 65315000 13911000  8140000        0
#> BB         0        0        0 11374000 45986000 14057000  5723000
#> B          0        0        0   413000  6700000 47402000 17132000
#> CCC        0        0        0        0        0  2094000 14843000
```
