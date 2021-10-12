# migrate 0.4.0

* This release mainly aims to resolve deprecated quasiquotation with `dplyr::enquo` and `(!! var)` in favor of the `({{ var }})` syntax. 
* This is the first release where we have adequate testing coverage.

## Breaking Changes

* The `date` argument in `migrate()` has been replaced (renamed) in favor of `time`

The term *date* seemed too specific to use in the `migrate()` function, so it was replaced by the more general term, *time*. Instead of holding users to using Date-type column variables, this change allows them flexibility to migrate from Time *A* --> Time *B*, for example. 

## Other Features & Improvements

* `migrate()` has a new `verbose` argument (logical TRUE/FALSE), which informs the user of the time horizon over which the migration is being calculated:

```{r}
migrate(
  data = mock_credit,
  id = customer_id,
  time = date,
  state = risk_rating
)
# > === Migrating from: `2020-06-30` --> `2020-09-30` ===
# > ... [output] ...
```

* {migrate} is now more light-weight: the dependencies on {crayon} & {stringr} have been removed 

# migrate 0.3.0
* Deprecated *rating* argument in `migrate()`, renamed to *state*
* Added back *id* argument in `migrate()`
* Revised **mock_credit** dataset
* `migrate()` defaults to percentage migration, instead of absolute

# migrate 0.2.0
* Patch fix for `migrate()`
* Improvements to docs

# migrate 0.1.0
* Initial release
