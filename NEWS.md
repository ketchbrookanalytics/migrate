# migrate 0.5.0

## Enhancements

* A new `fill_state` argument to `migrate()` has been introduced, allowing users to migrate IDs that only exist at a single timepoint to a new or existing state
* Errors, warnings, and messages (which were previously handled exclusively by {rlang}) output in the console have been improved via the use of {cli}

## Bug Fixes

* Resolves an error that was thrown when attempting to coerce a `state` argument of type `character` to type `factor`

## Technical Changes

* The `date` argument in `migrate()` has been fully deprecated (replaced by `time` argument in v0.4.0)
* The {magrittr} pipe `%>%` has been replaced by the native pipe `|>` that was introduced in R 4.1. Due to this, the minimum R version required to install {migrate} has been bumped to R 4.1. Accordingly, {magrittr} was removed from Imports.
* {cli} and {glue} have been added as Imports

## Miscellaneous

* A {pkgdown} site for the package has been created at [https://ketchbrookanalytics.github.io/migrate/](https://ketchbrookanalytics.github.io/migrate/)
* License has been bumped to 2024, and copyright holder has been changed to Ketchbrook Analytics LLC
* GitHub repository has been transferred from [mthomas-ketchbrook](https://github.com/mthomas-ketchbrook) to [ketchbrookanalytics](https://github.com/ketchbrookanalytics)

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
