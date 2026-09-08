# Contributing to econcompare

Contributions are welcome, especially when they improve econometric validity, estimator interoperability, diagnostics, reproducibility or the interactive workflow.

## Before opening an issue

Please include:

- your R version;
- your `econcompare` version;
- the relevant optional package versions;
- a minimal reproducible example where possible;
- the full warning or error message;
- the expected behaviour and the observed behaviour.

## Model contributions

A new estimator should not be added only because an R implementation exists. The current design principle is that the estimator should have a clear and defensible relationship to the OLS reference specification used by `econcompare`.

A proposed model should therefore document:

1. why comparison with the OLS baseline is meaningful;
2. which coefficients or marginal quantities are comparable;
3. which fit statistics are valid and which are not;
4. estimator-specific diagnostics that should be surfaced;
5. the underlying R package used for estimation.

## Pull requests

Please keep pull requests focused. Add or update tests when changing extraction logic, model registration or user-facing behaviour.

Before submitting a pull request, run:

```r
install.packages(c("devtools", "testthat"))
devtools::test()
devtools::check()
```

Warnings from third-party estimators should be preserved or explicitly handled rather than silently discarded.
