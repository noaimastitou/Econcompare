# econcompare

**Interactive OLS-reference comparison for cross-sectional econometrics in R.**

`econcompare` is an experimental R package for estimating, standardising and comparing cross-sectional econometric models against an ordinary least squares reference. It combines a programmatic API with an interactive Shiny workspace so that model configuration, estimation choices, fit statistics, diagnostics and warnings can be inspected in one place.

The package is designed for applied econometrics, research workflows and advanced teaching. The interactive interface is intentionally approachable, but it does not hide model assumptions or estimator-specific limitations.

## Current scope

Version 0.6.0 focuses on **cross-sectional models whose results remain meaningfully interpretable relative to an OLS baseline**.

Supported engines currently include:

- Ordinary least squares (OLS)
- Weighted least squares (WLS)
- OLS with heteroskedasticity-robust standard errors
- Robust M-estimation
- OLS through `fixest::feols()`
- Instrumental variables / 2SLS
- Quantile regression
- Tobit / censored regression
- Heckman sample-selection models

Use:

```r
eco_models()
```

to inspect the catalogue, required packages and local availability.

## Installation from GitHub

Until a CRAN release is available, install the development version from GitHub.

```r
install.packages("remotes")
remotes::install_github(
  "noaimastitou/econcompare",
  dependencies = TRUE
)
```

Replace `YOUR_GITHUB_USERNAME` with the GitHub account that hosts the repository.

Then load the package:

```r
library(econcompare)
```

## Interactive econometrics workspace

The simplest workflow is:

```r
library(econcompare)
eco_app(mtcars)
```

The application opens an interactive workspace where you can:

1. choose the dependent variable and regressors;
2. add models to the OLS reference specification;
3. configure estimator-specific parameters;
4. estimate all selected specifications;
5. compare coefficients side by side;
6. inspect fit statistics and diagnostics;
7. review estimator warnings explicitly;
8. preview the underlying data.

Contextual `?` helpers explain the econometric purpose of model-specific controls without replacing formal methodological judgement.

## Programmatic workflow

`econcompare` can also be used without the graphical interface.

```r
library(econcompare)

fit <- eco_run(
  data = mtcars,
  formula = mpg ~ wt + hp,
  models = c("ols", "ols_robust", "quantile"),
  model_args = list(
    ols_robust = list(se_type = "HC3"),
    quantile = list(tau = c(0.25, 0.50, 0.75))
  )
)

eco_compare(fit)
eco_diagnostics(fit)
eco_view(fit)
```

OLS is automatically included when an alternative estimator is requested.

## Instrumental variables example

```r
fit_iv <- eco_run(
  data = mtcars,
  formula = mpg ~ wt + hp,
  models = c("ols", "ivreg"),
  iv_formula = mpg ~ wt + hp | wt + qsec
)

eco_view(fit_iv)
```

The example is only intended to demonstrate the API. The validity of `qsec` as an instrument is **not established** by the package or by this example. Instrument relevance and exclusion remain substantive econometric requirements.

## Missing and estimator-specific statistics

Not every estimator defines the same fit statistics. `econcompare` does not manufacture a common statistic when none exists.

By default, unavailable or length-zero scalar statistics are normalised to `NA_real_` and listed in `unavailable_stats`:

```r
eco_compare(fit, empty_stats = "na")
```

For package development or strict auditing, use:

```r
eco_compare(fit, empty_stats = "error")
```

This stops when an expected scalar statistic is returned with length zero.

## Optional dependencies

Some model engines rely on specialised packages. The principal optional dependencies are:

```r
install.packages(c(
  "shiny",
  "estimatr",
  "fixest",
  "ivreg",
  "quantreg",
  "censReg",
  "sampleSelection"
))
```

`MASS` is used for robust M-estimation and is included with standard R distributions as a recommended package.

## Project status

`econcompare` is currently a **beta research software project**. The cross-sectional workflow is under active development. Time-series and panel-data extensions are intentionally outside the current scope and are planned as separate stages so that the cross-sectional interface and comparison logic can stabilise first.

Before relying on the package in production or published empirical work, inspect estimator warnings, verify the underlying model assumptions and validate results against the original model package when appropriate.

## Contributing

Bug reports, reproducible examples, methodological suggestions and interface feedback are welcome. See [`CONTRIBUTING.md`](CONTRIBUTING.md).

## Licence

MIT. See `LICENSE` and `LICENSE.md`.
