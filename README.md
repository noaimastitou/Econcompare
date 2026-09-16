# econcompare

**Outcome-aware econometric exploration for cross-sectional and explanatory time-series data in R.**

`econcompare` is a research-oriented R package for quickly exploring data, estimating a small set of candidate econometric models, and comparing their results in one interface before continuing the detailed analysis in dedicated R code.

The package is intentionally **comparison-first rather than automation-first**. It does not choose the “best” model for the researcher, and OLS is no longer imposed as a universal benchmark.

## Model groups

Version 0.11.0 preserves outcome-aware cross-sectional comparison and extends explanatory time-series econometrics from static/dynamic/ARDL regressions to explicit ECM and multivariate VAR/VECM systems, while keeping lag order, deterministic terms and cointegration rank under researcher control.

### Continuous outcomes

- Ordinary least squares (`ols`)
- Weighted least squares (`wls`)
- OLS with robust standard errors (`ols_robust`)
- Robust M-estimation (`robust_m`)
- OLS through `fixest::feols()` (`fixest`)
- Instrumental variables / 2SLS (`ivreg`)
- Quantile regression (`quantile`)
- Tobit / censored regression (`tobit`)
- Heckman sample-selection model (`heckman`)

### Binary outcomes

- Linear probability model (`lpm`)
- Logit (`logit`)
- Probit (`probit`)

For factor responses, the existing factor-level order determines the default 0/1 coding; for character responses, the event category must be supplied explicitly so econcompare does not make that substantive choice silently. Raw LPM, logit and probit coefficients are on different scales and should not be compared as if they were the same quantity. Use `eco_binary_compare()` for simple common in-sample prediction summaries.

### Nominal categorical outcomes

- Multinomial logit (`multinomial_logit`) via `nnet::multinom()`

The first factor level is the reference category. Numeric outcomes are **never converted to categories automatically**. If numbers are substantive category codes, convert them explicitly with `factor()` before fitting the multinomial model. Coefficients are reported by non-reference outcome category.

### Ordinal categorical outcomes

- Ordered logit (`ordered_logit`)
- Ordered probit (`ordered_probit`)

The programmatic API requires the dependent variable to be an **ordered factor**. In the Shiny app, the user explicitly selects the category order before estimation, from lowest to highest. econcompare deliberately does not infer a substantive ordering from labels or numeric codes.

Inspect the catalogue with:

```r
eco_models()
eco_models("binary")
eco_models("ordinal")
```

## Installation from GitHub

```r
install.packages("remotes")
remotes::install_github("noaimastitou/Econcompare", dependencies = TRUE)
```

Then:

```r
library(econcompare)
```

## Interactive workflow

```r
library(econcompare)
eco_app(mtcars)
```

The app follows a deliberately simple sequence:

1. explore the dataset;
2. choose the dependent variable;
3. choose whether the objective is continuous, binary, nominal categorical, or ordinal categorical;
4. choose only models compatible with that objective;
5. estimate and compare;
6. run a small number of optional diagnostics when useful;
7. continue the detailed econometric work outside econcompare.

The **Data explorer** remains intentionally lightweight: dimensions, missingness, simple variable summaries, distributions, a numeric scatterplot/correlation view, basic data-quality notices, and category counts/shares for binary, nominal and ordinal variables.

## Programmatic examples

### Continuous

```r
fit <- eco_run(
  mtcars,
  mpg ~ wt + hp,
  models = c("ols", "ols_robust")
)

eco_compare(fit)
```

OLS is not added automatically. If you want OLS, request it explicitly.

### Binary: LPM, logit and probit

```r
d <- mtcars
d$high_mpg <- as.integer(d$mpg > median(d$mpg))

fit_bin <- eco_run(
  d,
  high_mpg ~ wt + hp,
  models = c("lpm", "logit", "probit")
)

eco_compare(fit_bin)
eco_binary_compare(fit_bin)
```

For character binary outcomes, choose the event explicitly:

```r
d$high_label <- ifelse(d$high_mpg == 1, "high", "low")
fit_char <- eco_run(
  d,
  high_label ~ wt + hp,
  models = c("logit", "probit"),
  binary_event = "high"
)
```

### Nominal categorical: multinomial logit

```r
d <- mtcars
d$gear_f <- factor(d$gear)

fit_multi <- eco_run(
  d,
  gear_f ~ wt + hp,
  models = "multinomial_logit"
)

eco_compare(fit_multi)
```

### Ordinal categorical: ordered logit and probit

```r
d <- mtcars
d$gear_ord <- ordered(d$gear, levels = sort(unique(d$gear)))

fit_ord <- eco_run(
  d,
  gear_ord ~ wt + hp,
  models = c("ordered_logit", "ordered_probit")
)

eco_compare(fit_ord)
```


## Explanatory time-series econometrics (0.11.0)

`econcompare` does **not** aim to be a forecasting package. Time-series mode is designed to study relationships among variables observed over time. The workflow is: detect/confirm the time index, audit temporal structure, explore trajectories, choose an explicit lag specification, estimate, compare, and diagnose.

```r
d <- data.frame(
  year = 2000:2025,
  education_spending = rnorm(26),
  unemployment = rnorm(26),
  pisa_score = rnorm(26)
)

eco_data_structure(d)
eco_time_audit(d, "year")

fit <- eco_time_run(
  d,
  pisa_score ~ education_spending,
  time = "year",
  models = c("time_static", "distributed_lag", "dynamic_regression", "ardl"),
  p = 1,
  q = 2,
  error_policy = "collect"
)

eco_compare(fit, error_policy = "collect")
eco_time_diagnostics(fit, bg_order = 1)
```

Variable-specific explanatory lags are available programmatically while `q` remains the fallback for unspecified regressors:

```r
fit_lags <- eco_time_run(
  d,
  pisa_score ~ education_spending + unemployment,
  time = "year",
  models = "ardl",
  p = 1,
  q = 1,
  q_by_var = c(education_spending = 4, unemployment = 1)
)
```

ADF deterministic components are explicit research choices. ADF uses `urca` and reports its critical values directly; KPSS remains complementary rather than an automatic transformation rule:

```r
eco_stationarity_tests(
  d,
  variables = c("pisa_score", "education_spending"),
  time = "year",
  adf_k = 1,
  adf_deterministic = "trend",
  kpss_null = "Trend"
)
```

Temporal detection is deliberately conservative. A date-like column does not automatically make a dataset a time series: duplicated or irregular event dates are flagged, ambiguous date strings are not guessed, and the Shiny app asks the researcher to confirm the econometric mode explicitly. Calendar monthly/quarterly/annual spacing is detected from calendar positions rather than fixed month lengths. Lagged models refuse gaps instead of treating the previous observed row as a one-period lag.

### ECM

`eco_ecm_run()` estimates a transparent two-step single-equation ECM. The first-step levels relationship is retained separately and is **not** treated as automatic proof of cointegration.

```r
ecm <- eco_ecm_run(
  d,
  pisa_score ~ education_spending + unemployment,
  time = "year",
  p = 1,
  q = 0
)

eco_compare(ecm)          # short-run ECM
eco_ecm_long_run(ecm)     # levels relationship used to build ECT
```

### VAR and VECM systems

VAR/VECM use a separate system API because no variable is treated as the single privileged dependent variable.

Use `eco_system_models()` to inspect the system engines and whether their optional dependencies are available locally.

```r
lag_evidence <- eco_var_lag_selection(
  d,
  c("pisa_score", "education_spending", "unemployment"),
  time = "year",
  lag_max = 4
)

var_fit <- eco_system_run(
  d,
  variables = c("pisa_score", "education_spending", "unemployment"),
  time = "year",
  model = "var",
  p = 2
)

eco_compare(var_fit)
eco_system_diagnostics(var_fit)
```

For VECM, inspect Johansen evidence first, then provide the rank explicitly:

```r
eco_johansen_test(
  d,
  c("pisa_score", "education_spending", "unemployment"),
  time = "year",
  K = 2,
  type = "trace",
  ecdet = "const"
)

vecm_fit <- eco_system_run(
  d,
  variables = c("pisa_score", "education_spending", "unemployment"),
  time = "year",
  model = "vecm",
  p = 2,
  rank = 1,
  johansen_type = "trace",
  ecdet = "const"
)

eco_vecm_rank_test(vecm_fit)
eco_vecm_cointegration(vecm_fit)
```

`econcompare` deliberately does not convert Johansen statistics into an automatic rank choice and does not impose structural VAR identification or Cholesky ordering.

If storage classes or automatic suggestions are unsuitable, researchers can apply explicit validated overrides in the Shiny sidebar or programmatically:

```r
d2 <- eco_apply_types(
  d,
  c(year = "time_year", education_spending = "continuous")
)
```

Manual ordinal typing requires the substantive order explicitly:

```r
d_ord <- eco_apply_types(
  data.frame(satisfaction = c("High", "Low", "Medium")),
  c(satisfaction = "ordinal"),
  ordinal_levels = list(satisfaction = c("Low", "Medium", "High"))
)
```

Temporal coefficient inference can remain classical or use opt-in HAC/Newey-West standard errors:

```r
fit_hac <- eco_time_run(
  d2,
  pisa_score ~ education_spending,
  time = "year",
  models = "time_static",
  inference = "HAC",
  hac_lag = 2
)
```

HAC requires the optional `sandwich` and `lmtest` packages. It changes coefficient inference, not the underlying OLS point estimates.

## Robustness in 0.9.4

Version 0.9.4 is a cross-section hardening release. It does not add new estimators. The Shiny layout now contains wide tables and multi-value controls within their columns, Data Explorer relationship plots handle zero-variance variables explicitly, and categorical plots use a more defensive layout for longer labels.

Result extraction can also be isolated model by model:

```r
cmp <- eco_compare(fit, error_policy = "collect")
attr(cmp, "extraction_warnings")
attr(cmp, "extraction_failures")
```

The default `error_policy = "stop"` remains strict for programmatic use. The Shiny app and HTML viewer use collection internally so an extraction problem in one fitted alternative does not erase successful results from the others. IV setup now checks the minimum order condition in the interactive formula builder, and Heckman controls prevent the selection indicator from being reused as its own selection regressor.

## Comparison philosophy

`econcompare` only allows models from **one outcome group per comparison**. For example, `ols` and `logit` cannot be mixed in one `eco_run()` call.

This is intentional. Different model families can estimate parameters on different scales. The package therefore presents coefficient tables and fit information without claiming that every raw coefficient magnitude is directly comparable.

The goal is model exploration: understand how plausible alternatives behave, identify a specification worth investigating further, then continue with the original modelling package or your own R workflow.

## Diagnostics

Diagnostics remain optional and compatibility-aware. In the Shiny app, no diagnostic runs automatically. The continuous-outcome branch retains the validated cross-section checks from earlier versions. Binary logit/probit/LPM currently expose only simple coefficient-inference summaries in the interactive checklist; multinomial and ordered models intentionally have no automatic diagnostic checklist yet.

```r
eco_diagnostic_tests()
eco_diagnostics(fit, tests = c("breusch_pagan", "reset"))
```

## Sample audit

Sample comparison no longer assumes OLS is present. `eco_sample_audit()` uses the first successfully fitted model as a **technical sample anchor only**, or you can choose one explicitly:

```r
eco_sample_audit(fit_bin, reference = "logit")
```

This does not make that model an econometric benchmark.

## Project status

`econcompare` is research software. Version 0.11.0 combines the stable outcome-aware cross-sectional workflow with explanatory time-series tools covering temporal regression, distributed lags, dynamic regression, ARDL, ECM, VAR and VECM. The package intentionally prioritises a small, understandable and auditable workflow over forecasting automation or automatic model-selection rules.

Version 0.11.0 should be treated as an advanced public beta until the package passes a local `devtools::check()` / `R CMD check` and the GitHub Actions matrix. ECM/VAR/VECM outputs should be interpreted together with their diagnostics; econcompare does not automatically select lag order, deterministic specification or cointegration rank.

Before using results in published empirical work, inspect estimator warnings, verify assumptions and continue the final analysis with the underlying modelling package where appropriate.

## Licence

MIT. See `LICENSE` and `LICENSE.md`.
