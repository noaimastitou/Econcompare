<img width="1850" height="952" alt="image" src="https://github.com/user-attachments/assets/bb8364b0-7d92-4974-b427-be0735c56ee5" />

# econcompare

**Outcome-aware econometric exploration for cross-sectional, explanatory time-series and panel data in R.**

`econcompare` is a research-oriented R package for quickly exploring data, estimating a small set of candidate econometric models, and comparing their results in one interface before continuing the detailed analysis in dedicated R code.

The package is intentionally **comparison-first rather than automation-first**. It does not choose the “best” model for the researcher, and OLS is no longer imposed as a universal benchmark.

## Model groups

Version 0.14.4 fixes help-bubble layering and temporal diagnostic selection resets, removes the panel Interpretation tab, and introduces model-by-model panel diagnostic controls. 

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


## Explanatory time-series econometrics

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

`econcompare` is research software. Version 0.12.0 adds a static linear panel workflow and regression tests. Runtime validation remains pending. Release validation is described in GITHUB_PUBLISHING.md. The package intentionally prioritises a small, understandable and auditable workflow over forecasting automation or automatic model-selection rules.

Before using results in published empirical work, inspect estimator warnings, verify assumptions and continue the final analysis with the underlying modelling package where appropriate.

## Licence

MIT. See `LICENSE` and `LICENSE.md`.


## Static linear panels (0.12.0)

Install the optional `plm` package. Explicitly choose both indexes. A panel suggestion
from `eco_data_structure()` is advisory and does not activate an estimator.

```r
library(econcompare)
data("Grunfeld", package = "plm")
eco_panel_audit(Grunfeld, id = "firm", time = "year")
eco_panel_models()
fit <- eco_panel_run(
  Grunfeld, inv ~ value + capital, id = "firm", time = "year",
  models = c("panel_pooling", "panel_fe_individual", "panel_fe_time",
             "panel_fe_twoways", "panel_re"),
  inference = "cluster_id"
)
eco_compare(fit)
eco_sample_audit(fit)
eco_panel_diagnostics(fit, tests = c("serial", "dependence", "hausman"))
eco_app(Grunfeld)  # Choose Panel data econometrics, indexes and inference.
```

The first release accepts a numeric response and static additive named regressors
(numeric, logical or factors). Precompute transformations; dynamic models are not
supported by putting `lag()` in the formula. Unbalanced panels are allowed. Missing
model observations stop by default; `na_action = "omit"` records a common complete-case
sample. Missing indexes and duplicate id-period pairs always stop.

| Engine | Interpretation |
|---|---|
| `panel_pooling` | Pooled OLS; no absorption of individual heterogeneity |
| `panel_fe_individual` | Within-individual association |
| `panel_fe_time` | Association net of common period effects |
| `panel_fe_twoways` | Association net of individual and period effects |
| `panel_re` | Individual random-effects GLS, Swamy-Arora; requires orthogonality |
| `panel_fd` | Consecutive within-individual changes, without added drift |
| `panel_between` | Equally weighted individual temporal means |
| `panel_mundlak` | RE augmented with means of time-varying regressors |

`inference = "classical"` uses model-based covariance with residual t degrees of
freedom. For the original five estimators and Mundlak, `"cluster_id"` uses Arellano HC1 group covariance and t(G-1), assuming
independence between individuals. The number of groups is reported; this rule does
not solve small-cluster inference. The Shiny interface requires an explicit choice.

Absorbed regressors remain in the comparison with `NA` and `term_status`; they do not
have an estimated zero effect. Other collinearities fail explicitly. Fixed-effect
singleton observations are excluded and listed in `sample_exclusions`, recursively
for two-way effects. Compare exact samples with `eco_sample_audit()`. Original row
references are positions in the input, independent of user-assigned row names.

Classical F and panel-effects LM tests are available only for classical-inference
objects. The serial BG and Pesaran CD tests have explicit applicability gates.
Hausman is optional: fitted-model classical comparison, or robust auxiliary regression
for cluster inference with matching FE/RE slope sets. A failed prerequisite produces
`unavailable`, not an invented statistic. No test automatically chooses FE or RE.

Gaps are permitted for static estimation; serial tests require verified consecutive
periods within each individual. Numeric time means integer period coordinates. Use
`"2020-01"` or `"2020-Q1"` for calendar data. Check any cadence inferred from dates.
Balance and missing-at-random are different questions: this audit does not establish
that attrition is ignorable. Two-way FE is not automatically a valid causal DiD design.
R-squared refers to transformed equations, and AIC/BIC are not supplied for ranking
these estimators.

Run the reproducible tutorial with:

```r
source(system.file("examples", "panel_workflow.R", package = "econcompare"))
```

The [panel extension roadmap](PANEL_ROADMAP.md) keeps additional linear models,
nonlinear outcomes, IV, dynamics and more advanced inference in scope for later work.


## 0.12.2 specification and reproducibility contract

Temporal and ECM response formulas accept one named numeric column. Precompute
`log_y <- log(y)` in the input data rather than passing `log(y)` as the response.
Temporal formulas require an intercept; `y ~ x - 1` is rejected explicitly.
For ECM, use `long_run_intercept = FALSE` to remove the first-step intercept.
Generated lag/difference names must not collide; rename conflicting columns.

Aliased OLS/WLS terms remain visible as NA with an explanatory `term_status`.
Expected FE absorption is stored in `fit$information`, separate from warnings.
`fit$provenance` records requested/fitted formulas, source and loaded engine
versions, specifications and sample information. Raw model objects remain accessible.

In Shiny, changing estimation inputs flags the displayed results as belonging to
the previous run. Rerun before requesting diagnostics; stationarity diagnostics
use the captured data for that run. Changes to diagnostic parameters clear their
previous output. Suggestions for panel indexes still require explicit selection.


## Additional linear panel models in 0.13.0

```r
fit <- eco_panel_run(
  Grunfeld, inv ~ value + capital, id = "firm", time = "year",
  models = c("panel_fe_individual", "panel_fd", "panel_between", "panel_mundlak"),
  inference = "cluster_id"
)
eco_compare(fit)
eco_sample_audit(fit)
fit$transformations              # Original source-row membership
fit$mean_terms$panel_mundlak     # Generated names -> original variables
fit$components$panel_mundlak     # Slopes and covariance-based contrasts
eco_panel_diagnostics(fit, tests = "mundlak")
```

These three new engines currently accept numeric/logical regressors. Explicitly
encode factor contrasts before using them. First differences use only verified
consecutive periods and never bridge a missing observation. The intercept is
absorbed, no drift is introduced, and R-squared is uncentered. For a plm reference,
use `model = "fd"` and a formula with `-1`, because plm otherwise retains a drift.

Between averages the complete-case sample separately for each individual, includes
singletons and weights each mean equally. Mundlak uses that same sample to construct
means of varying regressors; invariant regressors receive no redundant mean. The
original slope is a within-type conditional association, the mean coefficient is a
between-minus-within contrast, and their sum is a between-type association. The
sum need not equal a separate between regression in an unbalanced panel. Neither
model corrects informative missingness or time-varying endogeneity automatically.

`n_used` counts contributing source rows; `n_effective` and `nobs` count observations
in the transformed equation. Shiny presents these units and source membership in
Sample audit. The panel Interpretation tab has been removed in 0.14.4. The
independent HTML export still includes interpretation and Mundlak components.

FD cluster covariance uses `sandwich::vcovCL(type = "HC1", cadjust = TRUE)` with
individual clusters. Between uses `sandwich::vcovHC(type = "HC1")` across individual
means. Both use t(G-1); the precise convention is recorded per model. Classical FD
inference requires assumptions on differenced errors; independent level errors
generally become correlated after differencing.

The optional Mundlak diagnostic is an approximate Wald F test of the added means,
using the selected covariance. It is not an automatic RE selection rule. BG/CD
requests for FD or between return explicit unavailable records; applying the
existing level-panel diagnostics mechanically would misrepresent their scope.

Method references: [plm estimator documentation](https://rdrr.io/cran/plm/man/plm.html),
[sandwich clustered covariance documentation](https://rdrr.io/cran/sandwich/man/vcovCL.html),
[Stata's Mundlak explanation](https://blog.stata.com/2015/10/29/fixed-effects-or-random-effects-the-mundlak-approach/).


## Panel extension 0.14.0: explicit outcome and identification

The original eight linear engines are preserved. Three engines are added:

| Engine | Outcome | Estimator | Inference |
|---|---|---|---|
| `panel_clogit` | Binary numeric 0/1 | Exact conditional logit, individual strata | Classical conditional-likelihood covariance, normal z; no cluster option |
| `panel_poisson` | Non-negative integer counts | Poisson individual FE | Classical normal z or individual-cluster t(G-1) |
| `panel_fe_iv` | Continuous | Individual within-2SLS | Classical residual t or individual-cluster t(G-1) |

```r
binary_fit <- eco_panel_run(d, binary ~ x + w, "id", "period",
  outcome_type = "binary", inference = "classical")
count_fit <- eco_panel_run(d, count ~ x + w, "id", "period",
  outcome_type = "count", inference = "cluster_id")
iv_fit <- eco_panel_run(d, y ~ endogenous_x + w, "id", "period",
  models = c("panel_fe_individual", "panel_fe_iv"),
  endogenous = "endogenous_x", instruments = c("z1", "z2"),
  inference = "cluster_id")
iv_fit$first_stages
eco_panel_diagnostics(iv_fit, tests = "iv_first_stage")
```

These snippets require your own columns; run the self-contained example
`inst/examples/panel_specialized_workflow.R` for simulated demonstration data.
No instruments are selected automatically. Instrument missingness enters the
common sample, including the non-IV comparisons in the same run. First-stage
Wald tests address relevance, not validity, and are not general weak-IV diagnostics.

All three adapters initially use individual effects and numeric/logical regressors
(FE-IV: numeric only). Exact logit excludes individuals without binary changes;
Poisson excludes all-zero individuals and singletons. Original row positions and
absorbed terms remain visible. The Poisson adapter currently excludes rates,
non-integer PPML outcomes and exposure offsets. R-squared is withheld for these
three adapters; no likelihood-based cross-estimator ranking is supplied.

Shiny requires an explicit panel outcome family. Endogenous-regressor and excluded-
instrument controls appear only for FE-IV. Model fit reports coefficient scales and
inference. In Diagnostics, select one fitted model, then its available tests;
first-stage relevance is available for FE-IV. There is no panel Interpretation tab.
Raw API term names are retained while `term_label` supplies readable Mundlak names.

Diagnostics retain `status` and add `reason_code` distinguishing missing models,
inapplicable tests, unimplemented diagnostics and calculation failures. After
period effects, standard Pesaran CD is marked `computed_uninterpreted`: the engine
statistic and `raw_p.value` remain auditable, but the inferential `p.value` is NA
and automatic conclusions are suspended. This is a conservative safeguard, not a
new corrected CD test. Mundlak displays covariance choice and inference degrees
of freedom explicitly. BG/CD are not implemented for the nonlinear/IV adapters.

References:
- [survival conditional logit](https://stat.ethz.ch/R-manual/R-devel/library/survival/html/clogit.html)
- [fixest GLM and Poisson](https://lrberge.github.io/fixest/reference/feglm.html)
- [fixest finite-sample covariance adjustments](https://lrberge.github.io/fixest/reference/ssc.html)
- [plm within instrumental variables](https://rdrr.io/cran/plm/man/plm.html)
- [Juodis and Reese: CD with estimated time effects](https://doi.org/10.1080/07350015.2021.1906687)
