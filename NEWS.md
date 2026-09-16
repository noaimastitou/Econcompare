# econcompare 0.11.0

- Adds a transparent two-step single-equation ECM through `eco_ecm_run()`, with the long-run levels relationship retained separately through `eco_ecm_long_run()`. The first-step levels regression is explicitly not treated as proof of cointegration.
- Adds multivariate dynamic systems through `eco_system_run()`: reduced-form VAR and Johansen-based VECM are represented as systems rather than forcing a single dependent-variable formula.
- Adds `eco_system_models()` as the system-model catalogue, mirroring `eco_models()` and `eco_time_models()` without mixing system and single-equation engines.
- Adds `eco_var_lag_selection()` to report AIC/HQ/SC/FPE lag-order evidence without automatically choosing the lag order.
- Adds `eco_johansen_test()` and retained `eco_vecm_rank_test()` output. VECM requires an explicit researcher-supplied cointegration rank.
- Adds `eco_vecm_cointegration()` for normalized cointegrating vectors with explicit normalization caveats.
- Adds `eco_system_diagnostics()` for companion-root information and multivariate residual serial-correlation checks. The Portmanteau horizon must exceed the fitted VAR lag order; the stationary-VAR root rule is not mechanically applied to cointegrated VECM systems.
- Extends the Shiny time-series workflow with three model families: single-equation temporal regressions, ECM, and multivariate VAR/VECM systems.
- Preserves the no-forecasting scope and researcher-control principle: no automatic differencing, lag selection, cointegration-rank selection, structural identification, or Cholesky ordering.
- Adds tests for ECM lag construction, calendar-gap protection, VAR equation extraction, lag-selection evidence, Johansen rank evidence, explicit VECM rank, and advanced Shiny controls.

# econcompare 0.10.2

- Makes manual ordinal type overrides fully explicit: `eco_apply_types()` now requires `ordinal_levels` for variables manually typed as ordinal, and the Shiny override control asks the researcher to select every category from lowest to highest.
- Replaces the implicit ADF deterministic specification with an explicit researcher choice (`none`, `drift`, or `trend`) using `urca::ur.df`; critical values are reported directly and no approximate ADF p-value is invented.
- Refuses HAC/Newey-West inference and Breusch-Godfrey lag diagnostics on irregular or incomplete calendars, where an observation lag would not represent a constant calendar lag.
- Lets researchers choose exactly which model variables receive ADF/KPSS diagnostics in the Shiny app.
- Clarifies model-comparison wording to distinguish an identical estimation sample from merely equal observation counts.
- Adds the first variable-specific lag API through `q_by_var`, while retaining `q` as the default lag order for unspecified regressors.
- Adds structured `time_metadata` to time-series `econcompare` objects, including calendar properties, ordered index information, model-specific lag specifications, and estimation-period ranges.
- Extends regression tests for explicit ordinal order, ADF deterministic components, irregular-grid HAC/BG guards, individual stationarity selection, variable-specific lags, temporal metadata, and sample-comparison wording.

# econcompare 0.10.1

- Adds a conservative observational-structure detector with explicit researcher confirmation. Native Date/POSIX indices and unambiguous year, year-month, year-quarter and ISO-date forms are recognized; ambiguous locale-dependent dates are not guessed.
- Adds `eco_time_audit()` with checks for missing/duplicated time points, ordering, regularity, frequency and gaps. Repeated dates are flagged as potential panel/longitudinal data rather than silently treated as a single series.
- Adds explanatory time-series econometrics: static temporal regression, distributed lags, dynamic regression and ARDL-style finite lag regressions via `eco_time_run()`. Forecasting-only models are intentionally out of scope.
- Adds `eco_time_diagnostics()` for explicit Breusch-Godfrey residual serial-correlation testing and `eco_stationarity_tests()` for optional ADF/KPSS diagnostics. No automatic differencing or lag selection is performed.
- Extends the Shiny app with cross-section/time-series mode detection, explicit time-index selection, temporal audit, lag controls, standardized temporal trajectories and time-series result metadata.
- Preserves the 0.9.4 cross-sectional model API and hardening work as the stable cross-section branch while adding temporal mode alongside it.

# econcompare 0.9.4

- Hardened the Shiny responsive layout so table wrappers, ordinal controls and Data Explorer cards no longer force neighbouring columns to move or overflow. Wide result tables now scroll inside their own containers, while compact sidebar tables remain compact.
- Made multi-value Selectize controls width-safe and kept the ordinal category-order selector explicitly researcher-controlled.
- Hardened Data Explorer plots: zero-variance numeric variables are handled explicitly, Pearson correlation is not attempted when it is undefined, and categorical distributions switch to a horizontal layout for longer or more numerous labels.
- Added model-by-model result extraction with `eco_compare(..., error_policy = "collect")`. Extraction warnings and failures are attached to the returned table instead of discarding successful results from other models.
- The Shiny app and HTML viewer now use isolated extraction internally, including quantile-summary warnings that can arise after estimation.
- Separated ordered-model slope coefficients from category thresholds in the Shiny and HTML viewers. Thresholds remain available as technical parameters but are no longer presented as regressor effects.
- Added IV input checks for structural/endogenous consistency, excluded-instrument overlap, and the minimum order condition.
- Hardened Heckman controls so the outcome and selection indicator cannot be reused as selection regressors; the selection-regressor list updates with the chosen indicator.
- Added validation for empty or duplicated data-frame column names and clarified that a two-valued numeric variable is not a binary model outcome unless it is coded 0/1.
- Corrected the `robust_m` catalogue description to identify it as robust M-estimation regression.
- Expanded regression tests for responsive CSS, ordinal workflow wiring, IV/Heckman guards, zero-variance Data Explorer relationships, and model-isolated extraction failures.

# econcompare 0.9.3

- Fixed the ordinal-outcome Shiny UI: the category-order control is now actually mounted in the sidebar when `Ordinal categorical outcome` is selected.
- Replaced the ordinal free-text order field with an explicit multi-category selector. Researchers select every observed category from lowest to highest and can drag selected categories to reorder them.
- Existing R `ordered` factor levels are pre-filled because that order is already researcher-supplied; unordered outcomes remain blank so econcompare does not infer a substantive ranking.
- Added the previously server-defined binary-event UI to the visible sidebar as well, preventing the same hidden-control failure for labelled binary outcomes.
- The run path now consumes the ordinal selector as an ordered vector while retaining strict validation that every observed category appears exactly once.

# econcompare 0.9.2

- Fixed a Shiny workflow bug where the server required a hidden/non-rendered analysis-objective confirmation input.
- Removed the redundant confirmation checkbox requirement: the value explicitly selected in `Analysis objective` is now the researcher's modelling choice.
- Kept outcome suggestions descriptive only; econcompare still does not make the substantive modelling decision on behalf of the researcher.
- Updated the app copy and test coverage so a valid selected objective can run without any hidden confirmation state.

# econcompare 0.9.1

- Made nominal modelling conservative: numeric outcomes are no longer converted to categories automatically; users must create a factor explicitly.
- Added common binary comparison summaries through `eco_binary_compare()` (event rate, mean fitted value, 0.5-threshold accuracy, RMSE and Brier score when fitted values remain in [0,1]).
- Added explicit binary-event selection for character outcomes and consistent coding metadata across LPM, logit and probit.
- Added an ordinal category-order control in the Shiny app; the app no longer requires users to leave the interface to create an ordered factor, while the programmatic API remains explicit.
- Ordered-model data now drop unused levels while preserving the user-defined order.
- Kept LPM, logit and probit unweighted in the simple binary comparison set.
- Expanded the Data explorer with category counts and shares for binary/categorical variables.
- Made Model fit outcome-aware so unlike fit scales are not presented as a single ranking metric.
- Added explicit confirmation of the analysis objective before estimation.
- Split the application UI/server and the diagnostics code into smaller source modules to improve maintainability.
- Removed duplicated app logic found during the 0.9.0 audit.

# econcompare 0.9.0

- Removed the mandatory OLS-reference architecture. OLS is now an optional model like the other registered engines.
- Added explicit model groups by dependent-variable objective: continuous, binary, nominal categorical and ordinal categorical.
- Added binary LPM, logit and probit engines.
- Added multinomial logit through `nnet::multinom()`.
- Added ordered logit and ordered probit through `MASS::polr()`.
- Added outcome validation so models from different outcome groups cannot be mixed in one comparison.
- Added explicit 0/1 coding metadata for binary factor responses so LPM, logit and probit analyse the same event.
- Added outcome-type guidance to the Shiny app and Data explorer.
- Generalised the sample audit so no model is treated as a universal econometric reference.
- Preserved the simple-first workflow: categorical models are added without pretending that raw coefficient scales are interchangeable.

# econcompare 0.8.0

- Added a simple pre-model Data explorer and simplified the interactive diagnostic workflow.
- Preserved compatibility-aware, user-controlled cross-section diagnostics.
