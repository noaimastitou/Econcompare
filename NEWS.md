# econcompare 0.14.4

- Help bubbles render in a fixed body-level layer, with viewport positioning, keyboard focus and Escape dismissal.
- Separate temporal diagnostic controls from result rendering, preventing ADF/KPSS result updates from resetting series selections in single-equation and VAR/VECM screens.
- Remove the panel Interpretation tab and its content; no relocation to other tabs.
- Panel diagnostics now select a fitted model first, then its available tests. Only that model is tested; required companion models remain available for explicit FE/pooled and FE/RE comparisons.
- Changing the panel diagnostic model clears previous results. Public batch diagnostic APIs and econometric estimators remain unchanged.
- Add regression tests and update prior UI expectations. Runtime R/Shiny and visual browser checks remain required.

# econcompare 0.14.3

- Panel HTML diagnostics="all" selects the compatible diagnostic families, including Hausman, Mundlak and IV first stages when available; limitations remain visible.
- HTML reports retain estimation warnings/failures alongside extraction issues, with their stage.
- No invitation to run unavailable panel diagnostics in Shiny.
- Shared specialized-panel fixture moved to a test helper for filtered test runs.
- HTML diagnostic p-values use the same formatter as Shiny.
- Panel objects now record their creation timestamp.
- Added targeted regression tests. Runtime validation remains required before release.

# econcompare 0.14.2

- Contextual panel diagnostic choices, with explicit limitations for unsupported estimators.
- Unavailable diagnostics report no inference performed and no inference degrees of freedom.
- Suspended CD raw p-values are isolated in audit disclosures in Shiny and HTML exports.
- Hide empty model-specific interpretation sections and wholly unavailable fit metrics in displays only.
- Preserve valid inference, missing-value, endogenous-variable and instrument selections on panel control rebuilds.
- Restrict exact conditional logit to its supported inference choices without silently switching inference.
- Add model-specific inference tables, run provenance disclosure and clearer transformed-sample messages.
- Add regression tests for contextual presentation, separation and Poisson nonconvergence.
- Runtime tests and interactive Shiny validation remain a release gate; see VALIDATION_0.14.2.md.

# econcompare 0.14.1 (source hotfix candidate)

- Bind coxph, Surv and strata locally when invoking survival::clogit. This fixes
  the reported unqualified coxph lookup path without attaching survival globally.
- Add a namespace-only regression test and make the direct reference test explicit
  about its own survival bindings.
- R execution remains unavailable in the preparation environment; validate in a
  fresh R session without library(survival). See VALIDATION_0.14.1.md.

# econcompare 0.14.0 (source candidate for audit)

- Add exact conditional panel logit (binary), Poisson individual effects (counts)
  and individual FE-IV/2SLS with explicit endogenous regressors/instruments.
- Preserve first stages, coefficient scales, observation exclusions and exact row
  maps; expose the new families and controls in Shiny.
- Distinguish diagnostic availability reasons using structured reason codes.
- Suspend standard Pesaran CD conclusions after time/two-way effects; retain raw
  engine results for audit, without presenting their p-values as calibrated.
- Report Mundlak inference and degrees of freedom; show readable mean-term labels.
- Correct sample warnings when source rows are identical but units differ.
- Add reference, failure and Shiny workflow tests; extend GitHub CI dependencies.
- This source candidate has not passed R runtime tests in the preparation
  environment (R is unavailable). See VALIDATION_0.14.0.md before release.

# econcompare 0.13.0 (source candidate)

- Add numeric/logical first differences, equally weighted between estimation and
  correlated random effects (Mundlak) to the panel API and Shiny catalogue.
- Record consecutive source-row pairs and individual-mean membership; preserve
  the original calendar through missing-value exclusions. Never bridge gaps.
- Distinguish contributing source rows, transformed sample sizes and observation
  units; retain absorbed terms and reject unidentified designs explicitly.
- Add selected-covariance Mundlak contrasts and an optional joint mean Wald F test.
- Explain estimands and exact covariance conventions in Shiny and HTML; mark
  BG/CD unavailable for the new FD/between representations.
- Add reference and edge-case tests. Runtime validation remains required; the
  development environment has no R executable. See VALIDATION_0.13.0.md.

# econcompare 0.12.2

- Rejects transformed temporal responses and no-intercept input formulas explicitly instead of silently rebuilding a different model. ECM retains its explicit long_run_intercept option. Precomputed transformed columns remain supported.
- Detects generated lag/difference name collisions before constructing temporal or ECM data.
- Retains aliased OLS/WLS coefficient rows as NA with term_status and a captured warning; aligns term-status columns across extractors.
- Quotes literal column names when constructing UI and temporal formulas.
- Captures Shiny run data, configuration and timestamp; flags outdated results, blocks new diagnostics until re-estimation, and uses run snapshots for stationarity tests. Clears diagnostic outputs when their parameters change.
- Separates expected panel absorption information from estimation warnings; keeps terms visible.
- Uses scientific notation for tiny nonzero displayed numbers, integer panel counts and an explicit threshold for zero p-values in coefficient/panel tables.
- Adds readable panel date bounds, index_valid distinct from balanced schedules, and suggestions requiring explicit index confirmation.
- Records source version, R/engine versions, requested/fitted formulas, specifications and sample audit in provenance.
- Adds targeted regression tests and a manual CI trigger. Runtime verification remains required before release; see VALIDATION_0.12.2.md.

# econcompare 0.12.1

- Harmonizes panel results with existing Shiny components: side-by-side coefficients, model-fit tab, audit cards, styled diagnostic tables and explicit empty states.
- Replaces the overflowing sidebar audit table with a wrapping vertical summary.
- Wraps all panel detail tables in bounded, keyboard-focusable horizontal scroll regions; warning text wraps and tab navigation adapts to narrow screens.
- Uses readable estimator labels in the interface while preserving API engine names and raw model objects.
- Clarifies duplicate individual-period keys for quarterly data indexed by year.
- Presentation update only: estimation and diagnostic engines are unchanged from 0.12.0. R/Shiny runtime validation remains pending in the preparation environment.

# econcompare 0.12.0

- Adds explicit panel audit/model/run/diagnostic APIs using optional plm.
- Adds pooled OLS, individual/time/two-way fixed effects and individual random effects.
- Provides classical and individual-cluster HC1 inference with explicit degrees of freedom.
- Preserves raw models, exact input row positions, common missingness exclusions, model-specific singleton exclusions and absorbed-term status.
- Rejects invalid index pairs, unsupported formulas, unidentified regressors and degenerate inference.
- Adds F, panel LM, serial BG, Pesaran CD and optional classical/robust auxiliary Hausman diagnostics with applicability explanations.
- Adds advisory panel candidates, an explicit Shiny panel workflow and panel HTML output.
- Adds plm-reference regression tests, error cases, Shiny coverage, help pages and a runnable tutorial. CI requires plm and shiny.
- Retains the corrected 0.11.1 code. No global UI redesign, dynamic panel or nonlinear panel estimators are added.
- Validation candidate only: runtime tests and R CMD check are pending; see VALIDATION_0.12.0.md.

# econcompare 0.11.1

- Fixes cross-sectional extraction and Shiny rendering when analysis_type is absent: new eco_run objects identify cross_section explicitly, and temporal branches safely handle older objects.

- Normalizes temporal working data to data.frame before assigning row identities, avoiding deprecated tibble row-name assignment while preserving temporal ordering.
- Rejects constant or numerically collinear system series, aliased equation coefficients and singular residual covariance rather than presenting incomplete VAR/VECM coefficient tables.
- Retains system-engine warnings in the returned object while still signalling them to the caller.
- Makes companion-root failures explicit for older singular objects; guards non-finite Portmanteau results and excessive diagnostic horizons, and reports test degrees of freedom.
- Cleans trailing separators from Johansen hypothesis labels and clarifies deterministic terms and VECM parameterizations in the interface.
- Hardens integer validation against overflow and invalid rank/horizon types.
- Adds regression coverage for VAR agreement with vars, all four VAR deterministic specifications, VECM K=4 and ranks 1/2, row identities, tibble inputs, invalid systems, ECM ECT alignment and HAC inference.
- Updates maintainer metadata and current-version documentation. No panel models or new estimator family are added.
- Validation status: the 0.11.1 R tests and R CMD check must run before publication; successful 0.11.0 examples do not certify the modified release.

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
