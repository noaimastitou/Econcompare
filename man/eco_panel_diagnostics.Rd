\name{eco_panel_diagnostics}
\alias{eco_panel_diagnostics}
\title{Explicit diagnostic tests for static linear panel models}
\usage{
eco_panel_diagnostics(x,
  tests = c("effects_f", "effects_lm", "serial", "dependence"),
  serial_order = 1L, alpha = .05)
}
\arguments{
\item{x}{An object returned by eco_panel_run().}
\item{tests}{One or more of effects_f, effects_lm, serial, dependence, hausman, mundlak, iv_first_stage.}
\item{serial_order}{Positive integer lag order for the panel Breusch-Godfrey test.}
\item{alpha}{Significance level strictly between zero and one.}
}
\value{A data.frame with model, test, null_hypothesis, status, statistic, p.value, parameters, interpretation and note. Inapplicable or failed tests are unavailable, with an explanation.}
\description{Runs selected plm diagnostics without automatic estimator selection or silent changes to the fitted sample.}
\details{effects_f compares within and pooled models on identical samples via pFtest. effects_lm uses the individual Breusch-Pagan panel-effects test; it is not a heteroskedasticity test. Both classical tests are unavailable for a cluster-inference fit. serial uses pbgtest, requiring consecutive verified periods and sufficient observations per individual. dependence uses Pesaran's residual CD test with a conservative minimum of three observations per individual. Its interpretation depends on panel dimensions, common periods and fitted effects. hausman requires individual FE and RE on identical samples; classical inference uses phtest on the fitted models. Cluster inference uses its auxiliary regression method with Arellano HC1 group covariance, restricted to matching slope sets. A non-rejection does not establish exogeneity, independence or the truth of the null.}
\seealso{\code{\link{eco_panel_run}}}

\section{Mundlak and transformed models}{Request \code{tests = "mundlak"} for an approximate joint Wald F test of the added individual-mean coefficients using the selected covariance and its reported denominator degrees of freedom. Singular or non-positive-definite restriction covariance yields an unavailable result. Non-rejection does not validate all random-effects exogeneity assumptions. BG and Pesaran CD are not implemented here for first differences or between means; an explicit unavailable record explains the restriction.}

\section{Availability, calibration and inference}{
The status column distinguishes computed, computed_uninterpreted and unavailable. The reason_code column distinguishes available, not_implemented, not_applicable, missing_models, calibration_unverified and calculation_failed. These are structured conditions rather than conclusions inferred from error-message wording.

For individual and time effects or time effects alone, standard Pesaran CD can be biased by the estimated period effects (Juodis and Reese, 2022, doi:10.1080/07350015.2021.1906687). The statistic and raw_p.value returned by the engine are retained for auditing, but p.value is NA, status is computed_uninterpreted and rejection/non-rejection is suspended. No corrected test is substituted. raw_p.value is not a validated inferential result in this case.

Mundlak rows explicitly report the selected covariance in inference, the denominator inference_df and both test degrees of freedom in parameters. iv_first_stage requests the saved excluded-instrument joint first-stage Wald F tests; their limitations are retained in the note. BG and CD are not implemented for the new nonlinear or IV adapters, and are not applicable to the between representation's single mean per individual. Missing FE/RE, Mundlak or IV models are reported as missing_models.
}
