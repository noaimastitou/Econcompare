\name{eco_panel_run}
\alias{eco_panel_run}
\title{Estimate static panel models}
\usage{
eco_panel_run(data, formula, id, time,
  models = c("panel_pooling", "panel_fe_individual"),
  inference = c("classical", "cluster_id"),
  na_action = c("fail", "omit"), error_policy = c("stop", "collect"),
  outcome_type = c("continuous", "binary", "count"),
  endogenous = NULL, instruments = NULL)
}
\arguments{
\item{data}{A data.frame. Unbalanced panels are permitted.}
\item{formula}{Static additive formula such as y ~ x1 + x2, with an intercept and named columns only. Precompute transformations.}
\item{id}{Individual identifier column name.}
\item{time}{Period column name: integer periods, Date, or unambiguous calendar labels.}
\item{models}{Engine names matching outcome_type; when omitted, defaults to conditional logit for binary and Poisson for count outcomes. Engine names returned by \code{eco_panel_models()}.}
\item{inference}{Classical covariance with residual t degrees of freedom for linear models and normal z for nonlinear models, or individual-robust HC1 with t(G-1). See Details for estimator-specific covariance conventions.}
\item{na_action}{Stop on missing/non-finite model values, or explicitly omit them on a common sample. Invalid index pairs are always rejected.}
\item{outcome_type}{Explicit continuous, binary (numeric 0/1) or count (non-negative integer) outcome.}
\item{endogenous}{Numeric regressor names selected as endogenous for panel_fe_iv.}
\item{instruments}{Numeric excluded instrument names, distinct from model variables and indexes.}
\item{error_policy}{Stop on a model failure, or collect failures while retaining successful models. All-model failure stops.}
}
\value{An object of classes econcompare_panel and econcompare. It contains raw plm/lm/clogit/fixest models, IV specification, first-stage models and relevance tests, transformations mapping observations to original rows, Mundlak mean_terms and components, metadata, covariance matrices, term_status, panel_audit, estimation_audit, model_audits, original positional sample_rows, sample_info, excluded_rows for common missingness, per-model sample_exclusions, methodological information, warnings, failures and provenance.}
\description{Fits pooled OLS, individual, time or two-way fixed effects, individual random effects, first differences, between, Mundlak, conditional logit, Poisson individual effects and FE-IV/2SLS. Uses optional plm and sandwich packages with stats::lm for explicit differences and individual means. No automatic estimator selection, imputation or causal interpretation is performed.}
\details{Time-invariant regressors absorbed by fixed effects remain visible as NA in eco_compare, with explicit term status. Other unidentified regressors cause a model failure. Singleton fixed-effect groups are excluded explicitly, recursively for two-way effects; their original row positions are recorded. This can produce different samples across estimators. Gaps are allowed for static estimation but serial tests require verified consecutive periods. Numeric time labels mean integer observation periods; for calendar months or quarters use explicit labels. Calendar frequency inferred from Date values is advisory. Cluster inference assumes independent individuals and can be unreliable with few groups. Random effects use the Swamy-Arora variance-components method. Likelihood criteria are not provided for automatic cross-estimator ranking; R-squared refers to each estimator's transformed equation. Dynamic models, weights, interactions and formula transformations are outside this release's scope.}
\seealso{\code{\link{eco_panel_audit}}, \code{\link{eco_panel_models}}, \code{\link{eco_panel_diagnostics}}, \code{\link{eco_compare}}, \code{\link{eco_sample_audit}}}
\examples{
if (requireNamespace("plm", quietly = TRUE)) {
  data("Grunfeld", package = "plm", envir = environment())
  fit <- eco_panel_run(Grunfeld, inv ~ value + capital,
    id = "firm", time = "year", inference = "cluster_id")
  eco_compare(fit)
  eco_sample_audit(fit)
}
}

\section{Additional linear estimators}{
\code{panel_fd}, \code{panel_between} and \code{panel_mundlak} require numeric or logical regressors in this release. Factors must be explicitly encoded before estimation.

First differences use consecutive calendar periods within the same individual only. An omitted period is never bridged. The input calendar is retained through complete-case filtering. The intercept and invariant regressors disappear; no drift is added. This differs from the default intercept convention of plm's FD estimator: the reference is \code{model = "fd"} with \code{-1}. R-squared is uncentered. Classical covariance concerns differenced errors, which can be serially correlated even when level errors are independent.

Between uses one equally weighted temporal mean per individual, including singletons, computed on the common complete-case sample. Different observation windows can change interpretation in an unbalanced panel.

Mundlak augments individual random effects with individual means of time-varying regressors computed on that same sample. Invariant regressors receive no redundant mean. \code{mean_terms} maps generated names to source variables. \code{components} separates the original within-type conditional slope, the mean coefficient (between-minus-within contrast), and their sum (between-type slope), with covariance-based standard errors. These labels do not assert causal identification or resolve informative missingness; in unbalanced panels the sum need not equal a separate between regression.

For cluster inference, FD uses \code{sandwich::vcovCL(type = "HC1", cadjust = TRUE)} with individual clusters; between uses \code{sandwich::vcovHC(type = "HC1")} across individual means; Mundlak and the original panel estimators use \code{plm::vcovHC(method = "arellano", type = "HC1", cluster = "group")}. All use t(G-1). The exact covariance convention is stored in each model's metadata.

\code{sample_info$n_used} counts contributing source rows; \code{n_effective} and extracted \code{nobs} count transformed observations. \code{transformations} records FD start/end row pairs or mean membership. \code{sample_rows} lists source positions; equality of source rows alone does not imply comparable observational units or estimands.
}

\section{Binary, count and instrumental-variable panels}{
\code{panel_clogit} requires \code{outcome_type = "binary"} and numeric 0/1 coding, with 1 denoting the event. It uses \code{survival::clogit(method = "exact")} with individual strata. Individuals with all-zero or all-one outcomes are explicitly excluded. Invariant regressors are absorbed. Coefficients are conditional log odds; their exponentials are conditional odds ratios. The individual intercepts and unconditional probabilities are not estimated. Only classical conditional-likelihood covariance and asymptotic normal inference are supported; cluster inference is rejected explicitly because the exact survival engine does not support that covariance. Large strata may make the exact computation expensive or infeasible; no approximation is silently substituted.

\code{panel_poisson} requires \code{outcome_type = "count"}. This release accepts non-negative integer counts and individual effects using \code{fixest::fepois}. Individuals with all-zero counts and singletons are excluded explicitly. Coefficients are on the log conditional-mean scale; exponentials are multiplicative mean ratios. Rates, exposures/offsets and non-integer PPML outcomes are outside this initial scope. Classical covariance uses normal z inference and assumes the Poisson variance specification. Cluster inference uses the individual sandwich with fixest corrections K.adj=TRUE, K.fixef=nonnested and G.adj=TRUE, followed by t(G-1). Engine convergence, retained rows, finite coefficients and positive variances are checked; this is not a general mathematical certificate against every separation configuration.

\code{panel_fe_iv} uses within 2SLS in plm with individual effects. Supply \code{endogenous} and \code{instruments} explicitly; included exogenous regressors are automatically included among the instruments. Regressors and instruments must be numeric. Instrument missingness is included in the common-sample filter for all requested models, so comparisons use an explicit shared complete-case base. Singleton groups and invariant exogenous controls are removed/absorbed; invariant endogenous variables and rank-deficient instrument designs are rejected. The first-stage models are retained in \code{first_stages}, and excluded-instrument Wald F tests in \code{first_stage_tests}. These are relevance diagnostics, not general weak-instrument-robust tests, not a test of exclusion restrictions, and not a basis for automatic instrument selection. No Sargan/Hansen or weak-IV-robust confidence intervals are implemented in this release.

For all three engines, coefficient scales and reference distributions are explicit. R-squared and likelihood ranking statistics are intentionally not supplied. The original eight linear engines remain available for continuous outcomes; nonlinear families cannot be mixed into the same comparison run.
}
