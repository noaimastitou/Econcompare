.ec_registry <- function() {
  data.frame(
    engine = c(
      "ols", "wls", "ols_robust", "robust_m", "fixest", "ivreg", "quantile", "tobit", "heckman",
      "lpm", "logit", "probit", "multinomial_logit", "ordered_logit", "ordered_probit"
    ),
    outcome_type = c(
      rep("continuous", 9),
      rep("binary", 3),
      "nominal", "ordinal", "ordinal"
    ),
    family = c(
      "Linear", "Linear", "Linear inference", "Robust linear", "Linear", "Endogeneity", "Distributional", "Censored outcome", "Sample selection",
      "Binary", "Binary", "Binary", "Nominal categorical", "Ordinal categorical", "Ordinal categorical"
    ),
    estimator = c(
      "Ordinary least squares", "Weighted least squares", "OLS with robust standard errors", "Robust M-estimation",
      "OLS via feols", "Instrumental variables / 2SLS", "Quantile regression", "Censored regression (Tobit)", "Heckman sample-selection model",
      "Linear probability model (LPM)", "Binary logit", "Binary probit", "Multinomial logit", "Ordered logit", "Ordered probit"
    ),
    package = c(
      "stats", "stats", "estimatr", "MASS", "fixest", "ivreg", "quantreg", "censReg", "sampleSelection",
      "stats", "stats", "stats", "nnet", "MASS", "MASS"
    ),
    comparison_note = c(
      "Linear conditional-mean model.",
      "Linear model estimated with observation weights.",
      "OLS coefficients with heteroskedasticity-robust inference.",
      "Robust M-estimation regression on the outcome scale; coefficient interpretation follows a linear regression structure but estimation downweights atypical residual patterns.",
      "Linear conditional-mean model estimated with fixest.",
      "Linear structural model estimated by 2SLS.",
      "Conditional-quantile model; coefficients answer a different question from mean regression.",
      "Latent-outcome censored regression; coefficients require model-specific interpretation.",
      "Sample-selection model; outcome-equation coefficients require model-specific interpretation.",
      "Linear model for a 0/1 outcome; fitted values are not constrained to [0,1].",
      "Log-odds scale; compare signs, fit and predictions rather than raw coefficient magnitudes across links.",
      "Latent normal-index scale; raw coefficients are not on the same scale as logit or LPM coefficients.",
      "Category-specific log-odds relative to a reference category; the response must be explicitly categorical.",
      "Proportional-odds cumulative logit model using an explicitly ordered outcome.",
      "Ordered probit model based on a latent normal index and an explicitly ordered outcome."
    ),
    stringsAsFactors = FALSE
  )
}

#' List cross-sectional model engines supported by econcompare
#'
#' Models are grouped by the type of dependent variable they are designed for:
#' continuous, binary, nominal categorical, or ordinal categorical. OLS is an
#' available model rather than a mandatory reference.
#'
#' @param outcome_type Optional outcome group to filter: `"continuous"`,
#'   `"binary"`, `"nominal"`, or `"ordinal"`.
#' @return A data.frame describing engines, outcome groups, requirements and comparison notes.
#' @export
eco_models <- function(outcome_type = NULL) {
  z <- .ec_registry()
  z$available <- vapply(z$package, function(pkg) {
    pkg == "stats" || requireNamespace(pkg, quietly = TRUE)
  }, logical(1))
  if (!is.null(outcome_type)) {
    outcome_type <- match.arg(outcome_type, c("continuous", "binary", "nominal", "ordinal"))
    z <- z[z$outcome_type == outcome_type, , drop = FALSE]
  }
  rownames(z) <- NULL
  z
}
