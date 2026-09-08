.ec_registry <- function() {
  data.frame(
    engine = c(
      "ols", "wls", "ols_robust", "robust_m", "fixest", "ivreg", "quantile",
      "tobit", "heckman"
    ),
    family = c(
      "OLS reference", "Linear", "Linear", "Robust linear", "Linear", "Endogeneity", "Distributional robustness",
      "Censored outcome", "Sample selection"
    ),
    estimator = c(
      "Ordinary least squares", "Weighted least squares", "OLS with robust standard errors", "Robust M-estimation",
      "OLS via feols", "Instrumental variables / 2SLS", "Quantile regression",
      "Censored regression (Tobit)", "Heckman sample-selection model"
    ),
    package = c(
      "stats", "stats", "estimatr", "MASS", "fixest", "ivreg", "quantreg",
      "censReg", "sampleSelection"
    ),
    comparison_to_ols = c(
      "Reference model",
      "Same linear coefficients under weighting",
      "Same OLS coefficients; inference changes",
      "Same outcome scale; robust location estimator",
      "Same linear conditional-mean scale",
      "Same outcome scale; addresses endogeneity",
      "Same outcome scale; conditional quantiles rather than conditional mean",
      "Latent-outcome coefficients; compare cautiously with OLS",
      "Outcome-equation coefficients; compare cautiously with OLS"
    ),
    stringsAsFactors = FALSE
  )
}

#' List OLS-comparable cross-sectional model engines supported by econcompare
#'
#' @return A data.frame describing engines, requirements and how each model relates to the OLS reference.
#' @export
eco_models <- function() {
  z <- .ec_registry()
  z$available <- vapply(z$package, function(pkg) {
    pkg %in% c("stats", "MASS") || requireNamespace(pkg, quietly = TRUE)
  }, logical(1))
  z
}
