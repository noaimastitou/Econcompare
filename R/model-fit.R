.ec_fit_basis <- function(engine) {
  switch(engine,
    lpm = "Gaussian linear fit",
    logit = "Binomial likelihood",
    probit = "Binomial likelihood",
    multinomial_logit = "Multinomial likelihood",
    ordered_logit = "Ordinal likelihood",
    ordered_probit = "Ordinal likelihood",
    ols = "Gaussian linear fit",
    wls = "Weighted linear fit",
    ols_robust = "OLS fit; robust inference",
    robust_m = "Robust M-estimation",
    fixest = "Linear fit",
    ivreg = "IV / 2SLS fit",
    quantile = "Quantile objective",
    tobit = "Censored likelihood",
    heckman = "Selection-model likelihood / two-step",
    time_static = "Gaussian temporal regression",
    distributed_lag = "Gaussian distributed-lag regression",
    dynamic_regression = "Gaussian dynamic regression",
    ardl = "Gaussian ARDL finite-lag regression",
    ecm = "Error-correction short-run equation",
    var = "VAR system equation",
    vecm = "VECM differenced system equation",
    "Estimator-specific"
  )
}

.ec_model_fit_table <- function(cmp, outcome_type = NULL) {
  if (!nrow(cmp)) return(data.frame())
  if (is.null(outcome_type) && "outcome_type" %in% names(cmp)) outcome_type <- unique(cmp$outcome_type)[1L]
  cols <- intersect(c("model", "engine", "nobs", "r2", "adj_r2", "logLik", "aic", "bic", "deviance", "unavailable_stats"), names(cmp))
  z <- unique(cmp[, cols, drop = FALSE])
  z$fit_basis <- vapply(z$engine, .ec_fit_basis, character(1))

  if (identical(outcome_type, "binary")) {
    # Keep estimator-native metrics, but avoid presenting unlike likelihoods as one ranking scale.
    is_lpm <- z$engine == "lpm"
    likelihood <- z$engine %in% c("logit", "probit")
    if ("logLik" %in% names(z)) z$logLik[is_lpm] <- NA_real_
    if ("aic" %in% names(z)) z$aic[is_lpm] <- NA_real_
    if ("bic" %in% names(z)) z$bic[is_lpm] <- NA_real_
    if ("deviance" %in% names(z)) z$deviance[is_lpm] <- NA_real_
    if ("r2" %in% names(z)) z$r2[likelihood] <- NA_real_
    if ("adj_r2" %in% names(z)) z$adj_r2[likelihood] <- NA_real_
  }
  z
}

.ec_model_fit_note <- function(outcome_type) {
  switch(outcome_type,
    binary = paste(
      "LPM and binary GLMs do not share the same fit scale.",
      "R-squared is shown for the LPM; likelihood-based measures are shown for logit/probit.",
      "Use the Binary comparison panel for common descriptive prediction summaries, not as an automatic model-selection rule."
    ),
    nominal = "Likelihood-based fit measures describe the multinomial model. They should be interpreted together with category probabilities and substantive plausibility, not as a standalone selection rule.",
    ordinal = "Likelihood-based fit measures describe the ordered model. Compare them only across models fitted to the same ordered outcome and estimation sample.",
    system = "VAR/VECM coefficients belong to equations within one multivariate system. Equation-level fit statistics are intentionally not used as a ranking device for the system; inspect lag structure, cointegration rank, stability and residual diagnostics instead.",
    "Fit measures are estimator-specific. Compare them only when they are defined on a compatible statistical basis and use the same outcome/sample."
  )
}
