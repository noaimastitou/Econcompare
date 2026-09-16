.ec_diag_registry <- function() {
  data.frame(
    id = c(
      "coefficient_significance", "global_significance", "confidence_intervals",
      "breusch_pagan", "white", "reset", "vif", "influence",
      "jarque_bera", "iv_diagnostics"
    ),
    diagnostic = c(
      "Coefficient significance", "Global significance", "Confidence intervals",
      "Koenker–Breusch–Pagan", "White", "Ramsey RESET", "VIF / GVIF",
      "Influence (Cook/leverage)", "Jarque-Bera", "IV diagnostics"
    ),
    category = c(
      "Inference", "Inference", "Inference", "Error variance", "Error variance",
      "Specification", "Multicollinearity", "Influence", "Residual distribution",
      "Endogeneity / instruments"
    ),
    applies_to = c(
      "ols,wls,ols_robust,robust_m,fixest,ivreg,quantile,tobit,heckman,lpm,logit,probit",
      "ols,wls,lpm",
      "ols,wls,ols_robust,fixest,ivreg,quantile,tobit,heckman,lpm,logit,probit",
      "ols", "ols", "ols", "ols", "ols", "ols", "ivreg"
    ),
    recommended_for = c(
      "ols,wls,ols_robust,fixest,ivreg,quantile,tobit,heckman,lpm,logit,probit",
      "ols,wls,lpm",
      "ols,wls,ols_robust,fixest,ivreg,quantile,tobit,heckman,lpm,logit,probit",
      "ols", "ols", "ols", "ols", "ols", "", "ivreg"
    ),
    purpose = c(
      "Summarises individual coefficient tests against zero.",
      "Tests whether slope coefficients are jointly zero in linear OLS/WLS models.",
      "Summarises model-based confidence intervals for estimated coefficients.",
      "Tests homoskedasticity using the studentized Koenker–Breusch–Pagan LM form.",
      "Tests the null of homoskedastic errors against a general quadratic variance specification.",
      "Checks for neglected nonlinear functions of the fitted values.",
      "Reports VIF for one-degree-of-freedom terms and adjusted GVIF for multi-parameter terms.",
      "Flags observations with large Cook distance or leverage using conventional screening thresholds.",
      "Tests residual skewness and kurtosis against Gaussian values; mainly an inference diagnostic in small samples.",
      "Reports weak-instrument, Wu-Hausman and overidentification diagnostics supplied by ivreg when available."
    ),
    stringsAsFactors = FALSE
  )
}

.ec_rule_applies <- function(rule, engine) {
  if (is.null(rule) || !length(rule) || is.na(rule) || !nzchar(rule)) return(FALSE)
  vals <- trimws(strsplit(rule, ",", fixed = TRUE)[[1L]])
  "all" %in% vals || engine %in% vals
}

.ec_diag_choice_table <- function(engine) {
  reg <- .ec_diag_registry()
  keep <- vapply(reg$applies_to, .ec_rule_applies, logical(1), engine = engine)
  out <- reg[keep, c("id", "diagnostic", "category", "recommended_for"), drop = FALSE]
  if (identical(engine, "ivreg") && "iv_diagnostics" %in% out$id) {
    out <- out[out$id != "iv_diagnostics", , drop = FALSE]
    iv <- data.frame(
      id = c("iv_weak_instruments", "iv_wu_hausman", "iv_overidentification"),
      diagnostic = c("Weak-instrument diagnostic", "Wu–Hausman endogeneity diagnostic", "Sargan overidentification diagnostic"),
      category = rep("Endogeneity / instruments", 3L),
      recommended_for = rep("ivreg", 3L),
      stringsAsFactors = FALSE
    )
    out <- rbind(out, iv)
  }
  out$recommended <- vapply(out$recommended_for, .ec_rule_applies, logical(1), engine = engine)
  out
}

#' List cross-section diagnostic tools
#'
#' @return A data.frame describing the operational cross-section diagnostic ids,
#'   including granular IV diagnostics, the model engines to which they apply,
#'   and whether they belong to the recommended selection for an engine.
#' @export
eco_diagnostic_tests <- function() {
  reg <- .ec_diag_registry()
  iv_i <- which(reg$id == "iv_diagnostics")
  if (length(iv_i)) {
    base <- reg[-iv_i, , drop = FALSE]
    proto <- reg[iv_i[1L], , drop = FALSE]
    iv <- proto[rep(1L, 3L), , drop = FALSE]
    iv$id <- c("iv_weak_instruments", "iv_wu_hausman", "iv_overidentification")
    iv$diagnostic <- c(
      "Weak-instrument diagnostic",
      "Wu–Hausman endogeneity diagnostic",
      "Sargan overidentification diagnostic"
    )
    iv$purpose <- c(
      "Assesses instrument relevance through the weak-instrument diagnostic returned by ivreg.",
      "Tests whether OLS and IV differ systematically under the Wu–Hausman test assumptions.",
      "Tests overidentifying restrictions when the IV specification is overidentified."
    )
    reg <- rbind(base, iv)
  }
  rownames(reg) <- NULL
  reg
}

.ec_diag_row <- function(model, engine, id, diagnostic, category,
                         statistic = NA_real_, df = NA_character_, p.value = NA_real_,
                         result = "informational", interpretation = "", details = "",
                         kind = "test", inference_basis = "model-specific") {
  data.frame(
    model = model,
    engine = engine,
    id = id,
    diagnostic = diagnostic,
    category = category,
    statistic = suppressWarnings(as.numeric(statistic)[1L]),
    df = as.character(df)[1L],
    p.value = suppressWarnings(as.numeric(p.value)[1L]),
    result = as.character(result)[1L],
    interpretation = as.character(interpretation)[1L],
    details = as.character(details)[1L],
    kind = as.character(kind)[1L],
    inference_basis = as.character(inference_basis)[1L],
    stringsAsFactors = FALSE
  )
}

.ec_diag_unavailable <- function(model, engine, id, diagnostic, category, reason) {
  .ec_diag_row(model, engine, id, diagnostic, category,
               result = "unavailable", interpretation = reason)
}

.ec_model_engine <- function(x, name) {
  z <- x$meta[[name]]$engine
  if (is.null(z) || !length(z)) name else z
}

.ec_coef_role <- function(model, engine, terms) {
  roles <- rep("slope", length(terms))
  roles[grepl("^\\(Intercept\\)$|^Intercept$", terms)] <- "intercept"

  if (identical(engine, "tobit")) {
    ancillary <- grepl("^(logSigma|sigma|logSigmaMu|logSigmaNu|sigmaMu|sigmaNu)$", terms, ignore.case = FALSE)
    roles[ancillary] <- "ancillary"
  }

  if (identical(engine, "heckman")) {
    # sampleSelection exposes the outcome equation explicitly via
    # coef(selection_object, part = "outcome"). Prefer exact prefixed names
    # (e.g. O:educ) so a regressor appearing in both equations is not
    # accidentally attributed to the outcome equation.
    outcome_names <- tryCatch(names(stats::coef(model, part = "outcome")), error = function(e) character())
    outcome_names <- outcome_names[nzchar(outcome_names)]

    strip_prefix <- function(x) {
      x <- sub("^(O|S)\\s*:\\s*", "", x, ignore.case = TRUE)
      x <- sub("^(outcome|selection|outcome equation|selection equation)\\s*[:.]\\s*", "", x, ignore.case = TRUE)
      trimws(x)
    }

    roles[] <- "other_equation_or_ancillary"
    is_outcome_prefix <- grepl("^(O\\s*:|outcome(?: equation)?\\s*[:.])", terms, ignore.case = TRUE)
    is_selection_prefix <- grepl("^(S\\s*:|selection(?: equation)?\\s*[:.])", terms, ignore.case = TRUE)
    is_outcome_exact <- length(outcome_names) > 0L & terms %in% outcome_names

    # Only use stripped-name matching for unprefixed coefficient tables.
    unprefixed <- !(is_outcome_prefix | is_selection_prefix)
    stripped_match <- rep(FALSE, length(terms))
    if (length(outcome_names)) {
      stripped_match[unprefixed] <- strip_prefix(terms[unprefixed]) %in% strip_prefix(outcome_names)
    }

    is_outcome <- is_outcome_prefix | is_outcome_exact | stripped_match
    stripped_terms <- strip_prefix(terms)
    outcome_intercept <- is_outcome & grepl("^\\(Intercept\\)$|^Intercept$", stripped_terms)
    roles[is_outcome] <- "outcome_slope"
    roles[outcome_intercept] <- "intercept"

    ancillary <- grepl("rho|sigma|mills|lambda|inv.*mills", stripped_terms, ignore.case = TRUE)
    roles[ancillary] <- "ancillary"
  }

  roles
}
.ec_clean_coef <- function(model, name, engine = NULL, purpose = c("slope", "all")) {
  purpose <- match.arg(purpose)
  if (is.null(engine)) engine <- if (inherits(model, "censReg")) "tobit" else if (inherits(model, "selection")) "heckman" else name
  out <- tryCatch(.ec_extract(model, name, missing = "na"), error = function(e) data.frame())
  if (!nrow(out)) return(out)
  out$term_role <- .ec_coef_role(model, engine, out$term)

  if (identical(purpose, "all")) return(out)

  keep <- if (identical(engine, "heckman")) {
    out$term_role == "outcome_slope"
  } else {
    out$term_role == "slope"
  }
  out[keep, , drop = FALSE]
}

.ec_native_ci <- function(model, engine, level) {
  if (engine %in% c("ols", "wls", "lpm")) {
    ci <- tryCatch(stats::confint(model, level = level), error = function(e) NULL)
    return(if (is.null(ci)) NULL else list(ci = as.matrix(ci), basis = "stats::confint.lm"))
  }

  if (identical(engine, "ols_robust")) {
    # estimatr provides a confint() method that respects the requested level and
    # the estimator's own finite-sample degrees-of-freedom calculations.
    ci <- tryCatch(stats::confint(model, level = level), error = function(e) NULL)
    if (!is.null(ci)) return(list(ci = as.matrix(ci), basis = "native estimatr confidence interval"))

    # Conservative fallback only when the object-stored interval was estimated
    # at exactly the requested level.
    object_alpha <- tryCatch(as.numeric(model$alpha)[1L], error = function(e) NA_real_)
    lo <- tryCatch(model$conf.low, error = function(e) NULL)
    hi <- tryCatch(model$conf.high, error = function(e) NULL)
    trm <- tryCatch(as.character(model$term), error = function(e) NULL)
    if (is.finite(object_alpha) && isTRUE(all.equal(object_alpha, 1 - level)) &&
        length(lo) && length(hi) && length(trm) &&
        length(lo) == length(hi) && length(lo) == length(trm)) {
      ci <- cbind(conf.low = as.numeric(lo), conf.high = as.numeric(hi))
      rownames(ci) <- trm
      return(list(ci = ci, basis = "estimatr object-stored confidence limits"))
    }
    return(NULL)
  }

  if (identical(engine, "fixest")) {
    ci <- tryCatch(stats::confint(model, level = level), error = function(e) NULL)
    return(if (is.null(ci)) NULL else list(ci = as.matrix(ci), basis = "native fixest confidence interval"))
  }

  if (identical(engine, "ivreg")) {
    ci <- tryCatch(stats::confint(model, level = level), error = function(e) NULL)
    return(if (is.null(ci)) NULL else list(ci = as.matrix(ci), basis = "native ivreg confidence interval"))
  }

  NULL
}

.ec_inference_spec <- function(model, engine, alpha) {
  level <- 1 - alpha
  native <- .ec_native_ci(model, engine, level)
  if (!is.null(native) && !is.null(native$ci) && nrow(native$ci) && ncol(native$ci) >= 2L) {
    return(list(critical = NA_real_, basis = native$basis, df = NA_real_, native = native$ci))
  }

  dfr <- tryCatch(stats::df.residual(model), error = function(e) NA_real_)
  dfr <- if (length(dfr) && is.finite(dfr[1L]) && dfr[1L] > 0) as.numeric(dfr[1L]) else NA_real_
  if (engine %in% c("ols", "wls", "lpm") && is.finite(dfr)) {
    return(list(critical = stats::qt(1 - alpha/2, dfr), basis = "t-based fallback interval", df = dfr, native = NULL))
  }

  if (engine %in% c("logit", "probit")) {
    return(list(
      critical = stats::qnorm(1 - alpha/2),
      basis = paste0("Wald interval for binomial ", engine, " coefficient inference"),
      df = NA_real_, native = NULL
    ))
  }

  if (identical(engine, "quantile")) {
    a <- attr(model, "econcompare_quantile_summary_args")
    if (is.null(a)) a <- list(se = "nid")
    se <- if (!is.null(a$se)) tolower(as.character(a$se)[1L]) else "nid"
    if (identical(se, "boot")) {
      return(list(
        critical = NA_real_,
        basis = "bootstrap standard errors are available, but econcompare does not reconstruct a bootstrap confidence interval from them",
        df = NA_real_,
        native = NULL
      ))
    }

    sm <- tryCatch(do.call(summary, c(list(object = model), a)), error = function(e) NULL)
    rdf <- if (!is.null(sm) && !is.null(sm$rdf) && length(sm$rdf)) suppressWarnings(as.numeric(sm$rdf)[1L]) else NA_real_
    if (is.finite(rdf) && rdf > 0) {
      return(list(
        critical = stats::qt(1 - alpha/2, df = rdf),
        basis = paste0("t-based Wald interval matching quantreg summary.rq se=", se, " with residual df=", format(rdf, trim = TRUE)),
        df = rdf,
        native = NULL
      ))
    }

    return(list(
      critical = NA_real_,
      basis = paste0("interval unavailable: quantreg summary.rq se=", se, " did not expose positive residual degrees of freedom required for inference-consistent Wald intervals"),
      df = NA_real_,
      native = NULL
    ))
  }

  if (engine %in% c("robust_m", "tobit", "heckman")) {
    return(list(critical = stats::qnorm(1 - alpha/2), basis = "asymptotic normal interval", df = NA_real_, native = NULL))
  }

  list(critical = NA_real_, basis = "interval unavailable: no validated inference rule", df = NA_real_, native = NULL)
}

.ec_match_native_ci <- function(ci, terms) {
  if (is.null(ci)) return(NULL)
  ci <- as.matrix(ci)
  rn <- rownames(ci)
  if (is.null(rn) || ncol(ci) < 2L) return(NULL)
  idx <- match(terms, rn)
  if (anyNA(idx)) return(NULL)
  data.frame(conf.low = suppressWarnings(as.numeric(ci[idx, 1L])),
             conf.high = suppressWarnings(as.numeric(ci[idx, 2L])), stringsAsFactors = FALSE)
}

.ec_diag_coef_significance <- function(model, name, engine, alpha) {
  co <- .ec_clean_coef(model, name, engine = engine, purpose = "slope")
  if (!nrow(co) || !"p.value" %in% names(co)) {
    return(.ec_diag_unavailable(name, engine, "coefficient_significance",
      "Coefficient significance", "Inference", "Coefficient p-values are unavailable for this estimator."))
  }
  ok <- is.finite(co$p.value)
  if (!any(ok)) {
    return(.ec_diag_unavailable(name, engine, "coefficient_significance",
      "Coefficient significance", "Inference", "Finite coefficient p-values are unavailable for this estimator."))
  }
  n_sig <- sum(co$p.value[ok] < alpha)
  n_test <- sum(ok)
  target_label <- if (identical(engine, "heckman")) {
    "outcome-equation slope coefficient(s)"
  } else if (identical(engine, "tobit")) {
    "latent-outcome slope coefficient(s)"
  } else {
    "slope coefficient(s)"
  }
  .ec_diag_row(
    name, engine, "coefficient_significance", "Coefficient significance", "Inference",
    statistic = NA_real_, p.value = NA_real_, result = "informational",
    interpretation = sprintf("%d of %d tested %s reject H0: beta = 0 at alpha = %.3g.", n_sig, n_test, target_label, alpha),
    details = "This is an aggregate summary. Inspect the coefficient-level table for each estimate, standard error, test statistic and p-value.",
    kind = "summary",
    inference_basis = if (identical(engine, "robust_m")) "asymptotic approximation" else "estimator-specific coefficient inference"
  )
}

.ec_diag_global <- function(model, name, engine, alpha) {
  sm <- tryCatch(summary(model), error = function(e) NULL)
  fs <- if (!is.null(sm)) sm$fstatistic else NULL
  if (is.null(fs) || length(fs) < 3L) {
    return(.ec_diag_unavailable(name, engine, "global_significance", "Global significance", "Inference",
                                "A model-level F statistic is not available in the expected form."))
  }
  f <- as.numeric(fs[[1L]]); df1 <- as.numeric(fs[[2L]]); df2 <- as.numeric(fs[[3L]])
  p <- stats::pf(f, df1, df2, lower.tail = FALSE)
  res <- if (is.finite(p) && p < alpha) "evidence_against_h0" else "no_evidence_against_h0"
  if (identical(engine, "lpm")) {
    details <- paste(
      "Classical linear-model F statistic for the linear probability model.",
      "Because a binary outcome is heteroskedastic by construction, use this as a descriptive joint-significance summary rather than a substitute for robust binary-model inference."
    )
    basis <- "classical LPM F statistic; binary-outcome caveat applies"
  } else if (identical(engine, "wls")) {
    details <- paste(
      "Model F statistic returned by the weighted linear model.",
      "Its inferential interpretation depends on the weighting scheme and the maintained variance assumptions;",
      "frequency, precision/inverse-variance, and ad-hoc weights do not have identical inferential meanings."
    )
    basis <- "weighted linear-model F inference; interpretation depends on weights"
  } else {
    details <- "Classical model F test; interpret together with the estimator and covariance assumptions."
    basis <- "classical F inference"
  }
  .ec_diag_row(name, engine, "global_significance", "Global significance", "Inference",
               statistic = f, df = paste0(df1, ", ", df2), p.value = p, result = res,
               interpretation = if (p < alpha)
                 "The joint null that all slope coefficients are zero is rejected."
               else "The joint null that all slope coefficients are zero is not rejected.",
               details = details,
               inference_basis = basis)
}

.ec_diag_ci <- function(model, name, engine, alpha) {
  co <- .ec_clean_coef(model, name, engine = engine, purpose = "slope")
  if (!nrow(co) || !all(c("estimate", "std.error") %in% names(co))) {
    return(.ec_diag_unavailable(name, engine, "confidence_intervals", "Confidence intervals", "Inference",
                                "Coefficient estimates or standard errors are unavailable."))
  }
  ok <- is.finite(co$estimate) & is.finite(co$std.error)
  if (!any(ok)) {
    return(.ec_diag_unavailable(name, engine, "confidence_intervals", "Confidence intervals", "Inference",
                                "Finite estimates and standard errors are unavailable."))
  }
  co <- co[ok, , drop = FALSE]
  spec <- .ec_inference_spec(model, engine, alpha)
  native <- .ec_match_native_ci(spec$native, co$term)
  if (!is.null(native)) {
    lo <- native$conf.low; hi <- native$conf.high
  } else if (is.finite(spec$critical)) {
    lo <- co$estimate - spec$critical * co$std.error
    hi <- co$estimate + spec$critical * co$std.error
  } else {
    return(.ec_diag_unavailable(name, engine, "confidence_intervals", "Confidence intervals", "Inference", spec$basis))
  }
  excludes <- sum(lo > 0 | hi < 0, na.rm = TRUE)
  target_label <- if (identical(engine, "heckman")) {
    "outcome-equation slope interval(s)"
  } else if (identical(engine, "tobit")) {
    "latent-outcome slope interval(s)"
  } else {
    "slope interval(s)"
  }
  .ec_diag_row(name, engine, "confidence_intervals", "Confidence intervals", "Inference",
               statistic = NA_real_, p.value = NA_real_, result = "informational",
               interpretation = sprintf("%d of %d %s at %.1f%% exclude zero.", excludes, length(lo), target_label, 100 * (1-alpha)),
               details = paste0("Intervals use ", spec$basis, ". Inspect the coefficient-level table for interval endpoints."),
               kind = "summary", inference_basis = spec$basis)
}

