.ec_args <- function(model_args, engine) {
  z <- model_args[[engine]]
  if (is.null(z)) list() else z
}

.ec_do <- function(fun, base_args, extra_args = list()) do.call(fun, c(base_args, extra_args))

.ec_name_quantile <- function(tau) paste0("quantile_q", gsub("\\.", "_", format(tau, trim = TRUE, scientific = FALSE)))

.ec_validate_wls_weights <- function(w, n) {
  if (!is.numeric(w) || length(w) != n) .ec_stop("WLS weights must be a numeric vector with one value per row of `data`.")
  if (any(!is.finite(w)) || any(w < 0) || !any(w > 0)) .ec_stop("WLS weights must be finite, non-negative, and contain at least one positive value.")
  w
}

.ec_validate_ols_reference_args <- function(a) {
  if (is.null(a)) return(invisible(TRUE))
  forbidden <- intersect(names(a), c("weights"))
  if (length(forbidden)) {
    .ec_stop(
      "The OLS engine cannot use `", paste(forbidden, collapse = "`, `"),
      "`. Select engine `wls` for weighted least squares so that `ols` remains ordinary unweighted least squares."
    )
  }
  invisible(TRUE)
}

.ec_validate_ols_robust_args <- function(a) {
  if (is.null(a)) return(invisible(TRUE))
  forbidden <- intersect(names(a), c("weights", "fixed_effects"))
  if (length(forbidden)) {
    .ec_stop(
      "`ols_robust` is reserved for the same unweighted OLS coefficients with robust inference. ",
      "Do not pass `", paste(forbidden, collapse = "`, `"), "`. Use `wls` for weighting or a dedicated engine such as `fixest` when the estimator itself changes."
    )
  }
  invisible(TRUE)
}

.ec_validate_quantile_summary_args <- function(a) {
  if (is.null(a)) return(list())
  if (!is.list(a)) .ec_stop("model_args$quantile$summary_args must be a list.")
  reserved <- intersect(names(a), c("object", "se"))
  if (length(reserved)) {
    .ec_stop(
      "model_args$quantile$summary_args cannot override reserved argument(s): `",
      paste(reserved, collapse = "`, `"), "`. Set the quantile `se` method with model_args$quantile$se."
    )
  }
  a
}

.ec_validate_tobit_response <- function(data, formula, left, right, subset = NULL) {
  mf_args <- list(formula = formula, data = data, na.action = stats::na.pass)
  if (!is.null(subset)) mf_args$subset <- subset
  mf <- tryCatch(do.call(stats::model.frame, mf_args), error = function(e) e)
  if (inherits(mf, "error")) .ec_stop("Could not validate the Tobit response: ", conditionMessage(mf))
  y <- stats::model.response(mf)
  if (!is.numeric(y) || is.matrix(y)) .ec_stop("Tobit requires a single numeric response variable.")
  y <- y[!is.na(y)]
  if (!length(y)) .ec_stop("Tobit requires at least one non-missing response observation.")
  if (is.finite(left) && any(y < left)) {
    .ec_stop("Observed Tobit responses fall below the declared left-censoring bound. Values below a finite censoring point must be recorded at the censoring value.")
  }
  if (is.finite(right) && any(y > right)) {
    .ec_stop("Observed Tobit responses exceed the declared right-censoring bound. Values above a finite censoring point must be recorded at the censoring value.")
  }
  invisible(TRUE)
}

.ec_is_binary_indicator <- function(x) {
  y <- x[!is.na(x)]
  if (!length(y)) return(FALSE)
  if (is.logical(y)) return(length(unique(y)) == 2L)
  if (is.factor(y) || is.character(y)) return(length(unique(y)) == 2L)
  if (is.numeric(y) || is.integer(y)) {
    vals <- sort(unique(as.numeric(y)))
    return(length(vals) == 2L && identical(vals, c(0, 1)))
  }
  FALSE
}

.ec_validate_selection_indicator <- function(data, selection_formula, subset = NULL) {
  mf_args <- list(formula = selection_formula, data = data, na.action = stats::na.pass)
  if (!is.null(subset)) mf_args$subset <- subset
  mf <- tryCatch(do.call(stats::model.frame, mf_args), error = function(e) e)
  if (inherits(mf, "error")) .ec_stop("Could not validate the Heckman selection equation: ", conditionMessage(mf))
  y <- stats::model.response(mf)
  y <- y[!is.na(y)]
  if (!length(y)) .ec_stop("Heckman selection requires a non-missing binary selection indicator.")

  if (!.ec_is_binary_indicator(y)) {
    .ec_stop("Heckman selection indicator must be binary: logical, character/factor with two observed states, or numeric coded 0/1 with both states represented.")
  }
  invisible(TRUE)
}

.ec_model_sample_info <- function(model, data_ids, name, engine) {
  data_ids <- as.character(data_ids)
  data_n <- length(data_ids)
  n_used <- tryCatch(as.integer(stats::nobs(model))[1L], error = function(e) NA_integer_)
  if (!length(n_used) || is.na(n_used)) n_used <- NA_integer_

  if (!is.finite(n_used) && identical(engine, "ols_robust")) {
    n_used <- tryCatch(as.integer(model$N)[1L], error = function(e) NA_integer_)
  }
  if (!is.finite(n_used) && identical(engine, "heckman")) {
    n_used <- tryCatch(as.integer(model$param$nObs)[1L], error = function(e) NA_integer_)
  }
  if (!is.finite(n_used) && !is.null(model$model) && is.data.frame(model$model)) {
    n_used <- nrow(model$model)
  }
  if (!length(n_used) || is.na(n_used)) n_used <- NA_integer_

  row_ids <- character()
  source <- "unavailable"
  identity <- "unavailable"
  n_effective <- n_used
  zero_weight_n <- 0L

  exact_engines <- c("ols", "wls", "lpm", "logit", "probit")
  if (engine %in% exact_engines) {
    mf <- tryCatch(stats::model.frame(model), error = function(e) NULL)
    if (!is.null(mf)) {
      cand <- rownames(mf)
      if (length(cand) && all(cand %in% data_ids) &&
          (!is.finite(n_used) || length(cand) == n_used)) {
        if (identical(engine, "wls")) {
          ww <- tryCatch(stats::weights(model), error = function(e) NULL)
          if (!is.null(ww) && length(ww) == length(cand)) {
            positive <- is.finite(ww) & ww > 0
            zero_weight_n <- sum(is.finite(ww) & ww == 0)
            cand <- cand[positive]
            n_effective <- length(cand)
            source <- "validated lm model.frame row names restricted to positive WLS weights"
          } else {
            source <- "validated lm model.frame row names; WLS effective-weight identity unavailable"
          }
        } else {
          source <- "validated lm model.frame row names"
        }
        row_ids <- as.character(cand)
        identity <- "exact"
      }
    }
  }

  if (!length(row_ids) && is.finite(n_used)) {
    source <- "nobs only"
    identity <- "sample_size_only"
  }

  data.frame(
    model = name,
    engine = engine,
    n_original = as.integer(data_n),
    n_used = n_used,
    n_effective = if (is.finite(n_effective)) as.integer(n_effective) else NA_integer_,
    zero_weight_n = as.integer(zero_weight_n),
    n_dropped = if (is.finite(n_used)) as.integer(data_n - n_used) else NA_integer_,
    row_identity = identity,
    row_identity_source = source,
    row_ids_available = identical(identity, "exact"),
    row_ids = if (length(row_ids)) paste(row_ids, collapse = "\u001f") else "",
    stringsAsFactors = FALSE
  )
}
#' Estimate and collect cross-sectional econometric models
#'
#' `econcompare` groups models by dependent-variable objective. Models from
#' different groups are intentionally not mixed in one call because their
#' coefficients and likelihoods generally answer different empirical questions.
#'
#' @param data A data.frame.
#' @param formula Main model formula.
#' @param models Character vector of engines. See [eco_models()]. OLS is not
#'   inserted automatically.
#' @param model_args Named list of engine-specific argument lists. For quantile
#'   regression, `se` and `summary_args` configure `summary.rq()` inference and
#'   are not passed to `quantreg::rq()`.
#' @param iv_formula Optional IV formula, e.g. `y ~ x1 + x2 | z1 + x2`.
#' @param fixest_formula Optional formula passed to `fixest::feols()`.
#' @param vcov Optional `vcov` argument passed to `fixest::feols()`.
#' @param selection_formula Selection equation for the Heckman model.
#' @param outcome_formula Outcome equation for the Heckman model. Defaults to `formula`.
#' @param binary_event For a character binary response, the category that should
#'   be coded as event = 1. Factor responses use their explicit factor level order
#'   unless this argument is supplied. Logical and numeric 0/1 responses are unambiguous.
#' @param error_policy Either `"stop"` or `"collect"`. In collect mode, failed
#'   estimators are stored and successful models remain available.
#' @return An object of class `econcompare`.
#' @export
eco_run <- function(data, formula, models = "ols", model_args = list(),
                    iv_formula = NULL, fixest_formula = NULL, vcov = NULL,
                    selection_formula = NULL, outcome_formula = NULL,
                    binary_event = NULL,
                    error_policy = c("stop", "collect")) {
  if (!is.data.frame(data)) .ec_stop("`data` must be a data.frame.")
  .ec_validate_data_columns(data)
  if (nrow(data) < 1L) .ec_stop("`data` must contain at least one row.")
  if (!inherits(formula, "formula")) .ec_stop("`formula` must be a formula.")
  if (!is.list(model_args)) .ec_stop("`model_args` must be a named list.")
  error_policy <- match.arg(error_policy)

  reg <- .ec_registry()
  models <- unique(as.character(models))
  if (!length(models)) .ec_stop("Choose at least one model. Run eco_models() to inspect the available engines.")
  bad <- setdiff(models, reg$engine)
  if (length(bad)) .ec_stop("Unsupported model(s): ", paste(bad, collapse = ", "), ". Run eco_models() to see available engines.")
  outcome_type <- .ec_models_outcome_type(models, reg)

  # Validate the response against the selected modelling objective before any fit.
  binary_info <- nominal_info <- NULL
  if (identical(outcome_type, "binary")) binary_info <- .ec_binary_model_data(data, formula, binary_event = binary_event)
  if (identical(outcome_type, "nominal")) nominal_info <- .ec_nominal_model_data(data, formula)
  ordinal_info <- NULL
  if (identical(outcome_type, "ordinal")) ordinal_info <- .ec_ordinal_model_data(data, formula)
  if (identical(outcome_type, "continuous")) {
    mf <- tryCatch(stats::model.frame(formula, data = data, na.action = stats::na.pass), error = function(e) e)
    if (inherits(mf, "error")) .ec_stop("Could not validate the continuous-outcome formula: ", conditionMessage(mf))
    yy <- stats::model.response(mf)
    if (!is.numeric(yy) || is.matrix(yy)) .ec_stop("Continuous-outcome models require a single numeric dependent variable.")
  }

  fits <- list(); meta <- list(); warnings <- list(); failures <- list(); sample_info <- list()
  add_fit <- function(name, fit, engine, meta_extra = list(), sample_data = data) {
    fits[[name]] <<- fit
    idx <- match(engine, reg$engine)
    base_meta <- list(
      engine = engine,
      outcome_type = reg$outcome_type[idx],
      family = reg$family[idx],
      estimator = reg$estimator[idx],
      comparison_note = reg$comparison_note[idx]
    )
    meta[[name]] <<- utils::modifyList(base_meta, meta_extra)
    sample_info[[name]] <<- .ec_model_sample_info(fit, rownames(sample_data), name, engine)
  }
  run_fit <- function(name, engine, fun, meta_extra = list(), sample_data = data) {
    captured <- character()
    ans <- tryCatch(withCallingHandlers(fun(), warning = function(w) {
      captured <<- c(captured, conditionMessage(w)); invokeRestart("muffleWarning")
    }), error = function(e) e)
    warnings[[name]] <<- unique(captured)
    if (inherits(ans, "error")) {
      if (identical(error_policy, "stop")) .ec_stop("Model `", name, "` failed: ", conditionMessage(ans))
      failures[[name]] <<- data.frame(model = name, engine = engine, message = conditionMessage(ans), stringsAsFactors = FALSE)
      return(invisible(NULL))
    }
    if (inherits(ans, "lm") && anyNA(stats::coef(ans))) {
      warnings[[name]] <<- unique(c(captured, "Some coefficients are not identifiable. NA rows are retained with term_status; remove redundant regressors explicitly."))
    }
    add_fit(name, ans, engine, meta_extra = meta_extra, sample_data = sample_data)
    invisible(ans)
  }

  if ("ols" %in% models) {
    ols_args <- .ec_args(model_args, "ols")
    .ec_validate_ols_reference_args(ols_args)
    run_fit("ols", "ols", function() .ec_do(stats::lm, list(formula = formula, data = data), ols_args))
  }

  if ("wls" %in% models) {
    a <- .ec_args(model_args, "wls"); w <- a$weights
    if (is.character(w) && length(w) == 1L) {
      if (!w %in% names(data)) .ec_stop("Unknown WLS weight column: `", w, "`.")
      w <- data[[w]]
    }
    if (is.null(w)) .ec_stop("`wls` requires model_args$wls$weights (a vector or column name).")
    w <- .ec_validate_wls_weights(w, nrow(data)); a$weights <- NULL
    run_fit("wls", "wls", function() .ec_do(stats::lm, list(formula = formula, data = data, weights = w), a))
  }

  if ("ols_robust" %in% models) {
    a <- .ec_args(model_args, "ols_robust"); if (is.null(a$se_type)) a$se_type <- "HC3"
    .ec_validate_ols_robust_args(a)
    run_fit("ols_robust", "ols_robust", function() {
      .ec_require("estimatr", "ols_robust")
      .ec_do(estimatr::lm_robust, list(formula = formula, data = data), a)
    }, meta_extra = list(inference = paste0("estimatr::lm_robust se_type=", a$se_type)))
  }

  if ("robust_m" %in% models) {
    run_fit("robust_m", "robust_m", function() {
      .ec_require("MASS", "robust_m")
      .ec_do(MASS::rlm, list(formula = formula, data = data), .ec_args(model_args, "robust_m"))
    }, meta_extra = list(inference = "asymptotic approximation"))
  }

  if ("fixest" %in% models) {
    fml <- if (is.null(fixest_formula)) formula else fixest_formula
    a <- .ec_args(model_args, "fixest"); if (!is.null(vcov) && is.null(a$vcov)) a$vcov <- vcov
    run_fit("fixest", "fixest", function() {
      .ec_require("fixest", "fixest")
      .ec_do(fixest::feols, list(fml = fml, data = data), a)
    }, meta_extra = list(inference = "native fixest inference"))
  }

  if ("ivreg" %in% models) {
    fml <- if (is.null(iv_formula)) model_args$ivreg$formula else iv_formula
    if (is.null(fml)) .ec_stop("`ivreg` requires `iv_formula` or model_args$ivreg$formula.")
    a <- .ec_args(model_args, "ivreg"); a$formula <- NULL
    run_fit("ivreg", "ivreg", function() {
      .ec_require("ivreg", "ivreg")
      .ec_do(ivreg::ivreg, list(formula = fml, data = data), a)
    }, meta_extra = list(inference = "native ivreg coefficient inference"))
  }

  if ("quantile" %in% models) {
    a <- .ec_args(model_args, "quantile")
    taus <- if (is.null(a$tau)) 0.5 else a$tau
    qse <- if (is.null(a$se) || !length(a$se)) "nid" else as.character(a$se)[1L]
    summary_args <- .ec_validate_quantile_summary_args(a$summary_args)
    a$tau <- NULL; a$se <- NULL; a$summary_args <- NULL
    if (!is.numeric(taus) || !length(taus) || any(!is.finite(taus)) || any(taus <= 0 | taus >= 1)) .ec_stop("Quantile `tau` values must be finite numbers strictly between 0 and 1.")
    supported_qse <- c("iid", "nid", "ker", "boot")
    qse <- tolower(qse)
    if (!qse %in% supported_qse) .ec_stop("econcompare currently supports quantile inference `se` methods: ", paste(supported_qse, collapse = ", "), ".")
    for (tau in unique(taus)) {
      nm <- .ec_name_quantile(tau)
      run_fit(nm, "quantile", function() {
        .ec_require("quantreg", "quantile")
        fit <- .ec_do(quantreg::rq, list(formula = formula, data = data, tau = tau), a)
        attr(fit, "econcompare_quantile_summary_args") <- c(list(se = qse), summary_args)
        fit
      }, meta_extra = list(inference = paste0("quantreg summary.rq se=", qse), tau = tau, se = qse))
    }
  }

  if ("tobit" %in% models) {
    a <- .ec_args(model_args, "tobit")
    left <- if (is.null(a$left)) -Inf else a$left; right <- if (is.null(a$right)) Inf else a$right
    if (length(left) != 1L || length(right) != 1L || is.na(left) || is.na(right) || left >= right) .ec_stop("Tobit censoring bounds must satisfy `left < right`.")
    .ec_validate_tobit_response(data, formula, left, right, subset = a$subset)
    run_fit("tobit", "tobit", function() {
      .ec_require("censReg", "tobit")
      .ec_do(censReg::censReg, list(formula = formula, data = data), a)
    }, meta_extra = list(inference = "censReg asymptotic inference"))
  }

  if ("heckman" %in% models) {
    sel <- if (is.null(selection_formula)) model_args$heckman$selection else selection_formula
    out_formula <- if (is.null(outcome_formula)) formula else outcome_formula
    if (is.null(sel)) .ec_stop("`heckman` requires `selection_formula` or model_args$heckman$selection.")
    a <- .ec_args(model_args, "heckman"); a$selection <- NULL; a$outcome <- NULL
    .ec_validate_heckman_formulas(sel, out_formula)
    .ec_validate_selection_indicator(data, sel, subset = a$subset)
    run_fit("heckman", "heckman", function() {
      .ec_require("sampleSelection", "heckman")
      .ec_do(sampleSelection::selection, list(selection = sel, outcome = out_formula, data = data), a)
    }, meta_extra = list(inference = "sampleSelection model inference"))
  }

  .ec_fit_binary_models(models, binary_info, formula, model_args, run_fit)
  .ec_fit_nominal_model(models, nominal_info, formula, model_args, run_fit)
  .ec_fit_ordinal_models(models, ordinal_info, formula, model_args, run_fit)

  if (!length(fits)) .ec_stop("No model could be estimated successfully.")

  failure_df <- if (length(failures)) do.call(rbind, failures) else data.frame(model=character(), engine=character(), message=character(), stringsAsFactors=FALSE)
  warning_df <- do.call(rbind, lapply(names(warnings), function(nm) {
    z <- warnings[[nm]]; if (!length(z)) return(NULL)
    engine <- if (!is.null(meta[[nm]])) meta[[nm]]$engine else if (nm %in% failure_df$model) failure_df$engine[match(nm, failure_df$model)] else nm
    data.frame(model = nm, engine = engine, message = z, stringsAsFactors = FALSE)
  }))
  if (is.null(warning_df)) warning_df <- data.frame(model=character(), engine=character(), message=character(), stringsAsFactors=FALSE)
  sample_df <- if (length(sample_info)) do.call(rbind, sample_info) else data.frame(model=character(), engine=character(), n_original=integer(), n_used=integer(), n_effective=integer(), zero_weight_n=integer(), n_dropped=integer(), row_identity=character(), row_identity_source=character(), row_ids_available=logical(), row_ids=character(), stringsAsFactors=FALSE)
  rownames(sample_df) <- NULL

  out <- list(call = match.call(), formula = formula, models = fits, meta = meta,
              outcome_type = outcome_type, analysis_type = "cross_section", warnings = warning_df, failures = failure_df,
              sample_info = sample_df, type_overrides = attr(data, "econcompare_type_overrides"),
              data_n = nrow(data), created = Sys.time(), version = .ec_version())
  out$provenance <- .ec_provenance(out)
  class(out) <- "econcompare"
  out
}

#' Audit estimation-sample consistency across models
#'
#' @param x An `econcompare` object.
#' @param reference Optional fitted model name used as the sample-comparison anchor.
#'   By default the first successfully fitted model is used. This is a technical
#'   sample anchor only; it is not an econometric reference model.
#' @return A data.frame with observations used by each model and sample-comparison information.
#' @export
eco_sample_audit <- function(x, reference = NULL) {
  if (!inherits(x, "econcompare")) .ec_stop("`x` must be an econcompare object.")
  z <- x$sample_info
  if (is.null(z) || !nrow(z)) return(data.frame())
  if (is.null(reference)) reference <- z$model[1L]
  if (!reference %in% z$model) .ec_stop("Unknown sample-audit reference model: `", reference, "`.")

  ref_i <- match(reference, z$model)
  ref_n <- if ("n_effective" %in% names(z) && is.finite(z$n_effective[ref_i])) z$n_effective[ref_i] else z$n_used[ref_i]
  ref_exact <- identical(z$row_identity[ref_i], "exact") && nzchar(z$row_ids[ref_i])
  ref_rows <- if (ref_exact) z$row_ids[ref_i] else ""

  compare_n <- if ("n_effective" %in% names(z)) ifelse(is.finite(z$n_effective), z$n_effective, z$n_used) else z$n_used
  z$reference_model <- reference
  z$matches_reference_n <- if (is.finite(ref_n)) compare_n == ref_n else NA
  z$matches_reference_rows <- vapply(seq_len(nrow(z)), function(i) {
    if (!ref_exact || !identical(z$row_identity[i], "exact") || !nzchar(z$row_ids[i])) return(NA)
    identical(z$row_ids[i], ref_rows)
  }, logical(1))

  z$comparison_certainty <- vapply(seq_len(nrow(z)), function(i) {
    if (!is.na(z$matches_reference_rows[i])) return("exact_row_identity")
    if (!is.na(z$matches_reference_n[i])) return("sample_size_only")
    "unavailable"
  }, character(1))
  z$sample_match <- vapply(seq_len(nrow(z)), function(i) {
    if ("observation_unit" %in% names(z) && !identical(z$observation_unit[i], z$observation_unit[ref_i])) return(paste0("different observation unit from ", reference))
    if ("observation_unit" %in% names(z) && !is.na(z$matches_reference_n[i]) && !z$matches_reference_n[i]) return(paste0("different effective sample size from ", reference))
    if (!is.na(z$matches_reference_rows[i])) {
      if (z$matches_reference_rows[i]) paste0("same rows as ", reference) else paste0("different rows from ", reference)
    } else if (!is.na(z$matches_reference_n[i])) {
      if (z$matches_reference_n[i]) "same sample size; row identity not verified" else paste0("different sample size from ", reference)
    } else "unknown"
  }, character(1))
  z$sample_warning <- vapply(seq_len(nrow(z)), function(i) {
    if ("observation_unit" %in% names(z) && isTRUE(z$matches_reference_rows[i]) && !identical(z$observation_unit[i], z$observation_unit[ref_i])) return("Same source rows, but different observation units and estimands; transformed sample sizes are not directly comparable")
    zero_w <- is.finite(z$zero_weight_n[i]) && z$zero_weight_n[i] > 0
    if (grepl("^different", z$sample_match[i])) {
      msg <- "comparison may reflect both estimator and sample differences"
    } else if (identical(z$comparison_certainty[i], "sample_size_only")) {
      msg <- "same size does not prove that the same observations were used"
    } else msg <- z$sample_match[i]
    if (zero_w) paste0(msg, "; ", z$zero_weight_n[i], " zero-weight WLS observation(s) are excluded from the effective estimation sample") else msg
  }, character(1))
  z
}
