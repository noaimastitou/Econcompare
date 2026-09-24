# Advanced explanatory time-series econometrics: ECM and multivariate systems.
# These tools deliberately separate single-equation error correction from
# system models (VAR/VECM). They do not perform forecasting or silently choose
# lag orders, cointegration ranks, deterministic terms, or identifying schemes.

.ec_require_numeric_series <- function(data, variables, label = "variables") {
  variables <- unique(as.character(variables))
  if (!length(variables)) .ec_stop("Choose at least one ", label, ".")
  bad <- setdiff(variables, names(data))
  if (length(bad)) .ec_stop("Unknown ", label, ": ", paste(bad, collapse = ", "), ".")
  nonnum <- variables[!vapply(data[variables], is.numeric, logical(1))]
  if (length(nonnum)) .ec_stop("Advanced time-series models require numeric series. Non-numeric variable(s): ", paste(nonnum, collapse = ", "), ".")
  invisible(variables)
}

.ec_require_complete_numeric_time_sample <- function(prep, variables, feature) {
  .ec_require_regular_time_grid(prep$audit, feature)
  .ec_require_numeric_series(prep$data, variables)
  bad <- variables[vapply(prep$data[variables], function(z) any(!is.finite(z)), logical(1))]
  if (length(bad)) {
    .ec_stop(
      feature, " requires a complete numeric sample on the selected regular time grid. Missing/non-finite values were found in: ",
      paste(bad, collapse = ", "), ". econcompare does not delete internal periods and reinterpret the remaining rows as adjacent lags."
    )
  }
  invisible(TRUE)
}

# Reject numerical singularity without selecting variables for the researcher.
.ec_validate_system_series <- function(data, variables, feature) {
  x <- as.matrix(data[, variables, drop = FALSE])
  constant <- vapply(seq_len(ncol(x)), function(j) all(x[, j] == x[1L, j]), logical(1))
  if (any(constant)) .ec_stop(feature, " cannot use constant endogenous series: ",
                            paste(variables[constant], collapse = ", "), ". Remove these series.")
  # Normalize before centering to avoid overflow and dependence on measurement units.
  x <- sweep(x, 2L, apply(abs(x), 2L, max), "/")
  x <- sweep(x, 2L, colMeans(x), "-")
  norms <- sqrt(colSums(x^2))
  if (any(!is.finite(norms) | norms == 0)) .ec_stop(feature, " has numerically constant series. Review their scale.")
  x <- sweep(x, 2L, norms, "/")
  if (qr(x, tol = 1e-10)$rank < ncol(x)) {
    .ec_stop(feature, " has collinear endogenous series (numerically rank-deficient): ",
             paste(variables, collapse = ", "), ". Remove redundant series; no variable is removed automatically.")
  }
  invisible(TRUE)
}

.ec_validate_system_equation <- function(m, label) {
  cf <- stats::coef(m)
  ncoef <- if (is.matrix(cf)) nrow(cf) else length(cf)
  if (!ncoef || is.null(m$rank) || m$rank < ncoef || any(!is.finite(cf))) {
    .ec_stop(label, " is rank-deficient or has non-finite coefficients. Reduce the lag order or remove collinear variables; no coefficients are silently dropped.")
  }
  invisible(TRUE)
}

.ec_validate_system_residuals <- function(resids, label) {
  z <- as.matrix(resids)
  if (!length(z) || any(!is.finite(z))) .ec_stop(label, " has non-finite residuals.")
  scales <- apply(abs(z), 2L, max)
  if (any(scales == 0)) .ec_stop(label, " has singular residual covariance (a perfectly fitted equation). Review the specification.")
  z <- sweep(z, 2L, scales, "/")
  if (qr(z, tol = 1e-10)$rank < ncol(z)) .ec_stop(label, " has singular residual covariance. Review redundant series or deterministic relationships.")
  invisible(TRUE)
}

.ec_exact_sample_info <- function(name, engine, prep, positions) {
  positions <- as.integer(positions)
  positions <- positions[is.finite(positions) & positions >= 1L & positions <= nrow(prep$data)]
  ids <- rownames(prep$data)[positions]
  data.frame(
    model = name,
    engine = engine,
    n_original = nrow(prep$data),
    n_used = length(ids),
    n_effective = length(ids),
    zero_weight_n = 0L,
    n_dropped = nrow(prep$data) - length(ids),
    row_identity = "exact",
    row_identity_source = "validated regular time-grid positions",
    row_ids_available = TRUE,
    row_ids = paste(ids, collapse = "\u001f"),
    stringsAsFactors = FALSE
  )
}

.ec_system_equation <- function(coef_table, equation, nobs, engine) {
  z <- list(
    coef_table = as.data.frame(coef_table, stringsAsFactors = FALSE),
    equation = as.character(equation)[1L],
    nobs = as.integer(nobs)[1L],
    engine = as.character(engine)[1L]
  )
  class(z) <- "econcompare_system_equation"
  z
}

.ec_clean_equation_name <- function(x) {
  x <- as.character(x)
  x <- sub("^Response\\s+", "", x)
  x <- sub("^d\\.", "Delta ", x)
  x
}

.ec_extract_lm_equations <- function(models, engine) {
  out <- list()
  for (nm in names(models)) {
    m <- models[[nm]]
    .ec_validate_system_equation(m, paste0(toupper(engine), " equation ", nm))
    sm <- summary(m)
    co <- sm$coefficients
    if (is.null(co)) next
    out[[nm]] <- .ec_system_equation(co, nm, stats::nobs(m), engine)
  }
  out
}

.ec_extract_mlm_equations <- function(mlm, engine) {
  .ec_validate_system_equation(mlm, "VECM equations")
  cf <- tryCatch(stats::coef(mlm), error = function(e) NULL)
  if (is.null(cf)) .ec_stop("Could not extract VECM equation coefficients from the fitted system.")
  cf <- as.matrix(cf)
  eq_names <- colnames(cf)
  if (is.null(eq_names)) eq_names <- paste0("equation_", seq_len(ncol(cf)))
  sm <- tryCatch(summary(mlm), error = function(e) NULL)
  n <- tryCatch(stats::nobs(mlm), error = function(e) NA_integer_)
  if (length(n) > 1L) n <- n[1L]
  if (!is.finite(n)) {
    rr <- tryCatch(stats::residuals(mlm), error = function(e) NULL)
    if (!is.null(rr)) n <- nrow(as.matrix(rr))
  }
  out <- list()
  for (j in seq_len(ncol(cf))) {
    co <- NULL
    if (is.list(sm) && length(sm) >= j && !is.null(sm[[j]]$coefficients)) co <- sm[[j]]$coefficients
    if (is.null(co)) {
      co <- cbind(Estimate = cf[, j], `Std. Error` = NA_real_, `t value` = NA_real_, `Pr(>|t|)` = NA_real_)
      rownames(co) <- rownames(cf)
    }
    eq <- .ec_clean_equation_name(eq_names[j])
    out[[eq]] <- .ec_system_equation(co, eq, n, engine)
  }
  out
}

.ec_companion_roots <- function(A) {
  if (!length(A)) return(numeric())
  A <- lapply(A, as.matrix)
  k <- nrow(A[[1L]])
  p <- length(A)
  if (!all(vapply(A, function(z) nrow(z) == k && ncol(z) == k, logical(1)))) return(numeric())
  if (!all(vapply(A, function(z) is.numeric(z) && all(is.finite(z)), logical(1)))) {
    .ec_stop("Companion roots are unavailable: non-finite coefficient matrices. The fitted system may be singular; re-estimate a full-rank specification.")
  }
  top <- do.call(cbind, A)
  if (p == 1L) return(eigen(top, only.values = TRUE)$values)
  lower <- cbind(diag(k * (p - 1L)), matrix(0, nrow = k * (p - 1L), ncol = k))
  comp <- rbind(top, lower)
  eigen(comp, only.values = TRUE)$values
}

#' Estimate a single-equation error-correction model
#'
#' Estimates a transparent two-step ECM. First, a long-run levels regression is
#' fitted. Its lagged residual becomes the error-correction term in a short-run
#' regression for the first difference of the dependent variable. This function
#' does not treat the first-step regression as proof of cointegration and does
#' not automatically difference, select lags, or choose deterministic terms.
#'
#' @param data A data.frame.
#' @param formula Levels relationship such as `y ~ x1 + x2` using simple numeric columns.
#' @param time Name of the validated time-index column.
#' @param p Number of lags of `Delta y` included in the short-run equation.
#' @param q Default number of lags of each `Delta x` after the contemporaneous difference.
#' @param q_by_var Optional named integer vector/list overriding `q` for selected explanatory variables.
#' @param long_run_intercept Include an intercept in the long-run levels equation.
#' @param inference `"classical"` or `"HAC"` for the short-run ECM coefficient inference.
#' @param hac_lag Optional Newey-West truncation lag.
#' @return An `econcompare` object. `eco_compare()` reports the short-run ECM;
#'   `eco_ecm_long_run()` reports the first-step long-run regression.
#' @export
eco_ecm_run <- function(data, formula, time, p = 1L, q = 0L, q_by_var = NULL,
                        long_run_intercept = TRUE,
                        inference = c("classical", "HAC"), hac_lag = NULL) {
  if (!is.data.frame(data)) .ec_stop("`data` must be a data.frame.")
  .ec_validate_data_columns(data)
  if (!inherits(formula, "formula")) .ec_stop("`formula` must be a formula.")
  if (length(time) != 1L || !is.character(time) || !time %in% names(data)) .ec_stop("`time` must name one column in `data`.")
  if (!is.logical(long_run_intercept) || length(long_run_intercept) != 1L || is.na(long_run_intercept)) .ec_stop("`long_run_intercept` must be TRUE or FALSE.")
  inference <- match.arg(inference)

  prep <- .ec_prepare_time_data(data, time)
  parts <- .ec_time_formula_parts(formula, prep$data)
  if (time %in% c(parts$y, parts$x)) .ec_stop("The time-index variable cannot also be a variable in the ECM relationship.")
  vars <- c(parts$y, parts$x)
  .ec_require_complete_numeric_time_sample(prep, vars, "ECM estimation")

  n <- nrow(prep$data)
  max_allowed <- max(0L, min(20L, floor((n - 5L) / 4L)))
  p <- .ec_validate_lag_order(p, "p", max_allowed)
  q_spec <- .ec_normalize_q_by_var(parts$x, q, q_by_var, max_allowed)

  generated <- c(paste0("D_", parts$y), "ECT_L1",
    if (p > 0L) paste0("D_", parts$y, "_L", seq_len(p)) else character(),
    unlist(lapply(parts$x, function(v) c(paste0("D_", v),
      if (q_spec[[v]] > 0L) paste0("D_", v, "_L", seq_len(q_spec[[v]])) else character())), use.names = FALSE))
  .ec_validate_generated_names(generated)

  long_formula <- if (isTRUE(long_run_intercept)) formula else stats::update.formula(formula, . ~ . - 1)
  long_fit <- stats::lm(long_formula, data = prep$data, na.action = stats::na.fail)
  if (long_fit$rank < length(stats::coef(long_fit))) {
    .ec_stop("The ECM long-run levels equation is rank-deficient. Remove collinear/constant regressors before estimating the ECM.")
  }
  ect <- stats::residuals(long_fit)

  dy <- c(NA_real_, diff(prep$data[[parts$y]]))
  short <- data.frame(.ec_dy = dy, .ec_ect_l1 = .ec_lag_vec(ect, 1L), check.names = FALSE)
  names(short)[1:2] <- c(paste0("D_", parts$y), "ECT_L1")
  rhs <- "ECT_L1"

  if (p > 0L) {
    for (kk in seq_len(p)) {
      nm <- paste0("D_", parts$y, "_L", kk)
      short[[nm]] <- .ec_lag_vec(dy, kk)
      rhs <- c(rhs, nm)
    }
  }
  for (v in parts$x) {
    dx <- c(NA_real_, diff(prep$data[[v]]))
    nm0 <- paste0("D_", v)
    short[[nm0]] <- dx
    rhs <- c(rhs, nm0)
    qv <- q_spec[[v]]
    if (qv > 0L) {
      for (kk in seq_len(qv)) {
        nm <- paste0("D_", v, "_L", kk)
        short[[nm]] <- .ec_lag_vec(dx, kk)
        rhs <- c(rhs, nm)
      }
    }
  }
  rownames(short) <- rownames(prep$data)
  short_formula <- stats::reformulate(vapply(rhs, .ec_quote_name, character(1)), response = .ec_quote_name(paste0("D_", parts$y)))
  cc <- stats::complete.cases(short)
  n_eff <- sum(cc)
  params <- 1L + length(rhs)
  if (n_eff <= params + 5L) {
    .ec_stop("The ECM short-run equation is over-parameterized: ", n_eff, " usable observations for about ", params, " coefficients. Reduce p/q or the number of regressors.")
  }
  mm <- stats::model.matrix(short_formula, short[cc, , drop = FALSE])
  if (qr(mm)$rank < ncol(mm)) .ec_stop("The ECM short-run design matrix is rank-deficient. Reduce the lag structure or remove collinear variables.")

  fit <- stats::lm(short_formula, data = short, na.action = stats::na.omit)
  if (identical(inference, "HAC")) {
    if (!.ec_model_sample_is_contiguous(fit, prep)) .ec_stop("HAC/Newey-West inference is not run because the ECM estimation sample contains internal calendar gaps.")
  }
  fit <- .ec_attach_time_inference(fit, inference, hac_lag)
  periods <- .ec_time_periods_used(fit, prep)
  used_rows <- rownames(stats::model.frame(fit))
  pos <- match(used_rows, rownames(prep$data))

  q_common <- if (length(unique(q_spec)) == 1L) unname(q_spec[1L]) else NA_integer_
  meta <- list(ecm = list(
    engine = "ecm", outcome_type = "continuous", family = "Error-correction model",
    estimator = "Two-step single-equation ECM",
    comparison_note = "Short-run equation in first differences with an explicitly estimated lagged error-correction term. Cointegration is a researcher-assessed prerequisite, not an automatic conclusion; reported second-step inference is conditional on the constructed first-step ECT.",
    analysis_type = "time_series", temporal_family = "ecm", time_variable = time,
    p = p, q = q_common, q_by_var = q_spec, inference = inference,
    hac_lag = attr(fit, "econcompare_hac_lag"),
    estimation_start = if (length(periods)) periods[1L] else NA_character_,
    estimation_end = if (length(periods)) tail(periods, 1L) else NA_character_, n_time_used = length(periods)
  ))
  time_md <- .ec_time_metadata(prep, time, formula, meta, inference, hac_lag, p, q_common, q_spec)
  time_md$temporal_family <- "ecm"
  time_md$long_run_formula <- long_formula
  time_md$short_run_formula <- short_formula
  time_md$cointegration_note <- "ECM estimation is conditional on a substantively and statistically defensible long-run relationship. econcompare does not infer cointegration merely because a levels regression was estimated. Reported second-step coefficient inference is conditional on the constructed first-step error-correction term."

  out <- list(
    call = match.call(), formula = formula, models = list(ecm = fit), meta = meta,
    outcome_type = "continuous", analysis_type = "time_series", temporal_family = "ecm",
    time_variable = time, time_audit = prep$audit, time_metadata = time_md,
    reordered_time = prep$reordered, p = p, q = q_common, q_by_var = q_spec,
    inference = inference, long_run_model = long_fit, long_run_formula = long_formula,
    short_run_formula = short_formula, error_correction_term = ect,
    warnings = data.frame(model = character(), engine = character(), message = character(), stringsAsFactors = FALSE),
    failures = data.frame(model = character(), engine = character(), message = character(), stringsAsFactors = FALSE),
    sample_info = .ec_exact_sample_info("ecm", "ecm", prep, pos),
    sample_rows = list(ecm = used_rows), sample_comparable = TRUE,
    type_overrides = attr(data, "econcompare_type_overrides"),
    ordinal_levels = attr(data, "econcompare_ordinal_levels"),
    data_n = nrow(data), created = Sys.time(), version = .ec_version()
  )
  out$provenance <- .ec_provenance(out)
  class(out) <- "econcompare"
  out
}

#' Long-run levels relationship used by an ECM
#' @param x An object returned by [eco_ecm_run()].
#' @return A tidy coefficient table for the first-step long-run regression.
#' @export
eco_ecm_long_run <- function(x) {
  if (!inherits(x, "econcompare") || !identical(x$temporal_family, "ecm") || is.null(x$long_run_model)) {
    .ec_stop("`x` must be an ECM result returned by eco_ecm_run().")
  }
  z <- .ec_extract_standard(x$long_run_model, "ecm_long_run", missing = "na")
  z$note <- "First-step levels relationship used to construct ECT. It is not, by itself, a cointegration test."
  z
}

#' Compare candidate VAR lag orders without choosing one automatically
#' @param data A data.frame.
#' @param variables Numeric endogenous system variables.
#' @param time Time-index column.
#' @param lag_max Maximum VAR lag order considered.
#' @param deterministic Deterministic terms passed to `vars::VARselect`: `"const"`, `"trend"`, `"both"`, or `"none"`.
#' @return A list with information-criterion values and the package-reported minima. The researcher remains responsible for the lag choice.
#' @export
eco_var_lag_selection <- function(data, variables, time, lag_max = 8L,
                                  deterministic = c("const", "trend", "both", "none")) {
  if (!requireNamespace("vars", quietly = TRUE)) .ec_stop("VAR lag selection requires optional package `vars`.")
  prep <- .ec_prepare_time_data(data, time)
  variables <- unique(as.character(variables))
  if (length(variables) < 2L) .ec_stop("VAR lag selection requires at least two endogenous variables.")
  .ec_require_complete_numeric_time_sample(prep, variables, "VAR lag selection")
  .ec_validate_system_series(prep$data, variables, "VAR lag selection")
  deterministic <- match.arg(deterministic)
  lag_max <- .ec_validate_lag_order(lag_max, "lag_max", max(1L, min(20L, floor((nrow(prep$data) - 5L) / (length(variables) + 1L)))))
  if (lag_max < 1L) .ec_stop("`lag_max` must be at least 1 for VAR lag selection.")
  det_n <- switch(deterministic, none = 0L, const = 1L, trend = 1L, both = 2L, 1L)
  worst_params <- length(variables) * lag_max + det_n
  if ((nrow(prep$data) - lag_max) <= worst_params + 5L) {
    .ec_stop("`lag_max` is too ambitious for the system dimension and available sample. Reduce lag_max or the number of endogenous variables.")
  }
  ans <- vars::VARselect(prep$data[, variables, drop = FALSE], lag.max = lag_max, type = deterministic)
  crit <- as.matrix(ans$criteria)
  if (any(!is.finite(crit))) .ec_stop("VAR lag-selection criteria are non-finite. Review collinearity, residual covariance and the requested lag range.")
  tab <- data.frame(criterion = rep(rownames(crit), times = ncol(crit)), p = rep(seq_len(ncol(crit)), each = nrow(crit)), value = as.numeric(crit), stringsAsFactors = FALSE)
  selection <- data.frame(criterion = names(ans$selection), suggested_p = as.integer(ans$selection), stringsAsFactors = FALSE)
  list(criteria = tab, selection = selection,
       note = "Information criteria are reported as decision aids only. econcompare does not select a lag order automatically.")
}

.ec_johansen_table <- function(jo, type, K, ecdet, spec) {
  st <- as.numeric(methods::slot(jo, "teststat"))
  cv <- as.matrix(methods::slot(jo, "cval"))
  labs <- rownames(cv)
  if (is.null(labs) || length(labs) != length(st)) labs <- paste0("rank hypothesis ", seq_along(st))
  labs <- trimws(sub("[[:space:]]*[|][[:space:]]*$", "", labs))
  pick <- function(nm) if (nm %in% colnames(cv)) as.numeric(cv[, nm]) else rep(NA_real_, nrow(cv))
  data.frame(
    null_rank_hypothesis = labs,
    statistic = st,
    critical_10pct = pick("10pct"),
    critical_5pct = pick("5pct"),
    critical_1pct = pick("1pct"),
    test = type,
    K = K,
    deterministic = ecdet,
    specification = spec,
    note = "Compare each statistic with the corresponding critical value. econcompare does not choose the cointegration rank automatically.",
    stringsAsFactors = FALSE
  )
}

#' Johansen cointegration rank diagnostics
#' @param data A data.frame.
#' @param variables Numeric system variables in levels.
#' @param time Time-index column.
#' @param K Lag order of the VAR in levels used by `urca::ca.jo`; must be at least 2.
#' @param type Johansen statistic: `"trace"` or `"eigen"`.
#' @param ecdet Deterministic term in the cointegration relations: `"none"`, `"const"`, or `"trend"`. This is not a switch for all deterministic terms in the system.
#' @param spec Johansen specification: `"transitory"` or `"longrun"`.
#' @return A data.frame of test statistics and critical values. No rank is selected automatically.
#' @export
eco_johansen_test <- function(data, variables, time, K = 2L,
                               type = c("trace", "eigen"),
                               ecdet = c("const", "none", "trend"),
                               spec = c("transitory", "longrun")) {
  if (!requireNamespace("urca", quietly = TRUE)) .ec_stop("Johansen cointegration diagnostics require optional package `urca`.")
  prep <- .ec_prepare_time_data(data, time)
  variables <- unique(as.character(variables))
  if (length(variables) < 2L) .ec_stop("Johansen diagnostics require at least two numeric series in levels.")
  .ec_require_complete_numeric_time_sample(prep, variables, "Johansen cointegration diagnostics")
  .ec_validate_system_series(prep$data, variables, "Johansen cointegration diagnostics")
  type <- match.arg(type); ecdet <- match.arg(ecdet); spec <- match.arg(spec)
  max_k <- max(2L, min(20L, floor((nrow(prep$data) - 5L) / (length(variables) + 1L))))
  K <- .ec_validate_lag_order(K, "K", max_k)
  if (K < 2L) .ec_stop("Johansen/VECM analysis requires `K >= 2` (VAR lag order in levels).")
  approx_regressors <- length(variables) * K + length(variables) + 2L
  if ((nrow(prep$data) - K) <= approx_regressors + 5L) {
    .ec_stop("The Johansen specification is too highly parameterized for the available sample. Reduce K or the number of system variables.")
  }
  jo <- tryCatch(urca::ca.jo(prep$data[, variables, drop = FALSE], type = type, ecdet = ecdet, K = K, spec = spec),
                 error = function(e) .ec_stop("Johansen estimation failed; review system rank and lag specification: ", conditionMessage(e)))
  .ec_johansen_table(jo, type, K, ecdet, spec)
}

.ec_system_registry <- function() {
  data.frame(
    engine = c("var", "vecm"),
    estimator = c("Vector autoregression (VAR)", "Vector error-correction model (VECM)"),
    package = c("vars", "urca + vars"),
    description = c(
      "Reduced-form multivariate dynamic system in levels; all selected series are endogenous.",
      "Cointegrated multivariate system estimated in the Johansen framework with an explicit researcher-supplied rank."
    ),
    stringsAsFactors = FALSE
  )
}

#' List multivariate time-series system engines
#' @return A data.frame describing VAR and VECM engines and local availability.
#' @export
eco_system_models <- function() {
  z <- .ec_system_registry()
  z$available <- c(
    requireNamespace("vars", quietly = TRUE),
    requireNamespace("urca", quietly = TRUE) && requireNamespace("vars", quietly = TRUE)
  )
  z
}

#' Estimate a VAR or VECM dynamic system
#'
#' VAR and VECM are treated as multivariate systems rather than as regressions
#' with one privileged dependent variable. For VECM, the researcher must provide
#' the cointegration rank explicitly; [eco_johansen_test()] can be used as a
#' diagnostic input to that decision.
#'
#' @param data A data.frame.
#' @param variables At least two numeric endogenous variables.
#' @param time Name of the validated time index.
#' @param model `"var"` or `"vecm"`.
#' @param p VAR lag order. For VECM this is the VAR lag order in levels (`K` in `urca::ca.jo`) and must be at least 2.
#' @param deterministic For VAR: `"const"`, `"trend"`, `"both"`, or `"none"`.
#' @param rank Cointegration rank for VECM. Required and must lie between 1 and `length(variables)-1`.
#' @param johansen_type `"trace"` or `"eigen"` for the VECM Johansen object.
#' @param ecdet Deterministic term in the cointegration relations: `"none"`, `"const"`, or `"trend"`. This is not a switch for all deterministic terms in the system.
#' @param spec VECM Johansen specification: `"transitory"` or `"longrun"`.
#' @return An `econcompare` object with equation-level coefficient wrappers and the retained system fit.
#' @details Constant or numerically collinear endogenous series, aliased equation
#' coefficients and singular residual covariance are rejected. No variable or lag
#' is removed automatically. Engine warnings are retained in `warnings` and are
#' also signalled normally. The permitted VECM rank is 1 through k-1.
#' @export
eco_system_run <- function(data, variables, time, model = c("var", "vecm"), p = 2L,
                           deterministic = c("const", "trend", "both", "none"),
                           rank = NULL, johansen_type = c("trace", "eigen"),
                           ecdet = c("const", "none", "trend"),
                           spec = c("transitory", "longrun")) {
  if (!is.data.frame(data)) .ec_stop("`data` must be a data.frame.")
  .ec_validate_data_columns(data)
  if (!requireNamespace("vars", quietly = TRUE)) .ec_stop("VAR/VECM system estimation requires optional package `vars`.")
  model <- match.arg(model)
  prep <- .ec_prepare_time_data(data, time)
  variables <- unique(as.character(variables))
  if (length(variables) < 2L) .ec_stop("VAR/VECM systems require at least two endogenous variables.")
  if (time %in% variables) .ec_stop("The time index is structural metadata and cannot be an endogenous system variable.")
  .ec_require_complete_numeric_time_sample(prep, variables, toupper(model))
  .ec_validate_system_series(prep$data, variables, toupper(model))
  k <- length(variables); n <- nrow(prep$data)
  max_p <- max(1L, min(20L, floor((n - 5L) / (k + 1L))))
  p <- .ec_validate_lag_order(p, "p", max_p)
  if (p < 1L) .ec_stop("VAR lag order `p` must be at least 1.")

  if (identical(model, "var")) {
    det_choice <- match.arg(deterministic)
    det_n <- switch(det_choice, none = 0L, const = 1L, trend = 1L, both = 2L, 1L)
    approx_params <- k * p + det_n
    if ((n - p) <= approx_params + 5L) .ec_stop("The VAR is over-parameterized for the available sample. Reduce p or the number of endogenous variables.")
  } else {
    approx_params <- k * (p - 1L) + max(1L, k - 1L) + 2L
    if ((n - p) <= approx_params + 5L) .ec_stop("The VECM/Johansen system is over-parameterized for the available sample. Reduce p or the number of endogenous variables.")
  }

  failures <- data.frame(model = character(), engine = character(), message = character(), stringsAsFactors = FALSE)
  warnings <- data.frame(model = character(), engine = character(), message = character(), stringsAsFactors = FALSE)
  capture_warning <- function(w) {
    warnings <<- rbind(warnings, data.frame(model = toupper(model), engine = model,
                                           message = conditionMessage(w), stringsAsFactors = FALSE))
  }
  run_engine <- function(expr) {
    tryCatch(withCallingHandlers(expr, warning = capture_warning),
             error = function(e) .ec_stop(toupper(model), " estimation failed: ", conditionMessage(e)))
  }
  system_fit <- NULL; jo <- NULL; rank_used <- NULL; model_wrappers <- list(); used_positions <- integer()

  if (identical(model, "var")) {
    deterministic <- det_choice
    raw <- run_engine(vars::VAR(prep$data[, variables, drop = FALSE], p = p, type = deterministic))
    system_fit <- raw
    model_wrappers <- run_engine(.ec_extract_lm_equations(raw$varresult, "var"))
    .ec_validate_system_residuals(stats::residuals(raw), "VAR")
    if (!length(model_wrappers)) .ec_stop("VAR was fitted but its equation results could not be extracted.")
    used_positions <- seq.int(p + 1L, n)
    det_note <- deterministic
  } else {
    if (!requireNamespace("urca", quietly = TRUE)) .ec_stop("VECM estimation requires optional package `urca` in addition to `vars`.")
    johansen_type <- match.arg(johansen_type); ecdet <- match.arg(ecdet); spec <- match.arg(spec)
    if (p < 2L) .ec_stop("VECM requires `p >= 2`, where p is the VAR lag order in levels (K in Johansen notation).")
    if (!is.numeric(rank) || length(rank) != 1L || !is.finite(rank) || abs(rank - round(rank)) > 1e-8) {
      .ec_stop("VECM requires an explicit integer `rank`. Use eco_johansen_test() as one input to the rank decision; econcompare does not select it automatically.")
    }
    if (rank < 1 || rank >= k) .ec_stop("VECM `rank` must be between 1 and number_of_variables - 1 (here 1 to ", k - 1L, ").")
    rank <- as.integer(rank)
    jo <- run_engine(urca::ca.jo(prep$data[, variables, drop = FALSE], type = johansen_type, ecdet = ecdet, K = p, spec = spec))
    cr <- run_engine(urca::cajorls(jo, r = rank))
    .ec_validate_system_equation(cr$rlm, "VECM equations")
    .ec_validate_system_residuals(stats::residuals(cr$rlm), "VECM")
    vr <- run_engine(vars::vec2var(jo, r = rank))
    system_fit <- list(johansen = jo, cajorls = cr, vec2var = vr)
    class(system_fit) <- c("econcompare_vecm_system", "list")
    model_wrappers <- run_engine(.ec_extract_mlm_equations(cr$rlm, "vecm"))
    if (!length(model_wrappers)) .ec_stop("VECM was fitted but its equation results could not be extracted.")
    rank_used <- rank
    used_positions <- seq.int(p + 1L, n)
    det_note <- ecdet
  }

  # Make equation names unique and explicit without implying a ranking among equations.
  old_names <- names(model_wrappers)
  if (is.null(old_names) || any(!nzchar(old_names))) old_names <- paste0("equation_", seq_along(model_wrappers))
  names(model_wrappers) <- make.unique(paste0(toupper(model), ": ", old_names))
  meta <- lapply(seq_along(model_wrappers), function(i) list(
    engine = model, outcome_type = "system", family = if (model == "var") "Vector autoregression" else "Vector error-correction model",
    estimator = if (model == "var") "VAR equation" else "VECM differenced equation",
    comparison_note = "Equation-level coefficients from one jointly specified dynamic system. Do not interpret equations as competing standalone models.",
    analysis_type = "time_series_system", temporal_family = model, time_variable = time,
    p = p, rank = rank_used, deterministic = det_note
  ))
  names(meta) <- names(model_wrappers)
  sample_info <- do.call(rbind, lapply(names(model_wrappers), function(nm) .ec_exact_sample_info(nm, model, prep, used_positions)))
  sample_rows <- stats::setNames(rep(list(rownames(prep$data)[used_positions]), length(model_wrappers)), names(model_wrappers))

  md <- list(
    temporal_family = model, time_variable = time, time_kind = prep$kind, frequency = prep$audit$frequency,
    regular_spacing = isTRUE(prep$audit$regular_spacing), missing_periods = prep$audit$missing_periods,
    complete_regular_grid = TRUE, first_time = prep$audit$first_time, last_time = prep$audit$last_time,
    n_time = prep$audit$unique_time, reordered_for_estimation = prep$reordered,
    variables = variables, p = p, deterministic = det_note, rank = rank_used,
    johansen_type = if (model == "vecm") johansen_type else NULL,
    johansen_spec = if (model == "vecm") spec else NULL,
    index = data.frame(original_row = rownames(prep$data), time_display = prep$display, time_numeric = prep$parsed, stringsAsFactors = FALSE),
    estimation_start = prep$display[min(used_positions)], estimation_end = prep$display[max(used_positions)], n_time_used = length(used_positions),
    interpretation_note = if (model == "var")
      "VAR treats all selected variables as endogenous. Lag order and deterministic terms are researcher choices; no forecasting or structural identification is imposed. Review stationarity and cointegration evidence before interpreting a levels VAR as a stable dynamic system."
    else
      "VECM rank is supplied explicitly by the researcher. Johansen statistics inform but do not mechanically determine rank. Cointegrating vectors are normalization-dependent."
  )

  out <- list(
    call = match.call(), formula = NULL, models = model_wrappers, meta = meta,
    outcome_type = "system", analysis_type = "time_series_system", temporal_family = model,
    time_variable = time, variables = variables, time_audit = prep$audit, time_metadata = md,
    reordered_time = prep$reordered, p = p, rank = rank_used, deterministic = det_note,
    system_model = system_fit, johansen = jo,
    johansen_table = if (model == "vecm") .ec_johansen_table(jo, johansen_type, p, ecdet, spec) else NULL,
    warnings = warnings, failures = failures, sample_info = sample_info,
    sample_rows = sample_rows, sample_comparable = TRUE,
    type_overrides = attr(data, "econcompare_type_overrides"), ordinal_levels = attr(data, "econcompare_ordinal_levels"),
    data_n = nrow(data), created = Sys.time(), version = .ec_version()
  )
  out$provenance <- .ec_provenance(out)
  class(out) <- c("econcompare_system", "econcompare")
  out
}

#' Johansen rank-test table retained by a fitted VECM
#' @param x A VECM object returned by [eco_system_run()].
#' @return A data.frame of Johansen statistics and critical values.
#' @export
eco_vecm_rank_test <- function(x) {
  if (!inherits(x, "econcompare_system") || !identical(x$temporal_family, "vecm")) .ec_stop("`x` must be a VECM result returned by eco_system_run().")
  x$johansen_table
}

#' Essential diagnostics for VAR/VECM systems
#' @param x An object returned by [eco_system_run()].
#' @param serial_lags Portmanteau residual-autocorrelation lag order. It must exceed the fitted VAR lag order and be smaller than the residual sample size.
#' @return A list containing root information and a multivariate residual serial-correlation table, including degrees of freedom (`df`). Unavailable diagnostics have an explicit explanatory note.
#' @export
eco_system_diagnostics <- function(x, serial_lags = 8L) {
  if (!inherits(x, "econcompare_system") || !identical(x$analysis_type, "time_series_system")) {
    .ec_stop("`x` must be a VAR/VECM result returned by eco_system_run().")
  }
  if (!is.numeric(serial_lags) || length(serial_lags) != 1L || !is.finite(serial_lags) || serial_lags < 1L || serial_lags > .Machine$integer.max || abs(serial_lags - round(serial_lags)) > 1e-8) {
    .ec_stop("`serial_lags` must be one positive integer.")
  }
  serial_lags <- as.integer(serial_lags)
  if (!is.null(x$p) && is.finite(x$p) && serial_lags <= as.integer(x$p)) {
    .ec_stop(
      "`serial_lags` must be greater than the fitted VAR lag order p = ", as.integer(x$p),
      " for the asymptotic multivariate Portmanteau test. Choose a larger diagnostic horizon."
    )
  }
  family <- x$temporal_family
  raw <- if (family == "var") x$system_model else x$system_model$vec2var
  A <- if (family == "var") tryCatch(vars::Acoef(raw), error = function(e) list()) else raw$A
  if (!is.null(raw$obs) && serial_lags >= raw$obs) {
    .ec_stop("`serial_lags` must be smaller than the number of residual observations (", raw$obs, ").")
  }
  root_error <- NULL
  roots <- tryCatch(.ec_companion_roots(A), error = function(e) {
    root_error <<- conditionMessage(e)
    numeric()
  })
  roots_df <- data.frame(root = seq_along(roots), modulus = Mod(roots), stringsAsFactors = FALSE)
  root_note <- if (family == "var") {
    if (!length(roots)) {
      "Companion-root information is unavailable for this fitted VAR."
    } else if (all(Mod(roots) < 1)) {
      "All companion roots are inside the unit circle: the fitted VAR satisfies the usual stability condition."
    } else {
      "At least one companion root is on or outside the unit circle; review VAR stability before dynamic interpretation."
    }
  } else {
    "A cointegrated VECM intentionally permits common stochastic trends, so unit roots in the levels companion representation are not interpreted with the stationary-VAR rule."
  }

  if (!length(roots)) root_note <- "Companion-root information is unavailable; stability has not been assessed."
  if (!is.null(root_error)) root_note <- paste("Unavailable:", root_error)

  serial <- NULL
  if (!requireNamespace("vars", quietly = TRUE)) {
    serial <- data.frame(test = "multivariate Portmanteau", statistic = NA_real_, p.value = NA_real_, df = NA_real_, note = "Unavailable: install `vars`.", stringsAsFactors = FALSE)
  } else {
    st <- tryCatch(vars::serial.test(raw, lags.pt = serial_lags, type = "PT.asymptotic"), error = function(e) e)
    if (inherits(st, "error")) {
      serial <- data.frame(test = "multivariate Portmanteau", statistic = NA_real_, p.value = NA_real_, df = NA_real_, note = paste("Unavailable:", conditionMessage(st)), stringsAsFactors = FALSE)
    } else {
      h <- st$serial
      valid <- length(h$p.value) == 1L && is.finite(h$p.value) &&
        length(h$statistic) == 1L && is.finite(h$statistic)
      serial <- data.frame(test = paste0("multivariate Portmanteau (lags ", serial_lags, ")"), statistic = unname(as.numeric(h$statistic)[1L]), p.value = h$p.value, df = unname(as.numeric(h$parameter)[1L]), note = if (!valid) "Unavailable: non-finite Portmanteau result; review system rank and residual covariance." else if (h$p.value < .05) "Evidence of residual serial correlation at the 5% level." else "No evidence of residual serial correlation at the 5% level.", stringsAsFactors = FALSE)
    }
  }
  list(roots = roots_df, root_note = root_note, serial_correlation = serial)
}

#' Cointegrating vectors retained in a fitted VECM
#' @param x A VECM object returned by [eco_system_run()].
#' @return A matrix of normalized cointegrating vectors as returned by `urca::cajorls`.
#' @export
eco_vecm_cointegration <- function(x) {
  if (!inherits(x, "econcompare_system") || !identical(x$temporal_family, "vecm")) .ec_stop("`x` must be a VECM result returned by eco_system_run().")
  beta <- x$system_model$cajorls$beta
  attr(beta, "note") <- "Cointegrating vectors are normalization-dependent. Their signs and scale are not unique; interpret the normalized relations rather than treating normalization as an economic restriction."
  beta
}
