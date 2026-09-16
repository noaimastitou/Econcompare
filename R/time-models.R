.ec_time_registry <- function() {
  data.frame(
    engine = c("time_static", "distributed_lag", "dynamic_regression", "ardl"),
    estimator = c("Static time regression", "Distributed-lag regression", "Dynamic regression", "ARDL regression"),
    family = c("Temporal regression", "Distributed lag", "Dynamic regression", "ARDL"),
    package = "stats",
    comparison_note = c(
      "Contemporaneous relationship Y_t on X_t. Time ordering is explicit; no lag is included.",
      "Relates Y_t to current and lagged X. A complete regular time grid is required so L1 means exactly one period.",
      "Relates Y_t to its own lags and contemporaneous X. A complete regular time grid is required.",
      "Relates Y_t to lags of Y and current/lagged X. Variable-specific explanatory lag orders can be supplied explicitly; Bounds/ECM inference is not automatic."
    ),
    stringsAsFactors = FALSE
  )
}

#' List time-series econometric model engines
#' @return A data.frame describing supported temporal engines.
#' @export
eco_time_models <- function() {
  z <- .ec_time_registry()
  z$available <- TRUE
  z
}

.ec_lag_vec <- function(x, k) {
  k <- as.integer(k)
  if (k == 0L) return(x)
  if (k < 0L) .ec_stop("Lag orders must be non-negative integers.")
  c(rep(NA, k), head(x, -k))
}

.ec_time_formula_parts <- function(formula, data) {
  tt <- stats::terms(formula, data = data)
  if (attr(tt, "response") != 1L) .ec_stop("Time-series models require a formula with one dependent variable.")
  y <- all.vars(formula[[2L]])
  if (length(y) != 1L || !y %in% names(data)) .ec_stop("The time-series dependent variable must be one data column.")
  x <- attr(tt, "term.labels")
  vars <- all.vars(stats::delete.response(tt))
  if (!length(vars)) .ec_stop("Choose at least one explanatory variable for time-series econometrics.")
  if (!all(x %in% names(data)) || !setequal(x, vars)) {
    .ec_stop("Time-series lag construction requires simple column names on the right-hand side. Create transformations explicitly in the data first.")
  }
  if (!is.numeric(data[[y]])) .ec_stop("Time-series regression requires a numeric dependent variable. Use an explicit type override if the variable is numerically stored as text.")
  nonnum <- x[!vapply(data[x], is.numeric, logical(1))]
  if (length(nonnum)) .ec_stop("Time-series regressors must be numeric. Non-numeric regressor(s): ", paste(nonnum, collapse = ", "), ".")
  list(y = y, x = x)
}

.ec_validate_lag_order <- function(k, name, max_allowed) {
  if (length(k) != 1L || !(is.numeric(k) || is.integer(k)) || is.na(k) || !is.finite(k) || abs(k - round(k)) > 1e-8 || k < 0) {
    .ec_stop("`", name, "` must be one non-negative integer.")
  }
  k <- as.integer(k)
  if (k > max_allowed) .ec_stop("`", name, "` = ", k, " is too large for this dataset. Maximum allowed here is ", max_allowed, ".")
  k
}

.ec_normalize_q_by_var <- function(xvars, q, q_by_var, max_allowed) {
  q <- .ec_validate_lag_order(q, "q", max_allowed)
  out <- stats::setNames(rep(q, length(xvars)), xvars)
  if (is.null(q_by_var) || !length(q_by_var)) return(out)
  if (is.list(q_by_var)) q_by_var <- unlist(q_by_var, use.names = TRUE)
  if (is.null(names(q_by_var)) || any(!nzchar(names(q_by_var)))) {
    .ec_stop("`q_by_var` must be a named numeric vector or named list, with explanatory-variable names.")
  }
  if (anyDuplicated(names(q_by_var))) .ec_stop("`q_by_var` must not contain duplicated variable names.")
  unknown <- setdiff(names(q_by_var), xvars)
  if (length(unknown)) .ec_stop("Unknown variable(s) in `q_by_var`: ", paste(unknown, collapse = ", "), ".")
  for (nm in names(q_by_var)) out[[nm]] <- .ec_validate_lag_order(q_by_var[[nm]], paste0("q_by_var[['", nm, "']]"), max_allowed)
  out
}

.ec_require_complete_lag_grid <- function(prep, engine) {
  if (!engine %in% c("distributed_lag", "dynamic_regression", "ardl")) return(invisible(TRUE))
  a <- prep$audit
  if (!isTRUE(a$regular_spacing) || is.na(a$missing_periods) || a$missing_periods > 0L) {
    gap_text <- if (is.na(a$missing_periods)) "an irregular/custom calendar grid" else paste0(a$missing_periods, " missing period(s)")
    .ec_stop(
      "Lagged model `", engine, "` requires a complete regular time grid so L1 means exactly one period. The selected time index has ",
      gap_text, ". econcompare will not reinterpret the previous observed row as the previous calendar period."
    )
  }
  invisible(TRUE)
}

.ec_require_regular_time_grid <- function(audit, feature) {
  if (!isTRUE(audit$regular_spacing) || is.na(audit$missing_periods) || audit$missing_periods > 0L) {
    .ec_stop(
      feature, " requires a complete regular time grid because its lag distances are defined in observation periods. ",
      "The selected time index is irregular or contains missing periods. econcompare will not treat unequal calendar gaps as equal lags."
    )
  }
  invisible(TRUE)
}

.ec_model_parameter_count <- function(k, p, q_by_var, engine) {
  if (engine == "time_static") return(1L + k)
  if (engine == "distributed_lag") return(1L + sum(q_by_var + 1L))
  if (engine == "dynamic_regression") return(1L + p + k)
  if (engine == "ardl") return(1L + p + sum(q_by_var + 1L))
  1L + k
}

.ec_build_time_model_data <- function(data, time, formula, p = 0L, q = 0L, q_by_var = NULL,
                                      include_x0 = TRUE, engine = "time_static") {
  prep <- .ec_prepare_time_data(data, time)
  .ec_require_complete_lag_grid(prep, engine)
  parts <- .ec_time_formula_parts(formula, prep$data)
  n <- nrow(prep$data)
  max_allowed <- max(0L, min(20L, floor((n - 3L) / 4L)))
  p <- .ec_validate_lag_order(p, "p", max_allowed)
  q_spec <- .ec_normalize_q_by_var(parts$x, q, q_by_var, max_allowed)

  d <- data.frame(.ec_y = prep$data[[parts$y]], check.names = FALSE)
  names(d)[1L] <- parts$y
  if (p > 0L) {
    for (kk in seq_len(p)) d[[paste0(parts$y, "_L", kk)]] <- .ec_lag_vec(prep$data[[parts$y]], kk)
  }

  xterms <- character()
  for (v in parts$x) {
    if (include_x0) {
      d[[v]] <- prep$data[[v]]
      xterms <- c(xterms, v)
    }
    qv <- q_spec[[v]]
    if (qv > 0L) {
      for (kk in seq_len(qv)) {
        nm <- paste0(v, "_L", kk)
        d[[nm]] <- .ec_lag_vec(prep$data[[v]], kk)
        xterms <- c(xterms, nm)
      }
    }
  }

  rhs <- c(if (p > 0L) paste0(parts$y, "_L", seq_len(p)) else character(), xterms)
  f <- stats::reformulate(rhs, response = parts$y)
  rownames(d) <- rownames(prep$data)
  cc <- stats::complete.cases(d)
  n_eff <- sum(cc)
  params <- .ec_model_parameter_count(length(parts$x), p, q_spec, engine)
  if (n_eff <= params + 5L) {
    .ec_stop(
      "Model `", engine, "` is over-parameterized for the usable sample: ", n_eff,
      " complete observations for about ", params, " estimated coefficients. Reduce p/q, variable-specific lags, or the number of regressors."
    )
  }
  if (n_eff > 0L) {
    mm <- stats::model.matrix(f, d[cc, , drop = FALSE])
    rk <- qr(mm)$rank
    if (rk < ncol(mm)) {
      .ec_stop("Model `", engine, "` has a rank-deficient design matrix after lagging/missing-value handling. Remove collinear/constant regressors or reduce the lag structure.")
    }
  }
  list(
    data = d, formula = f, prep = prep, parts = parts, p = p,
    q = if (length(unique(q_spec)) == 1L) unname(q_spec[1L]) else NA_integer_,
    q_by_var = q_spec, n_effective = n_eff, parameter_count = params
  )
}

.ec_model_sample_is_contiguous <- function(model, prep) {
  mf <- tryCatch(stats::model.frame(model), error = function(e) NULL)
  if (is.null(mf)) return(FALSE)
  used <- rownames(mf)
  pos <- match(used, rownames(prep$data))
  if (anyNA(pos)) return(FALSE)
  length(pos) < 2L || all(diff(pos) == 1L)
}

.ec_sample_rows_are_contiguous <- function(rows, index_rows) {
  pos <- match(rows, index_rows)
  if (anyNA(pos)) return(FALSE)
  length(pos) < 2L || all(diff(pos) == 1L)
}

.ec_attach_time_inference <- function(model, inference, hac_lag = NULL) {
  inference <- match.arg(inference, c("classical", "HAC"))
  attr(model, "econcompare_time_inference") <- inference
  if (inference == "HAC") {
    if (!requireNamespace("sandwich", quietly = TRUE) || !requireNamespace("lmtest", quietly = TRUE)) {
      .ec_stop("HAC/Newey-West inference requires optional packages `sandwich` and `lmtest`.")
    }
    if (is.null(hac_lag)) {
      n <- stats::nobs(model)
      hac_lag <- floor(4 * (n / 100)^(2 / 9))
    }
    if (length(hac_lag) != 1L || !is.finite(hac_lag) || hac_lag < 0 || abs(hac_lag - round(hac_lag)) > 1e-8) {
      .ec_stop("`hac_lag` must be NULL or one non-negative integer.")
    }
    hac_lag <- as.integer(hac_lag)
    vc <- sandwich::NeweyWest(model, lag = hac_lag, prewhite = FALSE, adjust = TRUE)
    ct <- lmtest::coeftest(model, vcov. = vc)
    attr(model, "econcompare_time_coeftest") <- ct
    attr(model, "econcompare_hac_lag") <- hac_lag
  }
  model
}

.ec_time_periods_used <- function(model, prep) {
  mf <- tryCatch(stats::model.frame(model), error = function(e) NULL)
  if (is.null(mf)) return(character())
  ids <- rownames(mf)
  pos <- match(ids, rownames(prep$data))
  prep$display[pos[!is.na(pos)]]
}

.ec_time_metadata <- function(prep, time, formula, meta, inference, hac_lag, p, q, q_by_var) {
  list(
    time_variable = time,
    time_kind = prep$kind,
    frequency = prep$audit$frequency,
    regular_spacing = isTRUE(prep$audit$regular_spacing),
    missing_periods = prep$audit$missing_periods,
    complete_regular_grid = isTRUE(prep$audit$regular_spacing) && !is.na(prep$audit$missing_periods) && prep$audit$missing_periods == 0L,
    first_time = prep$audit$first_time,
    last_time = prep$audit$last_time,
    n_time = prep$audit$unique_time,
    reordered_for_estimation = prep$reordered,
    index = data.frame(
      original_row = rownames(prep$data),
      time_display = prep$display,
      time_numeric = prep$parsed,
      stringsAsFactors = FALSE
    ),
    formula = formula,
    p = p,
    q = q,
    q_by_var = q_by_var,
    inference = inference,
    hac_lag = if (length(meta)) stats::setNames(vapply(meta, function(m) if (is.null(m$hac_lag)) NA_integer_ else as.integer(m$hac_lag), integer(1)), names(meta)) else integer(),
    model_specs = lapply(meta, function(m) {
      list(
        engine = m$engine,
        p = m$p,
        q = m$q,
        q_by_var = m$q_by_var,
        estimation_start = m$estimation_start,
        estimation_end = m$estimation_end,
        n_time_used = m$n_time_used
      )
    })
  )
}

#' Estimate explanatory time-series econometric regressions
#'
#' @param data A data.frame.
#' @param formula Formula such as `y ~ x1 + x2` using simple numeric columns.
#' @param time Name of the time-index column.
#' @param models Character vector from [eco_time_models()].
#' @param p Lags of the dependent variable for dynamic/ARDL models.
#' @param q Default/common lag order applied to explanatory variables in distributed-lag/ARDL models.
#' @param q_by_var Optional named integer vector/list overriding `q` for selected explanatory variables.
#'   Example: `c(education_spending = 8, unemployment = 2)`.
#' @param inference `"classical"` or `"HAC"` (Newey-West coefficient inference).
#' @param hac_lag Optional Newey-West truncation lag. NULL uses a standard sample-size rule.
#' @param error_policy Either `"stop"` or `"collect"`.
#' @return An `econcompare` object with `analysis_type = "time_series"` and structured temporal metadata.
#' @export
eco_time_run <- function(data, formula, time, models = "time_static", p = 1L, q = 1L,
                         q_by_var = NULL, inference = c("classical", "HAC"), hac_lag = NULL,
                         error_policy = c("stop", "collect")) {
  if (!is.data.frame(data)) .ec_stop("`data` must be a data.frame.")
  .ec_validate_data_columns(data)
  if (!inherits(formula, "formula")) .ec_stop("`formula` must be a formula.")
  if (length(time) != 1L || !is.character(time) || !time %in% names(data)) .ec_stop("`time` must name one column in `data`.")

  inference <- match.arg(inference)
  error_policy <- match.arg(error_policy)
  reg <- .ec_time_registry()
  models <- unique(as.character(models))
  bad <- setdiff(models, reg$engine)
  if (length(bad)) .ec_stop("Unsupported time-series model(s): ", paste(bad, collapse = ", "), ".")
  if (!length(models)) .ec_stop("Choose at least one time-series econometric model.")

  prep0 <- .ec_prepare_time_data(data, time)
  parts0 <- .ec_time_formula_parts(formula, prep0$data)
  if (time %in% c(parts0$y, parts0$x)) .ec_stop("The time-index variable cannot also be the dependent variable or an explanatory regressor.")
  if (identical(inference, "HAC")) .ec_require_regular_time_grid(prep0$audit, "HAC/Newey-West inference")

  n <- nrow(data)
  max_allowed <- max(0L, min(20L, floor((n - 3L) / 4L)))
  p <- .ec_validate_lag_order(p, "p", max_allowed)
  q <- .ec_validate_lag_order(q, "q", max_allowed)
  q_spec_requested <- .ec_normalize_q_by_var(parts0$x, q, q_by_var, max_allowed)
  q_spec <- if (any(models %in% c("distributed_lag", "ardl"))) q_spec_requested else stats::setNames(rep(0L, length(parts0$x)), parts0$x)
  if (any(models %in% c("dynamic_regression", "ardl")) && p < 1L) .ec_stop("Dynamic regression and ARDL require `p >= 1`.")
  if (any(models %in% c("distributed_lag", "ardl")) && max(q_spec) < 1L) .ec_stop("Distributed-lag regression and ARDL require at least one explanatory lag >= 1.")

  fits <- list(); meta <- list(); failures <- list(); warnings <- list(); sample_info <- list(); sample_rows <- list()

  run_one <- function(name, engine, md) {
    captured <- character()
    ans <- tryCatch(
      withCallingHandlers(
        stats::lm(md$formula, data = md$data, na.action = stats::na.omit),
        warning = function(w) { captured <<- c(captured, conditionMessage(w)); invokeRestart("muffleWarning") }
      ),
      error = function(e) e
    )
    warnings[[name]] <<- unique(captured)
    if (inherits(ans, "error")) {
      if (error_policy == "stop") .ec_stop("Model `", name, "` failed: ", conditionMessage(ans))
      failures[[name]] <<- data.frame(model = name, engine = engine, message = conditionMessage(ans), stringsAsFactors = FALSE)
      return()
    }
    if (ans$rank < length(stats::coef(ans)) || stats::df.residual(ans) < 5L) {
      msg <- "Model is rank-deficient or leaves fewer than five residual degrees of freedom."
      if (error_policy == "stop") .ec_stop("Model `", name, "` failed: ", msg)
      failures[[name]] <<- data.frame(model = name, engine = engine, message = msg, stringsAsFactors = FALSE)
      return()
    }
    if (identical(inference, "HAC") && !.ec_model_sample_is_contiguous(ans, md$prep)) {
      msg <- "HAC/Newey-West inference is not run because missing model values create gaps inside the estimation sample; adjacent residual rows would not represent adjacent calendar periods."
      if (error_policy == "stop") .ec_stop("Model `", name, "` failed: ", msg)
      failures[[name]] <<- data.frame(model = name, engine = engine, message = msg, stringsAsFactors = FALSE)
      return()
    }
    ans2 <- tryCatch(.ec_attach_time_inference(ans, inference, hac_lag), error = function(e) e)
    if (inherits(ans2, "error")) {
      if (error_policy == "stop") stop(ans2)
      failures[[name]] <<- data.frame(model = name, engine = engine, message = conditionMessage(ans2), stringsAsFactors = FALSE)
      return()
    }

    fits[[name]] <<- ans2
    rr <- reg[reg$engine == engine, , drop = FALSE]
    periods <- .ec_time_periods_used(ans2, md$prep)
    q_common <- if (length(unique(md$q_by_var)) == 1L) unname(md$q_by_var[1L]) else NA_integer_
    meta[[name]] <<- list(
      engine = engine, outcome_type = "continuous", family = rr$family[1L], estimator = rr$estimator[1L],
      comparison_note = rr$comparison_note[1L], analysis_type = "time_series", time_variable = time,
      p = md$p, q = q_common, q_by_var = md$q_by_var, inference = inference,
      hac_lag = attr(ans2, "econcompare_hac_lag"),
      estimation_start = if (length(periods)) periods[1L] else NA_character_,
      estimation_end = if (length(periods)) tail(periods, 1L) else NA_character_,
      n_time_used = length(periods)
    )
    sample_info[[name]] <<- .ec_model_sample_info(ans2, rownames(md$data), name, engine)
    mf_used <- tryCatch(stats::model.frame(ans2), error = function(e) NULL)
    sample_rows[[name]] <<- if (is.null(mf_used)) character() else rownames(mf_used)
  }

  if ("time_static" %in% models) {
    run_one("time_static", "time_static", .ec_build_time_model_data(data, time, formula, p = 0, q = 0, q_by_var = stats::setNames(rep(0L, length(parts0$x)), parts0$x), engine = "time_static"))
  }
  if ("distributed_lag" %in% models) {
    nm <- if (length(unique(q_spec)) == 1L) paste0("distributed_lag_q", q_spec[1L]) else "distributed_lag_qvar"
    run_one(nm, "distributed_lag", .ec_build_time_model_data(data, time, formula, p = 0, q = q, q_by_var = q_spec, engine = "distributed_lag"))
  }
  if ("dynamic_regression" %in% models) {
    run_one(paste0("dynamic_regression_p", p), "dynamic_regression", .ec_build_time_model_data(data, time, formula, p = p, q = 0, q_by_var = stats::setNames(rep(0L, length(parts0$x)), parts0$x), engine = "dynamic_regression"))
  }
  if ("ardl" %in% models) {
    nm <- if (length(unique(q_spec)) == 1L) paste0("ardl_p", p, "_q", q_spec[1L]) else paste0("ardl_p", p, "_qvar")
    run_one(nm, "ardl", .ec_build_time_model_data(data, time, formula, p = p, q = q, q_by_var = q_spec, engine = "ardl"))
  }
  if (!length(fits)) .ec_stop("No time-series model could be estimated successfully.")

  failure_df <- if (length(failures)) do.call(rbind, failures) else data.frame(model = character(), engine = character(), message = character(), stringsAsFactors = FALSE)
  warning_df <- do.call(rbind, lapply(names(warnings), function(nm) {
    z <- warnings[[nm]]
    if (!length(z)) return(NULL)
    eng <- if (!is.null(meta[[nm]])) meta[[nm]]$engine else nm
    data.frame(model = nm, engine = eng, message = z, stringsAsFactors = FALSE)
  }))
  if (is.null(warning_df)) warning_df <- data.frame(model = character(), engine = character(), message = character(), stringsAsFactors = FALSE)
  sample_df <- if (length(sample_info)) do.call(rbind, sample_info) else data.frame()
  sample_comparable <- if (length(sample_rows) <= 1L) TRUE else all(vapply(sample_rows[-1L], function(z) identical(z, sample_rows[[1L]]), logical(1)))
  q_common <- if (length(unique(q_spec)) == 1L) unname(q_spec[1L]) else NA_integer_
  md_time <- .ec_time_metadata(prep0, time, formula, meta, inference, hac_lag, p, q_common, q_spec)

  out <- list(
    call = match.call(), formula = formula, models = fits, meta = meta,
    outcome_type = "continuous", analysis_type = "time_series", time_variable = time,
    time_audit = prep0$audit, time_metadata = md_time, reordered_time = prep0$reordered,
    p = p, q = q_common, q_by_var = q_spec, inference = inference,
    warnings = warning_df, failures = failure_df, sample_info = sample_df,
    sample_rows = sample_rows, sample_comparable = sample_comparable,
    type_overrides = attr(data, "econcompare_type_overrides"),
    ordinal_levels = attr(data, "econcompare_ordinal_levels"),
    data_n = nrow(data), created = Sys.time(), version = .ec_version()
  )
  class(out) <- "econcompare"
  out
}

#' Essential diagnostics for explanatory time-series regressions
#' @param x An `econcompare` time-series result.
#' @param bg_order Lag order for Breusch-Godfrey.
#' @return A data.frame.
#' @export
eco_time_diagnostics <- function(x, bg_order = 1L) {
  if (!inherits(x, "econcompare") || !identical(x$analysis_type, "time_series")) {
    .ec_stop("`x` must be a time-series econcompare object returned by eco_time_run().")
  }
  .ec_require_regular_time_grid(x$time_audit, "Breusch-Godfrey residual serial-correlation testing")
  if (length(bg_order) != 1L || !is.finite(bg_order) || bg_order < 1 || abs(bg_order - round(bg_order)) > 1e-8) {
    .ec_stop("`bg_order` must be a positive integer.")
  }
  bg_order <- as.integer(bg_order)
  rows <- lapply(names(x$models), function(nm) {
    m <- x$models[[nm]]
    e <- .ec_meta_value(x, nm, "engine", nm)
    index_rows <- if (is.list(x$time_metadata) && is.data.frame(x$time_metadata$index)) x$time_metadata$index$original_row else character()
    rows_used <- if (is.list(x$sample_rows)) x$sample_rows[[nm]] else character()
    if (!length(index_rows) || !length(rows_used) || !.ec_sample_rows_are_contiguous(rows_used, index_rows)) {
      .ec_stop("Breusch-Godfrey is not run for model `", nm, "` because its estimation sample contains internal calendar gaps after missing-value handling.")
    }
    if (!requireNamespace("lmtest", quietly = TRUE)) {
      return(data.frame(model = nm, engine = e, test = "Breusch-Godfrey", statistic = NA_real_, p.value = NA_real_, interpretation = "Unavailable: install `lmtest`.", stringsAsFactors = FALSE))
    }
    ans <- tryCatch(lmtest::bgtest(m, order = bg_order), error = function(err) err)
    if (inherits(ans, "error")) {
      return(data.frame(model = nm, engine = e, test = "Breusch-Godfrey", statistic = NA_real_, p.value = NA_real_, interpretation = paste("Unavailable:", conditionMessage(ans)), stringsAsFactors = FALSE))
    }
    data.frame(
      model = nm, engine = e, test = paste0("Breusch-Godfrey (order ", bg_order, ")"),
      statistic = unname(ans$statistic[1L]), p.value = ans$p.value,
      interpretation = if (ans$p.value < .05) "Evidence of residual serial correlation at the 5% level." else "No evidence of residual serial correlation at the 5% level.",
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

.ec_adf_urca <- function(z, deterministic, k) {
  if (!requireNamespace("urca", quietly = TRUE)) .ec_stop("ADF tests with explicit deterministic components require optional package `urca`.")
  fit <- urca::ur.df(z, type = deterministic, lags = k, selectlags = "Fixed")
  teststat <- methods::slot(fit, "teststat")
  cval <- methods::slot(fit, "cval")
  stat <- as.numeric(teststat)[1L]
  cv <- as.numeric(cval[1L, ])
  names(cv) <- colnames(cval)
  getcv <- function(nm) if (nm %in% names(cv)) unname(cv[[nm]]) else NA_real_
  list(statistic = stat, critical_1pct = getcv("1pct"), critical_5pct = getcv("5pct"), critical_10pct = getcv("10pct"))
}

#' Run complementary stationarity diagnostics
#'
#' @param data A data.frame.
#' @param variables Numeric variable names. Only the explicitly supplied series are tested.
#' @param time Validated time index.
#' @param adf_k Optional fixed ADF lag order. NULL uses `floor((n - 1)^(1/3))` for each series.
#' @param adf_deterministic Deterministic component for ADF: `"none"`, `"drift"`, or `"trend"`.
#' @param kpss_null `"Level"` or `"Trend"`.
#' @return A data.frame with ADF and KPSS results. ADF uses `urca::ur.df`; its critical values are reported explicitly and p-values are left NA rather than approximated silently.
#' @export
eco_stationarity_tests <- function(data, variables, time, adf_k = NULL,
                                   adf_deterministic = c("drift", "none", "trend"),
                                   kpss_null = c("Level", "Trend")) {
  prep <- .ec_prepare_time_data(data, time)
  .ec_require_regular_time_grid(prep$audit, "ADF/KPSS stationarity diagnostics")
  adf_deterministic <- match.arg(adf_deterministic)
  kpss_null <- match.arg(kpss_null)
  if (!is.null(adf_k) && (length(adf_k) != 1L || !(is.numeric(adf_k) || is.integer(adf_k)) || !is.finite(adf_k) || adf_k < 0 || abs(adf_k - round(adf_k)) > 1e-8)) {
    .ec_stop("`adf_k` must be NULL or one non-negative integer.")
  }

  variables <- unique(as.character(variables))
  if (!length(variables)) .ec_stop("Choose at least one numeric series for stationarity diagnostics.")
  bad <- setdiff(variables, names(prep$data))
  if (length(bad)) .ec_stop("Unknown variable(s): ", paste(bad, collapse = ", "), ".")
  nonnum <- variables[!vapply(prep$data[variables], is.numeric, logical(1))]
  if (length(nonnum)) .ec_stop("Stationarity tests require numeric series: ", paste(nonnum, collapse = ", "), ".")

  rows <- list(); kk <- 0L
  for (v in variables) {
    z <- prep$data[[v]]
    if (any(!is.finite(z))) {
      .ec_stop("Stationarity test for `", v, "` is not run because the series contains missing/non-finite internal observations. econcompare does not collapse the time grid by deleting them.")
    }
    if (length(z) < 10L || length(unique(z)) < 3L) {
      kk <- kk + 1L
      rows[[kk]] <- data.frame(variable = v, test = "ADF", deterministic = adf_deterministic, null_hypothesis = "unit root", statistic = NA_real_, p.value = NA_real_, critical_1pct = NA_real_, critical_5pct = NA_real_, critical_10pct = NA_real_, note = "Unavailable: too few usable or varying observations.", stringsAsFactors = FALSE)
      kk <- kk + 1L
      rows[[kk]] <- data.frame(variable = v, test = "KPSS", deterministic = tolower(kpss_null), null_hypothesis = paste(tolower(kpss_null), "stationarity"), statistic = NA_real_, p.value = NA_real_, critical_1pct = NA_real_, critical_5pct = NA_real_, critical_10pct = NA_real_, note = "Unavailable: too few usable or varying observations.", stringsAsFactors = FALSE)
      next
    }

    k_use <- if (is.null(adf_k)) floor((length(z) - 1)^(1 / 3)) else as.integer(adf_k)
    max_k <- max(0L, floor((length(z) - 5L) / 3L))
    if (k_use > max_k) .ec_stop("ADF lag order is too large for series `", v, "`. Maximum allowed here is ", max_k, ".")
    adf <- tryCatch(.ec_adf_urca(z, adf_deterministic, k_use), error = function(e) e)
    kk <- kk + 1L
    if (inherits(adf, "error")) {
      rows[[kk]] <- data.frame(variable = v, test = "ADF", deterministic = adf_deterministic, null_hypothesis = "unit root", statistic = NA_real_, p.value = NA_real_, critical_1pct = NA_real_, critical_5pct = NA_real_, critical_10pct = NA_real_, note = conditionMessage(adf), stringsAsFactors = FALSE)
    } else {
      decision <- if (is.finite(adf$critical_5pct)) {
        if (adf$statistic < adf$critical_5pct) "Statistic is beyond the 5% critical value: evidence against the unit-root null under this deterministic specification." else "Statistic does not cross the 5% critical value under this deterministic specification."
      } else "Read the reported ADF statistic against the available critical values."
      rows[[kk]] <- data.frame(variable = v, test = "ADF", deterministic = adf_deterministic, null_hypothesis = "unit root", statistic = adf$statistic, p.value = NA_real_, critical_1pct = adf$critical_1pct, critical_5pct = adf$critical_5pct, critical_10pct = adf$critical_10pct, note = paste0("Fixed ADF lag k=", k_use, ". ", decision, " econcompare does not transform the series automatically."), stringsAsFactors = FALSE)
    }

    if (!requireNamespace("tseries", quietly = TRUE)) {
      kp <- simpleError("KPSS test unavailable: install optional package `tseries`.")
    } else {
      kp <- tryCatch(suppressWarnings(tseries::kpss.test(z, null = kpss_null)), error = function(e) e)
    }
    kk <- kk + 1L
    rows[[kk]] <- if (inherits(kp, "error")) {
      data.frame(variable = v, test = "KPSS", deterministic = tolower(kpss_null), null_hypothesis = paste(tolower(kpss_null), "stationarity"), statistic = NA_real_, p.value = NA_real_, critical_1pct = NA_real_, critical_5pct = NA_real_, critical_10pct = NA_real_, note = conditionMessage(kp), stringsAsFactors = FALSE)
    } else {
      data.frame(variable = v, test = "KPSS", deterministic = tolower(kpss_null), null_hypothesis = paste(tolower(kpss_null), "stationarity"), statistic = unname(kp$statistic[1L]), p.value = kp$p.value, critical_1pct = NA_real_, critical_5pct = NA_real_, critical_10pct = NA_real_, note = "Read jointly with ADF, deterministic components, plots and substantive knowledge.", stringsAsFactors = FALSE)
    }
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
