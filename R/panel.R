# Static panel API. Raw plm/lm objects and explicit source-row maps are retained.
# No automatic estimator selection, differencing, imputation or causal claim.

.ec_panel_index <- function(data, id, time) {
  if (!is.data.frame(data) || !nrow(data)) .ec_stop("`data` must be a non-empty data.frame.")
  .ec_validate_data_columns(data)
  if (!is.character(id) || length(id) != 1L || is.na(id) || !id %in% names(data) ||
      !is.character(time) || length(time) != 1L || is.na(time) || !time %in% names(data) || identical(id, time)) {
    .ec_stop("Choose distinct existing columns for `id` and `time`.")
  }
  ids <- data[[id]]
  if (!(is.character(ids) || is.factor(ids) || is.numeric(ids)) || is.matrix(ids)) .ec_stop("Panel identifiers must be character, factor or numeric values.")
  if (is.numeric(ids) && any(!is.na(ids) & !is.finite(ids))) .ec_stop("Numeric individual identifiers must be finite.")
  id_missing <- is.na(ids)
  ids <- as.character(ids)
  missing_id <- id_missing | is.na(ids) | !nzchar(trimws(ids))
  t <- data[[time]]
  if (all(is.na(t))) {
    ti <- rep(NA_real_, length(t)); frequency <- "unknown"; regular <- FALSE
  } else if (is.numeric(t) && !inherits(t, c("Date", "POSIXt"))) {
    if (any(!is.na(t) & (!is.finite(t) | abs(t - round(t)) > 1e-8))) .ec_stop("Numeric panel time must contain finite integer period labels.")
    ti <- as.numeric(t); frequency <- "integer periods"; regular <- TRUE
  } else {
    parsed <- .ec_parse_time_vector(t, time)
    if (identical(parsed$kind, "unknown")) .ec_stop("Panel time must be integer periods, Date, ISO dates, year-month or year-quarter labels.")
    ti <- parsed$parsed; frequency <- parsed$kind; regular <- TRUE
    if (parsed$kind %in% c("date", "datetime")) {
      # Do not treat irregular dates as adjacent periods for serial tests.
      u <- sort(unique(parsed$dates[!is.na(parsed$dates)]))
      grid <- .ec_date_grid(u)
      frequency <- grid$frequency
      regular <- frequency %in% c("annual", "quarterly", "monthly", "weekly", "daily") && length(grid$grid) == length(u)
      ti <- if (regular) grid$grid[match(parsed$dates, u)] else match(parsed$parsed, sort(unique(parsed$parsed)))
      if (parsed$kind == "datetime") { ti <- parsed$parsed; regular <- FALSE }
    }
  }
  missing_time <- is.na(t) | is.na(ti) | !is.finite(ti)
  # Codes, rather than concatenated user strings, avoid key collisions.
  code <- match(ids, unique(ids))
  key <- paste(code, ti, sep = ":")
  good <- !missing_id & !missing_time
  duplicates <- good & (duplicated(key) | duplicated(key, fromLast = TRUE))
  list(id = ids, time = ti, key = key, missing_id = missing_id,
       missing_time = missing_time, duplicates = duplicates,
       frequency = frequency, regular = regular)
}

#' Audit explicitly selected panel indexes
#' @param data A data.frame.
#' @param id Individual identifier column.
#' @param time Period column (integer, Date or unambiguous calendar labels).
#' @return A list with summary, by_individual, issues and index tables. No rows are changed.
#' @export
eco_panel_audit <- function(data, id, time) {
  z <- .ec_panel_index(data, id, time)
  good <- !z$missing_id & !z$missing_time
  groups <- split(which(good), z$id[good])
  per <- lapply(names(groups), function(g) {
    rows <- groups[[g]]; tt <- sort(unique(z$time[rows]))
    data.frame(id = g, observations = length(rows), periods = length(tt),
               first_period = min(tt), last_period = max(tt),
               first_time = as.character(data[[time]][rows[which.min(z$time[rows])]]),
               last_time = as.character(data[[time]][rows[which.max(z$time[rows])]]),
               internal_missing_periods = if (z$regular) sum(pmax(diff(tt) - 1, 0)) else NA_real_,
               singleton = length(tt) == 1L, stringsAsFactors = FALSE)
  })
  per <- if (length(per)) do.call(rbind, per) else data.frame()
  schedules <- lapply(groups, function(rows) sort(unique(z$time[rows])))
  balanced <- length(schedules) > 0L && all(vapply(schedules, identical, logical(1), schedules[[1L]]))
  issue <- rep("", nrow(data))
  issue[z$missing_id] <- "missing individual identifier"
  issue[z$missing_time] <- paste(issue[z$missing_time], "missing/invalid time", sep = "; ")
  issue[z$duplicates] <- paste(issue[z$duplicates], "duplicate id-time pair", sep = "; ")
  list(summary = data.frame(observations = nrow(data), individuals = length(groups),
    periods = length(unique(z$time[good])), balanced = balanced,
    index_valid = !any(z$missing_id | z$missing_time | z$duplicates),
    duplicate_rows = sum(z$duplicates), missing_id = sum(z$missing_id), missing_time = sum(z$missing_time),
    singletons = if (nrow(per)) sum(per$singleton) else 0L,
    frequency = z$frequency, calendar_verified = z$regular, stringsAsFactors = FALSE),
    by_individual = per,
    issues = data.frame(row = which(nzchar(issue)), reason = issue[nzchar(issue)], stringsAsFactors = FALSE),
    index = data.frame(row = seq_len(nrow(data)), id = z$id, period = z$time, stringsAsFactors = FALSE))
}

#' Available static panel estimators
#' @return A data.frame describing engines and their interpretation.
#' @export
eco_panel_models <- function() {
  z <- data.frame(engine = c("panel_pooling", "panel_fe_individual", "panel_fe_time", "panel_fe_twoways", "panel_re", "panel_fd", "panel_between", "panel_mundlak"),
    estimator = c("Pooled OLS", "Individual fixed effects", "Time fixed effects", "Individual and time fixed effects", "Individual random effects", "First differences", "Between individuals", "Correlated random effects (Mundlak)"),
    model = c("pooling", "within", "within", "within", "random", "fd", "between", "random"),
    effect = c("individual", "individual", "time", "twoways", "individual", "individual", "individual", "individual"),
    interpretation = c("Pooled association; unobserved individual heterogeneity is not absorbed.",
      "Within-individual association; time-invariant regressors are absorbed.",
      "Association net of period effects; individual heterogeneity is not absorbed.",
      "Association net of individual and period effects; not automatically a causal DiD estimate.",
      "Random-effects GLS requires orthogonality between the individual effect and regressors.",
      "Association between consecutive within-individual changes; no drift term; uncentered R-squared.",
      "Association between equally weighted individual temporal means, not within-individual changes.",
      "Random effects augmented with individual means; inspect separate within-type and between-type components."),
    package = c(rep("plm", 5L), "stats", "stats", "plm"), available = requireNamespace("plm", quietly = TRUE), stringsAsFactors = FALSE)
  z$outcome_type <- "continuous"
  z <- rbind(z, data.frame(
    engine = c("panel_clogit", "panel_poisson", "panel_fe_iv"),
    estimator = c("Conditional logit (individual FE)", "Poisson (individual FE)", "FE-IV / 2SLS (individual FE)"),
    model = c("conditional_logit", "poisson", "within_iv"), effect = "individual",
    interpretation = c("Conditional within-individual log-odds association; exact likelihood.",
      "Multiplicative conditional-mean association with individual effects.",
      "Within-individual IV relationship, conditional on researcher-specified instrument assumptions."),
    package = c("survival", "fixest", "plm"),
    available = vapply(c("survival", "fixest", "plm"), requireNamespace, logical(1), quietly = TRUE),
    outcome_type = c("binary", "count", "continuous"), stringsAsFactors = FALSE))
  z

}

.ec_panel_formula <- function(formula, data, id, time) {
  if (!inherits(formula, "formula") || length(formula) != 3L) .ec_stop("Use a two-sided panel formula, e.g. y ~ x1 + x2.")
  # Explicit static, additive formula scope. No accidental cross-individual lag().
  tt <- stats::terms(formula)
  labs <- attr(tt, "term.labels")
  vars <- all.vars(formula)
  if (any(!vars %in% names(data)) || any(c(id, time) %in% vars)) .ec_stop("Model variables must exist and must not include the panel indexes.")
  allowed <- vapply(names(data), .ec_quote_name, character(1))
  if (!is.symbol(formula[[2L]]) || !length(labs) || any(!labs %in% c(names(data), allowed)) || attr(tt, "intercept") != 1L) {
    .ec_stop("This panel release accepts y ~ x1 + x2 with an intercept and named columns only. Precompute transformations; lagged/dynamic panel models are not supported yet.")
  }
  y <- as.character(formula[[2L]])
  if (!is.numeric(data[[y]]) || is.matrix(data[[y]])) .ec_stop("The panel response must be numeric and match the chosen outcome_type.")
  for (v in vars) if (!(is.numeric(data[[v]]) || is.factor(data[[v]]) || is.logical(data[[v]])) || is.matrix(data[[v]])) .ec_stop("Panel regressors must be numeric, logical or factors: ", v, ".")
  vars
}

#' Estimate static panel models with explicit indexes and inference
#' @param data A data.frame; unbalanced panels are permitted.
#' @param formula Static additive formula y ~ x1 + x2. Precompute transformations.
#' @param id Individual identifier column.
#' @param time Period column.
#' @param models Engine names from eco_panel_models().
#' @param inference Classical or cluster_id. Linear models use t inference; nonlinear classical inference uses normal z. Exact conditional logit does not support cluster_id.
#' @param na_action Either fail or omit. Omission is explicit, common to all models and recorded.
#' @param error_policy Either stop or collect model-specific failures.
#' @param outcome_type Explicit continuous, binary or count response family.
#' @param endogenous Numeric endogenous regressor names for panel_fe_iv.
#' @param instruments Numeric excluded instrument names for panel_fe_iv.
#' @return An econcompare_panel object containing raw plm/lm models, transformation maps, coefficients, covariance matrices, term status and sample audits.
#' @export
eco_panel_run <- function(data, formula, id, time,
                          models = c("panel_pooling", "panel_fe_individual"),
                          inference = c("classical", "cluster_id"),
                          na_action = c("fail", "omit"), error_policy = c("stop", "collect"),
                          outcome_type = c("continuous", "binary", "count"),
                          endogenous = NULL, instruments = NULL) {
  outcome_type <- match.arg(outcome_type)
  if (missing(models) && outcome_type != "continuous") models <- if (outcome_type == "binary") "panel_clogit" else "panel_poisson"
  inference <- match.arg(inference); na_action <- match.arg(na_action); error_policy <- match.arg(error_policy)
  audit <- eco_panel_audit(data, id, time)
  if (nrow(audit$issues)) .ec_stop("Panel indexes contain missing values or duplicate id-time pairs. Inspect eco_panel_audit(); no aggregation or index deletion is automatic.")
  data <- as.data.frame(data)
  vars <- .ec_panel_formula(formula, data, id, time)
  reg <- eco_panel_models(); models <- unique(as.character(models))
  if (!length(models) || anyNA(models) || any(!models %in% reg$engine)) .ec_stop("Choose supported model names from eco_panel_models().")
  if (any(reg$outcome_type[match(models, reg$engine)] != outcome_type)) .ec_stop("Selected engines do not match outcome_type. Compare models on the same explicitly chosen response scale.")
  if (any(!models %in% c("panel_clogit", "panel_poisson"))) .ec_require("plm", "linear panel estimation")
  iv <- NULL
  if ("panel_fe_iv" %in% models) {
    iv <- .ec_panel_iv_spec(data, formula, id, time, endogenous, instruments)
    vars <- unique(c(vars, instruments))
  } else if (length(endogenous) || length(instruments)) .ec_stop("Instrument selections require the panel_fe_iv engine.")
  keep <- stats::complete.cases(data[vars])
  for (v in vars) if (is.numeric(data[[v]])) keep <- keep & is.finite(data[[v]])
  if (any(!keep) && na_action == "fail") .ec_stop("Missing/non-finite model values. Choose na_action = 'omit' explicitly to use a common complete-case sample.")
  if (sum(keep) < 4L) .ec_stop("Too few complete observations for panel estimation.")
  dat <- droplevels(data[keep, , drop = FALSE]); original <- which(keep)
  # Preserve the input calendar even if omissions remove a whole period.
  z <- list(id = audit$index$id[keep], time = audit$index$period[keep])
  ord <- order(z$id, z$time)
  dat <- dat[ord, , drop = FALSE]; original <- original[ord]
  # Private engine data: user columns and original row identities stay intact.
  if (any(c(".ec_panel_id", ".ec_panel_time") %in% names(dat))) .ec_stop("Reserved column names .ec_panel_id/.ec_panel_time are present; rename them.")
  dat$.ec_panel_id <- z$id[ord]; dat$.ec_panel_time <- z$time[ord]
  engine_key <- paste(match(dat$.ec_panel_id, unique(dat$.ec_panel_id)), dat$.ec_panel_time, sep = ":")
  G <- length(unique(dat$.ec_panel_id)); Tn <- length(unique(dat$.ec_panel_time))
  if (G < 2L || Tn < 2L) .ec_stop("Panel estimation requires at least two individuals and two periods in the estimation sample.")
  used_audit <- .ec_panel_audit_on_grid(dat, id, time, audit)
  full_mm <- stats::model.matrix(formula, dat)
  if (any(!is.finite(full_mm))) .ec_stop("Non-finite design matrix.")
  fits <- meta <- rows <- covs <- terms_status <- model_audits <- exclusions <- list()
  warnings <- failures <- data.frame(model = character(), engine = character(), message = character(), stringsAsFactors = FALSE)
  information <- warnings
  samples <- transformations <- components <- mean_terms <- first_stages <- first_stage_tests <- list()
  for (nm in models) {
    rr <- reg[match(nm, reg$engine), ]
    captured <- character()
    result <- tryCatch(withCallingHandlers({
      if (nm %in% c("panel_clogit", "panel_poisson", "panel_fe_iv")) {
        .ec_panel_special_fit(nm, dat, formula, full_mm, original, inference, iv)
      } else if (nm %in% c("panel_fd", "panel_between", "panel_mundlak")) {
        .ec_panel_extension_fit(nm, dat, formula, full_mm, original,
          audit$summary$calendar_verified, inference)
      } else {
      modeldat <- dat
      # Singleton groups have no within information; prune explicitly and record.
      if (rr$model == "within") {
        repeat {
          keep_model <- rep(TRUE, nrow(modeldat))
          if (rr$effect %in% c("individual", "twoways")) keep_model <- keep_model & stats::ave(rep(1L, nrow(modeldat)), modeldat$.ec_panel_id, FUN = length) > 1L
          if (rr$effect %in% c("time", "twoways")) keep_model <- keep_model & stats::ave(rep(1L, nrow(modeldat)), modeldat$.ec_panel_time, FUN = length) > 1L
          if (all(keep_model) || !length(keep_model)) break
          modeldat <- modeldat[keep_model, , drop = FALSE]
        }
        if (nrow(modeldat) < 4L || length(unique(modeldat$.ec_panel_id)) < 2L || length(unique(modeldat$.ec_panel_time)) < 2L) .ec_stop("Insufficient panel variation after excluding singleton fixed-effect groups.")
      }
      fit <- plm::plm(formula, data = modeldat, index = c(".ec_panel_id", ".ec_panel_time"),
                      model = rr$model, effect = rr$effect, random.method = "swar", na.action = stats::na.fail)
      b <- stats::coef(fit)
      response <- plm::pmodel.response(fit)
      response_scale <- max(1, max(abs(response)))
      if (max(abs(response - mean(response))) <= 1e-12 * response_scale) .ec_stop("No usable response variation after the model transformation.")
      if (max(abs(stats::residuals(fit))) <= 1e-12 * response_scale) .ec_stop("Essentially perfect fit: coefficient inference is unreliable.")
      if (!length(b) || any(!is.finite(b)) || stats::df.residual(fit) <= 0) .ec_stop("No finite identifiable coefficients or no residual degrees of freedom.")
      fi <- plm::index(fit)
      fk <- paste(match(as.character(fi[[1L]]), unique(dat$.ec_panel_id)), as.numeric(as.character(fi[[2L]])), sep = ":")
      pos <- match(fk, engine_key)
      if (anyNA(pos) || anyDuplicated(pos)) .ec_stop("Could not verify the engine's exact estimation sample.")
      ng <- length(unique(fi[[1L]]))
      V <- if (inference == "classical") stats::vcov(fit) else plm::vcovHC(fit, method = "arellano", type = "HC1", cluster = "group")
      V <- V[names(b), names(b), drop = FALSE]
      if (any(!is.finite(V)) || any(diag(V) <= 0)) .ec_stop("Coefficient inference is unavailable: non-finite or non-positive variances.")
      df <- if (inference == "classical") stats::df.residual(fit) else ng - 1L
      if (df < 1) .ec_stop("Insufficient independent clusters for inference.")
      se <- sqrt(diag(V)); stat <- b/se
      co <- cbind(Estimate = b, `Std. Error` = se, `t value` = stat, `Pr(>|t|)` = 2 * stats::pt(abs(stat), df = df, lower.tail = FALSE))
      status <- data.frame(term = colnames(full_mm), status = "estimated", stringsAsFactors = FALSE)
      absent <- !status$term %in% names(b)
      # Inspect the transformation itself to distinguish absorption from aliasing.
      mm <- stats::model.matrix(fit, model = rr$model, effect = rr$effect, cstcovar.rm = "none")
      absorbed <- vapply(status$term, function(term) {
        if (rr$model != "within") return(FALSE)
        if (term == "(Intercept)") return(TRUE)
        if (!term %in% colnames(mm)) return(FALSE)
        j <- match(term, colnames(full_mm))
        max(abs(mm[, term])) <= 1e-10 * max(1, max(abs(full_mm[, j])))
      }, logical(1))
      status$status[absent & absorbed] <- "absorbed by fixed effects"
      status$status[absent & !absorbed] <- "not estimated: collinear after transformation"
      if (any(absent & !absorbed)) .ec_stop("Unidentified regressors after transformation: ", paste(status$term[absent & !absorbed], collapse = ", "), ". Remove redundant regressors explicitly.")
      attr(fit, "econcompare_panel_table") <- co
      attr(fit, "econcompare_panel_df") <- df
      attr(fit, "econcompare_panel_status") <- status
      list(fit = fit, V = V, status = status, pos = pos, df = df, groups = ng,
        unit = "individual-period", note = "",
        covariance_note = if (inference == "classical") "Model-based covariance; residual t degrees of freedom" else "plm Arellano HC1, individual clusters; t(G-1)",
        exclusion_reason = "singleton fixed-effect group (recursive for two-way FE)")
      }
    }, warning = function(w) {captured <<- c(captured, conditionMessage(w)); invokeRestart("muffleWarning")}), error = function(e) e)
    if (length(captured)) warnings <- rbind(warnings, data.frame(model = nm, engine = nm, message = unique(captured)))
    if (inherits(result, "error")) {
      if (error_policy == "stop") .ec_stop(nm, ": ", conditionMessage(result))
      failures <- rbind(failures, data.frame(model = nm, engine = nm, message = conditionMessage(result))); next
    }
    fits[[nm]] <- result$fit; covs[[nm]] <- result$V; terms_status[[nm]] <- result$status
    rows[[nm]] <- as.character(sort(original[result$pos]))
    if (length(result$first_stages)) first_stages[[nm]] <- result$first_stages
    if (!is.null(result$first_stage_tests) && nrow(result$first_stage_tests)) first_stage_tests[[nm]] <- result$first_stage_tests
    if (!is.null(result$mapping)) transformations[[nm]] <- result$mapping
    if (!is.null(result$components) && nrow(result$components)) components[[nm]] <- result$components
    if (!is.null(result$mean_terms) && nrow(result$mean_terms)) mean_terms[[nm]] <- result$mean_terms
    model_audits[[nm]] <- .ec_panel_audit_on_grid(dat[result$pos, , drop = FALSE], id, time, audit)
    lost <- setdiff(original, original[result$pos])
    exclusions[[nm]] <- data.frame(row = lost, reason = rep(result$exclusion_reason, length(lost)))
    if (length(lost)) warnings <- rbind(warnings, data.frame(model = nm, engine = nm, message = paste(length(lost), "source observations do not contribute to this estimator; see sample_exclusions.")))
    if (any(result$status$status != "estimated")) information <- rbind(information, data.frame(model = nm, engine = nm, message = "Some terms are absorbed by the model transformation; inspect term_status. They are not zero-effect estimates."))
    meta[[nm]] <- list(engine = nm, outcome_type = outcome_type, family = rr$estimator,
      model = rr$model, effect = rr$effect, inference = inference, inference_df = result$df,
      coefficient_scale = if (is.null(result$scale)) "response units" else result$scale,
      reference_distribution = if (is.infinite(result$df)) "normal z" else "Student t",
      observation_unit = result$unit, covariance_note = result$covariance_note,
      groups = result$groups, comparison_note = paste(rr$interpretation, result$note, "No causal conclusion or automatic estimator ranking."))
    samples[[nm]] <- data.frame(model = nm, engine = nm, n_original = nrow(data), n_used = length(rows[[nm]]),
      n_effective = if (is.null(attr(result$fit, "econcompare_panel_nobs"))) stats::nobs(result$fit) else attr(result$fit, "econcompare_panel_nobs"), observation_unit = result$unit, zero_weight_n = 0L, n_dropped = nrow(data)-length(rows[[nm]]),
      row_identity = "exact", row_identity_source = "verified contributing source rows; see transformations for observation membership", row_ids_available = TRUE,
      row_ids = paste(rows[[nm]], collapse = "\u001f"), stringsAsFactors = FALSE)
  }
  if (!length(fits)) .ec_stop("No panel model succeeded. ", paste(failures$message, collapse = "; "))
  out <- list(call = match.call(), formula = formula, models = fits, meta = meta,
    analysis_type = "panel", outcome_type = outcome_type, panel_id = id, time_variable = time,
    panel_audit = audit, estimation_audit = used_audit, inference = inference,
    iv_specification = iv, first_stages = first_stages, first_stage_tests = first_stage_tests,
    transformations = transformations, components = components, mean_terms = mean_terms,
    covariance = covs, term_status = terms_status, model_audits = model_audits, sample_exclusions = exclusions, sample_info = do.call(rbind, samples), sample_rows = rows,
    sample_comparable = all(vapply(rows, identical, logical(1), rows[[1L]])) && length(unique(vapply(meta, function(m) m$observation_unit, character(1)))) == 1L,
    excluded_rows = data.frame(row = which(!keep), reason = rep("missing/non-finite model value", sum(!keep))),
    estimation_data = dat, information = information, warnings = warnings, failures = failures,
    panel_metadata = list(groups = G, periods = Tn, inference = inference,
      covariance = paste(unique(vapply(meta, function(m) m$covariance_note, character(1))), collapse = " | "),
      note = paste(if (inference == "cluster_id") "Cluster inference assumes independence across individuals. Few clusters can make inference unreliable; report the group count." else "Model-based inference requires the estimator-specific assumptions shown above.", "Static estimation permits calendar gaps; serial tests are restricted.")),
    data_n = nrow(data), created = Sys.time(), version = .ec_version())
  out$provenance <- .ec_provenance(out)
  class(out) <- c("econcompare_panel", "econcompare")
  out
}

.ec_extract_panel <- function(model, name) {
  co <- attr(model, "econcompare_panel_table")
  out <- .ec_matrix_to_df(co, name)
  status <- attr(model, "econcompare_panel_status")
  absent <- status$term[status$status != "estimated"]
  if (length(absent)) out <- rbind(out, data.frame(model = name, term = absent, estimate = NA_real_, std.error = NA_real_, statistic = NA_real_, p.value = NA_real_))
  out$term_status <- status$status[match(out$term, status$term)]
  out$nobs <- if (is.null(attr(model, "econcompare_panel_nobs"))) stats::nobs(model) else attr(model, "econcompare_panel_nobs")
  out$r2 <- if (isTRUE(attr(model, "econcompare_panel_no_r2"))) NA_real_ else if (inherits(model, "plm")) .ec_try_scalar(plm::r.squared(model), stat = "r2") else summary(model)$r.squared
  out$adj_r2 <- if (isTRUE(attr(model, "econcompare_panel_no_r2"))) NA_real_ else if (inherits(model, "plm")) .ec_try_scalar(plm::r.squared(model, dfcor = TRUE), stat = "adj_r2") else summary(model)$adj.r.squared
  out$logLik <- out$aic <- out$bic <- out$deviance <- NA_real_
  out$unavailable_stats <- paste(if (isTRUE(attr(model, "econcompare_panel_no_r2"))) "r2, adj_r2: not supplied for this estimator;" else "", "logLik, AIC, BIC, deviance: not supplied for cross-estimator panel ranking")
  out$inference_df <- attr(model, "econcompare_panel_df")
  out
}

#' Diagnostic tests for static panel models
#' @param x An object returned by eco_panel_run().
#' @param tests Test ids: effects_f, effects_lm, serial, dependence, hausman, mundlak, iv_first_stage.
#' @param serial_order Positive lag order for the panel BG test.
#' @param alpha Significance level; no automatic model selection is performed.
#' @return A table with hypothesis, status, statistic, p.value and explanatory notes. Unsupported cases are reported, not silently re-estimated.
#' @export
eco_panel_diagnostics <- function(x, tests = c("effects_f", "effects_lm", "serial", "dependence"), serial_order = 1L, alpha = .05) {
  if (!inherits(x, "econcompare_panel")) .ec_stop("Use an object from eco_panel_run().")
  allowed <- c("effects_f", "effects_lm", "serial", "dependence", "hausman", "mundlak", "iv_first_stage")
  if (!length(tests) || anyNA(tests) || any(!tests %in% allowed)) .ec_stop("Unknown or empty panel diagnostic selection.")
  if (!is.numeric(alpha) || length(alpha)!=1L || !is.finite(alpha) || alpha<=0 || alpha>=1) .ec_stop("alpha must lie strictly between 0 and 1.")
  serial_order <- .ec_validate_lag_order(serial_order, "serial_order", 100L)
  if (serial_order < 1L) .ec_stop("serial_order must be positive.")
  records <- list()
  add <- function(nm, test, null, expr, note = "", suspend = FALSE) {
    caught <- character()
    ans <- tryCatch(withCallingHandlers(expr, warning = function(w) {caught <<- c(caught, conditionMessage(w)); invokeRestart("muffleWarning")}), error = function(e) e)
    ok <- !inherits(ans, "error") && length(ans$p.value) == 1L && is.finite(ans$p.value) && length(ans$statistic) == 1L && is.finite(ans$statistic)
    reason <- if (inherits(ans, "error")) conditionMessage(ans) else if (!ok) "Non-finite test result." else ""
    code <- if (ok && suspend) "calibration_unverified" else if (ok) "available" else if (!is.null(ans$reason_code)) ans$reason_code else "calculation_failed"
    mode <- if (test %in% c("mundlak", "hausman")) x$inference else "test-specific calibration"
    if (test == "mundlak" && !is.null(x$meta$panel_mundlak)) mode <- paste(mode, x$meta$panel_mundlak$covariance_note, sep = ": ")
    if (!ok) mode <- "No inference performed"
    records[[length(records)+1L]] <<- data.frame(model = nm, test = test, null_hypothesis = null,
      status = if (ok && suspend) "computed_uninterpreted" else if (ok) "computed" else "unavailable",
      reason_code = code, inference = mode,
      inference_df = if (ok && test == "mundlak" && !is.null(x$meta$panel_mundlak)) x$meta$panel_mundlak$inference_df else NA_real_, statistic = if(ok) unname(ans$statistic) else NA_real_,
      p.value = if(ok && !suspend) ans$p.value else NA_real_,
      raw_p.value = if(ok) ans$p.value else NA_real_,
      parameters = if(ok) paste(names(ans$parameter), ans$parameter, collapse = "; ") else "",
      interpretation = if(suspend && ok) "Conclusion suspended: calibration unverified after time effects" else if(!ok) "No conclusion" else if(ans$p.value < alpha) "Reject H0 at the selected level" else "Do not reject H0; this does not establish H0",
      note = paste(c(if (ok) note, reason, caught), collapse = " "), stringsAsFactors = FALSE)
  }
  pool <- x$models$panel_pooling
  for (nm in names(x$models)) {
    m <- x$models[[nm]]; md <- x$meta[[nm]]
    if ("effects_f" %in% tests && md$model == "within") add(nm, "effects_f", "No fixed effects relative to the pooled specification", {
      if (is.null(pool)) .ec_panel_diag_stop("missing_models", "Fit panel_pooling on the same sample to run this comparison.")
      if (!identical(x$sample_rows[[nm]], x$sample_rows$panel_pooling)) .ec_panel_diag_stop("not_applicable", "Different estimation samples.")
      if (x$inference != "classical") .ec_panel_diag_stop("not_applicable", "The classical F test is not made cluster-robust by the coefficient covariance choice. Refit with classical inference only if its assumptions are defensible.")
      plm::pFtest(m, pool)
    }, "Classical F test; not a robust selection rule.")
    if ("effects_lm" %in% tests && md$model == "pooling") add(nm, "effects_lm", "Zero individual random-effect variance", {
      if (x$inference != "classical") .ec_panel_diag_stop("not_applicable", "This is a classical LM test, not a cluster-robust test.")
      plm::plmtest(m, effect = "individual", type = "bp")
    }, "Breusch-Pagan panel effects test, not the heteroskedasticity test.")
    if ("serial" %in% tests) add(nm, "panel_bg", "No serial correlation up to the chosen order", {
      if (nm == "panel_between") .ec_panel_diag_stop("not_applicable", "Between has one mean per individual and no residual time series for panel BG.")
      if (!inherits(m, "plm") || nm == "panel_fe_iv") .ec_panel_diag_stop("not_implemented", "A serial diagnostic adapted to this estimator is not implemented. The usual level-model BG is not substituted.")
      a <- x$model_audits[[nm]]
      if (!isTRUE(a$summary$calendar_verified) || anyNA(a$by_individual$internal_missing_periods) || any(a$by_individual$internal_missing_periods > 0)) .ec_panel_diag_stop("not_applicable", "Serial testing requires verified consecutive periods within each individual; no calendar compression is allowed.")
      if (min(a$by_individual$periods) <= serial_order + 1L) .ec_panel_diag_stop("not_applicable", "Too few periods in at least one individual for this serial order.")
      plm::pbgtest(m, order = serial_order, type = "Chisq")
    }, "BG on the transformed panel model; asymptotic test, not automatically robust to every error structure.")
    if ("dependence" %in% tests) add(nm, "pesaran_cd", "No cross-sectional residual dependence", {
      if (nm == "panel_between") .ec_panel_diag_stop("not_applicable", "Between has no period-indexed residual series for this panel CD test.")
      if (!inherits(m, "plm") || nm == "panel_fe_iv") .ec_panel_diag_stop("not_implemented", "A cross-sectional dependence diagnostic adapted to this estimator is not implemented.")
      if (min(x$model_audits[[nm]]$by_individual$periods) < 3L) .ec_panel_diag_stop("not_applicable", "At least three observations per individual are required by this conservative diagnostic gate.")
      plm::pcdtest(m, test = "cd")
    }, if (md$effect %in% c("time", "twoways")) "Time effects can bias the standard Pesaran CD calibration (Juodis and Reese, 2022). Statistic and raw_p.value are retained for audit only; p.value and automatic rejection are suppressed. No corrected test is substituted." else "Interpret with the panel dimensions, common observation periods and fitted effects; not proof of independence.",
      suspend = md$effect %in% c("time", "twoways"))
  }
  if ("mundlak" %in% tests) add("panel_mundlak", "mundlak", "All added individual-mean coefficients equal zero", {
    m <- x$models$panel_mundlak
    if (is.null(m)) .ec_panel_diag_stop("missing_models", "Fit panel_mundlak to test its individual means jointly.")
    terms <- x$mean_terms$panel_mundlak$term
    b <- stats::coef(m)[terms]; V <- x$covariance$panel_mundlak[terms, terms, drop = FALSE]
    if (!length(b) || any(!is.finite(V)) || qr(V)$rank != length(b)) .ec_stop("Singular covariance for the joint mean restrictions; no Wald conclusion.")
    if (inherits(try(chol(V), silent = TRUE), "try-error")) .ec_stop("Mean-restriction covariance is not positive definite.")
    q <- length(b); df <- x$meta$panel_mundlak$inference_df
    statistic <- as.numeric(crossprod(b, solve(V, b))) / q
    list(statistic = statistic, parameter = c(df1 = q, df2 = df),
      p.value = stats::pf(statistic, q, df, lower.tail = FALSE))
  }, "Approximate Wald F with the selected coefficient covariance. Tests the specified mean terms, not every form of endogeneity; non-rejection does not validate RE.")
  if ("hausman" %in% tests) add("panel_fe_individual vs panel_re", "hausman", "FE/RE coefficient differences are not systematic under RE orthogonality", {
    fe <- x$models$panel_fe_individual; re <- x$models$panel_re
    if (is.null(fe) || is.null(re)) .ec_panel_diag_stop("missing_models", "Fit both individual FE and individual RE.")
    if (!identical(x$sample_rows$panel_fe_individual, x$sample_rows$panel_re)) .ec_panel_diag_stop("not_applicable", "Hausman comparison requires identical samples.")
    if (x$inference == "classical") {
      ans <- plm::phtest(fe, re)
      if (any(!is.finite(ans$statistic)) || ans$statistic < 0) .ec_stop("Invalid Hausman covariance difference; no conclusion is reported.")
      ans
    } else {
      if (!setequal(names(stats::coef(fe)), setdiff(names(stats::coef(re)), "(Intercept)"))) .ec_panel_diag_stop("not_applicable", "Robust auxiliary Hausman is restricted here to matching slope sets; remove time-invariant regressors explicitly for this diagnostic.")
      plm::phtest(x$formula, data = x$estimation_data, index = c(".ec_panel_id", ".ec_panel_time"),
        model = c("within", "random"), effect = "individual", method = "aux",
        vcov = function(m) plm::vcovHC(m, method = "arellano", type = "HC1", cluster = "group"))
    }
  }, "No automatic FE/RE choice; a non-rejection does not validate RE exogeneity.")
  if ("iv_first_stage" %in% tests) {
    if (is.null(x$models$panel_fe_iv)) add("panel_fe_iv", "iv_first_stage", "Excluded instruments have zero first-stage coefficients", .ec_panel_diag_stop("missing_models", "Fit panel_fe_iv with explicit instruments.")) else {
      for (j in seq_len(nrow(x$first_stage_tests$panel_fe_iv))) {
        z <- x$first_stage_tests$panel_fe_iv[j, ]
        add("panel_fe_iv", paste("iv_first_stage", z$endogenous, sep = ": "), "Excluded instruments have zero first-stage coefficients", {
          if (z$status != "computed") .ec_panel_diag_stop("calculation_failed", "First-stage covariance is singular or invalid.")
          list(statistic = z$statistic, p.value = z$p.value, parameter = c(df1 = z$df1, df2 = z$df2))
        }, paste(z$note, "Inference:", z$inference))
      }
    }
  }
  if (!length(records)) add("panel", "requested diagnostics", "Not applicable", .ec_panel_diag_stop("not_applicable", "None of the requested tests applies to the fitted estimators."))
  do.call(rbind, records)
}

.ec_panel_candidates <- function(data) {
  ids <- names(data)[grepl("(^|_)(id|individual|entity|firm|country|person|region|school|household|unit)($|_)", tolower(names(data)))]
  times <- names(data)[vapply(names(data), .ec_time_name_score, numeric(1)) > 0]
  out <- list()
  for (id in ids) for (time in setdiff(times, id)) {
    z <- tryCatch(.ec_panel_index(data, id, time), error = function(e) NULL)
    if (is.null(z) || any(z$missing_id | z$missing_time | z$duplicates)) next
    if (length(unique(z$id)) < 2L || length(unique(z$id)) == nrow(data) || length(unique(z$time)) < 2L) next
    out[[length(out)+1L]] <- data.frame(id = id, time = time, stringsAsFactors = FALSE)
  }
  if (length(out)) do.call(rbind, out) else data.frame(id = character(), time = character())
}
