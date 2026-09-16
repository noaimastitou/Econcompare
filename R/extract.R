.ec_scalar_numeric <- function(x, missing = "na", stat = NULL) {
  missing <- match.arg(missing, c("na", "error"))
  if (length(x) == 0L || is.null(x)) {
    if (identical(missing, "error")) {
      .ec_stop("Statistic `", if (is.null(stat)) "unknown" else stat,
               "` returned a length-0 value.")
    }
    return(NA_real_)
  }
  out <- suppressWarnings(as.numeric(x)[1L])
  if (!length(out) || is.na(out) && length(x) == 0L) {
    if (identical(missing, "error")) .ec_stop("Statistic returned no numeric value.")
    return(NA_real_)
  }
  out
}

.ec_try_scalar <- function(expr, missing = "na", stat = NULL) {
  tryCatch(.ec_scalar_numeric(expr, missing = missing, stat = stat),
           error = function(e) {
             if (identical(missing, "error")) stop(e)
             NA_real_
           })
}

.ec_fit_stats <- function(model, missing = "na") {
  sm <- tryCatch(summary(model), error = function(e) NULL)
  r2 <- adj <- NA_real_
  if (!is.null(sm) && !is.null(sm$r.squared)) r2 <- .ec_scalar_numeric(sm$r.squared, missing, "r2")
  if (!is.null(sm) && !is.null(sm$adj.r.squared)) adj <- .ec_scalar_numeric(sm$adj.r.squared, missing, "adj_r2")

  vals <- list(
    nobs = .ec_try_scalar(stats::nobs(model), missing, "nobs"),
    r2 = r2,
    adj_r2 = adj,
    logLik = .ec_try_scalar(stats::logLik(model), missing, "logLik"),
    aic = .ec_try_scalar(stats::AIC(model), missing, "aic"),
    bic = .ec_try_scalar(stats::BIC(model), missing, "bic"),
    deviance = .ec_try_scalar(stats::deviance(model), missing, "deviance")
  )

  missing_names <- names(vals)[vapply(vals, function(v) length(v) == 0L || is.na(v[1L]), logical(1))]
  data.frame(
    nobs = vals$nobs,
    r2 = vals$r2,
    adj_r2 = vals$adj_r2,
    logLik = vals$logLik,
    aic = vals$aic,
    bic = vals$bic,
    deviance = vals$deviance,
    unavailable_stats = if (length(missing_names)) paste(missing_names, collapse = ", ") else "",
    stringsAsFactors = FALSE
  )
}

.ec_matrix_to_df <- function(co, model_name, prefix = NULL) {
  co <- as.matrix(co)
  if (!nrow(co)) return(data.frame())
  cn <- colnames(co)
  pick <- function(pattern, fallback = NA_real_) {
    hit <- grep(pattern, cn, ignore.case = TRUE)
    if (length(hit)) suppressWarnings(as.numeric(co[, hit[1]])) else rep(fallback, nrow(co))
  }
  est <- if (ncol(co)) suppressWarnings(as.numeric(co[, 1])) else rep(NA_real_, nrow(co))
  se <- pick("std|s\\.e\\.|se$|error")
  stat <- pick("t value|z value|statistic|stat$")
  p <- pick("pr\\(|p.value|p-value|p value|pvalue")
  term <- rownames(co)
  if (is.null(term)) term <- paste0("term", seq_len(nrow(co)))
  if (!is.null(prefix)) term <- paste0(prefix, ": ", term)
  data.frame(model = model_name, term = term, estimate = est, std.error = se,
             statistic = stat, p.value = p, stringsAsFactors = FALSE)
}

.ec_bind_fit <- function(coef_df, model, missing = "na") {
  fs <- .ec_fit_stats(model, missing = missing)
  if (!nrow(coef_df)) return(coef_df)
  for (nm in names(fs)) coef_df[[nm]] <- fs[[nm]][1]
  coef_df
}

.ec_extract_standard <- function(model, name, missing = "na") {
  sm <- summary(model)
  co <- sm$coefficients
  if (is.null(co)) co <- sm$coef
  if (is.null(co)) co <- sm$estimate
  if (is.null(co)) .ec_stop("Could not find a coefficient table for model class: ", paste(class(model), collapse = "/"))
  .ec_bind_fit(.ec_matrix_to_df(co, name), model, missing)
}

.ec_extract_fixest <- function(model, name, missing = "na") {
  sm <- summary(model); co <- as.matrix(sm$coeftable)
  out <- .ec_matrix_to_df(co, name)
  fs <- .ec_fit_stats(model, missing)
  fs$r2 <- .ec_try_scalar(fixest::r2(model, "r2"), missing, "r2")
  fs$adj_r2 <- .ec_try_scalar(fixest::r2(model, "ar2"), missing, "adj_r2")
  stat_names <- setdiff(names(fs), "unavailable_stats")
  fs$unavailable_stats <- paste(stat_names[vapply(fs[stat_names], function(v) is.na(v[1]), logical(1))], collapse = ", ")
  for (nm in names(fs)) out[[nm]] <- fs[[nm]][1]
  out
}

.ec_extract_lm_robust <- function(model, name, missing = "na") {
  out <- data.frame(model = name, term = as.character(model$term),
                    estimate = as.numeric(model$coefficients), std.error = as.numeric(model$std.error),
                    statistic = as.numeric(model$statistic), p.value = as.numeric(model$p.value),
                    stringsAsFactors = FALSE)
  fs <- .ec_fit_stats(model, missing)
  if (!is.null(model$N) && length(model$N)) fs$nobs <- .ec_scalar_numeric(model$N, missing, "nobs")
  if (!is.null(model$r.squared) && length(model$r.squared)) fs$r2 <- .ec_scalar_numeric(model$r.squared, missing, "r2")
  if (!is.null(model$adj.r.squared) && length(model$adj.r.squared)) fs$adj_r2 <- .ec_scalar_numeric(model$adj.r.squared, missing, "adj_r2")
  stat_cols <- c("nobs", "r2", "adj_r2", "logLik", "aic", "bic", "deviance")
  fs$unavailable_stats <- paste(stat_cols[vapply(fs[stat_cols], function(v) is.na(v[1]), logical(1))], collapse = ", ")
  for (nm in names(fs)) out[[nm]] <- fs[[nm]][1]
  out
}

.ec_extract_rq <- function(model, name, missing = "na") {
  a <- attr(model, "econcompare_quantile_summary_args")
  if (is.null(a)) a <- list(se = "nid")
  sm <- do.call(summary, c(list(object = model), a))
  .ec_bind_fit(.ec_matrix_to_df(sm$coefficients, name), model, missing)
}

 .ec_extract_time_inference <- function(model, name, missing = "na") {
  ct <- attr(model, "econcompare_time_coeftest")
  if (is.null(ct)) return(NULL)
  .ec_bind_fit(.ec_matrix_to_df(ct, name), model, missing)
}


.ec_extract_system_equation <- function(model, name, missing = "na") {
  co <- as.matrix(model$coef_table)
  out <- .ec_matrix_to_df(co, name)
  out$nobs <- if (length(model$nobs) && is.finite(model$nobs)) as.numeric(model$nobs) else NA_real_
  out$r2 <- NA_real_; out$adj_r2 <- NA_real_; out$logLik <- NA_real_
  out$aic <- NA_real_; out$bic <- NA_real_; out$deviance <- NA_real_
  out$unavailable_stats <- "r2, adj_r2, logLik, aic, bic, deviance (system-equation wrapper)"
  out
}

.ec_extract <- function(model, name, missing = "na") {
  if (inherits(model, "econcompare_system_equation")) return(.ec_extract_system_equation(model, name, missing))
  ti <- .ec_extract_time_inference(model, name, missing)
  if (!is.null(ti)) return(ti)
  if (inherits(model, "fixest")) return(.ec_extract_fixest(model, name, missing))
  if (inherits(model, "lm_robust")) return(.ec_extract_lm_robust(model, name, missing))
  if (inherits(model, "rq")) return(.ec_extract_rq(model, name, missing))
  if (inherits(model, "multinom")) return(.ec_extract_multinom(model, name, missing))
  if (inherits(model, "polr")) return(.ec_extract_polr(model, name, missing))
  .ec_extract_standard(model, name, missing)
}

.ec_empty_compare <- function() {
  data.frame(
    model = character(), term = character(), estimate = numeric(), std.error = numeric(),
    statistic = numeric(), p.value = numeric(), nobs = numeric(), r2 = numeric(),
    adj_r2 = numeric(), logLik = numeric(), aic = numeric(), bic = numeric(),
    deviance = numeric(), unavailable_stats = character(), engine = character(),
    outcome_type = character(), family = character(), comparison_note = character(),
    stringsAsFactors = FALSE
  )
}

.ec_empty_extraction_issue <- function() {
  data.frame(model = character(), engine = character(), stage = character(), message = character(), stringsAsFactors = FALSE)
}

.ec_meta_value <- function(x, model, field, default = "") {
  meta <- if (is.list(x$meta)) x$meta[[model]] else NULL
  value <- if (is.list(meta)) meta[[field]] else NULL
  if (is.null(value) || !length(value) || is.na(value[1L])) return(as.character(default)[1L])
  as.character(value)[1L]
}

.ec_decorate_extract <- function(out, x) {
  if (is.null(out) || !nrow(out)) return(out)
  out$engine <- vapply(out$model, function(nm) .ec_meta_value(x, nm, "engine", nm), character(1))
  out$outcome_type <- vapply(out$model, function(nm) .ec_meta_value(x, nm, "outcome_type", if (is.null(x$outcome_type)) "" else x$outcome_type), character(1))
  out$family <- vapply(out$model, function(nm) .ec_meta_value(x, nm, "family", ""), character(1))
  out$comparison_note <- vapply(out$model, function(nm) .ec_meta_value(x, nm, "comparison_note", ""), character(1))
  if (x$analysis_type %in% c("time_series", "time_series_system")) {
    if (identical(x$analysis_type, "time_series")) out$inference <- vapply(out$model, function(nm) .ec_meta_value(x, nm, "inference", "classical"), character(1))
    out$sample_comparable <- if (isTRUE(x$sample_comparable)) "same estimation sample across compared models" else "different estimation samples — interpret AIC/BIC comparisons cautiously"
  }
  out
}

#' Compare collected model results
#'
#' @param x An `econcompare` object returned by [eco_run()].
#' @param empty_stats How to handle statistics returned with length 0. `"na"`
#'   converts them to `NA_real_`; `"error"` stops and reports the statistic.
#' @param error_policy How extraction failures are handled. `"stop"` preserves
#'   strict behaviour. `"collect"` keeps successfully extracted models and stores
#'   model-specific extraction warnings/failures as attributes on the returned
#'   data.frame.
#' @return A tidy data.frame with coefficient statistics and model fit information.
#'   With `error_policy = "collect"`, attributes `extraction_warnings` and
#'   `extraction_failures` contain model-specific extraction issues.
#' @export
eco_compare <- function(x, empty_stats = c("na", "error"), error_policy = c("stop", "collect")) {
  if (!inherits(x, "econcompare")) .ec_stop("`x` must be an econcompare object.")
  empty_stats <- match.arg(empty_stats)
  error_policy <- match.arg(error_policy)

  if (identical(error_policy, "stop")) {
    pieces <- Map(function(m, n) .ec_extract(m, n, missing = empty_stats), x$models, names(x$models))
    out <- if (length(pieces)) do.call(rbind, pieces) else .ec_empty_compare()
    if (is.null(out)) out <- .ec_empty_compare()
    rownames(out) <- NULL
    return(.ec_decorate_extract(out, x))
  }

  pieces <- list()
  warning_rows <- list()
  failure_rows <- list()
  for (nm in names(x$models)) {
    engine <- .ec_meta_value(x, nm, "engine", nm)
    captured <- character()
    ans <- tryCatch(
      withCallingHandlers(
        .ec_extract(x$models[[nm]], nm, missing = empty_stats),
        warning = function(w) {
          captured <<- c(captured, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) e
    )
    if (length(captured)) {
      warning_rows[[nm]] <- data.frame(
        model = nm, engine = engine, stage = "extraction",
        message = unique(captured), stringsAsFactors = FALSE
      )
    }
    if (inherits(ans, "error")) {
      failure_rows[[nm]] <- data.frame(
        model = nm, engine = engine, stage = "extraction",
        message = conditionMessage(ans), stringsAsFactors = FALSE
      )
      next
    }
    pieces[[nm]] <- ans
  }

  out <- if (length(pieces)) do.call(rbind, pieces) else .ec_empty_compare()
  if (is.null(out)) out <- .ec_empty_compare()
  rownames(out) <- NULL
  out <- .ec_decorate_extract(out, x)
  warnings <- if (length(warning_rows)) do.call(rbind, warning_rows) else .ec_empty_extraction_issue()
  failures <- if (length(failure_rows)) do.call(rbind, failure_rows) else .ec_empty_extraction_issue()
  rownames(warnings) <- NULL; rownames(failures) <- NULL
  attr(out, "extraction_warnings") <- warnings
  attr(out, "extraction_failures") <- failures
  out
}

# Multinomial and ordered-outcome extractors are intentionally explicit because
# their coefficient tables are not shaped like lm/glm tables.
.ec_extract_multinom <- function(model, name, missing = "na") {
  sm <- summary(model)
  b <- sm$coefficients
  se <- sm$standard.errors
  if (is.null(b) || is.null(se)) .ec_stop("Could not extract multinomial coefficients and standard errors.")
  b <- as.matrix(b); se <- as.matrix(se)
  cats <- rownames(b); if (is.null(cats)) cats <- paste0("category", seq_len(nrow(b)))
  terms <- colnames(b); if (is.null(terms)) terms <- paste0("term", seq_len(ncol(b)))
  rows <- lapply(seq_len(nrow(b)), function(i) {
    data.frame(
      model = name,
      term = paste0(cats[i], ": ", terms),
      estimate = as.numeric(b[i, ]),
      std.error = as.numeric(se[i, ]),
      statistic = NA_real_,
      p.value = NA_real_,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  fs <- .ec_fit_stats(model, missing)
  if (is.na(fs$nobs[1L]) && !is.null(model$model)) fs$nobs <- nrow(model$model)
  for (nm in names(fs)) out[[nm]] <- fs[[nm]][1L]
  out
}

.ec_extract_polr <- function(model, name, missing = "na") {
  sm <- summary(model)
  co <- as.matrix(sm$coefficients)
  out <- .ec_matrix_to_df(co, name)
  beta_names <- names(stats::coef(model))
  is_beta <- out$term %in% beta_names
  out$term[!is_beta] <- paste0("threshold: ", out$term[!is_beta])
  # MASS::polr does not provide coefficient p-values in summary(); econcompare
  # does not invent them. The reported t values are retained as statistics.
  out$p.value <- NA_real_
  .ec_bind_fit(out, model, missing)
}
