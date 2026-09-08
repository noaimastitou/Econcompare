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
  if (all(is.na(p)) && !all(is.na(stat))) p <- 2 * stats::pnorm(abs(stat), lower.tail = FALSE)
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
  sm <- summary(model, se = "nid")
  .ec_bind_fit(.ec_matrix_to_df(sm$coefficients, name), model, missing)
}

.ec_extract <- function(model, name, missing = "na") {
  if (inherits(model, "fixest")) return(.ec_extract_fixest(model, name, missing))
  if (inherits(model, "lm_robust")) return(.ec_extract_lm_robust(model, name, missing))
  if (inherits(model, "rq")) return(.ec_extract_rq(model, name, missing))
  .ec_extract_standard(model, name, missing)
}

#' Compare collected model results
#'
#' @param x An `econcompare` object returned by [eco_run()].
#' @param empty_stats How to handle statistics returned with length 0. `"na"`
#'   converts them to `NA_real_`; `"error"` stops and reports the statistic.
#' @return A tidy data.frame with coefficient statistics and model fit information.
#' @export
eco_compare <- function(x, empty_stats = c("na", "error")) {
  if (!inherits(x, "econcompare")) .ec_stop("`x` must be an econcompare object.")
  empty_stats <- match.arg(empty_stats)
  pieces <- Map(function(m, n) .ec_extract(m, n, missing = empty_stats), x$models, names(x$models))
  out <- do.call(rbind, pieces); rownames(out) <- NULL
  if (!is.null(x$meta)) {
    out$engine <- vapply(out$model, function(nm) x$meta[[nm]]$engine, character(1))
    out$family <- vapply(out$model, function(nm) x$meta[[nm]]$family, character(1))
  }
  out
}
