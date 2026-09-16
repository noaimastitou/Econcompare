.ec_binary_prediction_metrics <- function(model, engine, threshold = 0.5) {
  mf <- tryCatch(stats::model.frame(model), error = function(e) NULL)
  if (is.null(mf)) return(NULL)
  y <- tryCatch(as.numeric(stats::model.response(mf)), error = function(e) numeric())
  p <- tryCatch(as.numeric(stats::fitted(model)), error = function(e) numeric())
  if (!length(y) || !length(p) || length(y) != length(p)) return(NULL)
  keep <- is.finite(y) & is.finite(p)
  y <- y[keep]; p <- p[keep]
  if (!length(y)) return(NULL)

  in_probability_range <- all(p >= 0 & p <= 1)
  pred_class <- as.numeric(p >= threshold)
  data.frame(
    engine = engine,
    n = length(y),
    event_rate = mean(y),
    mean_prediction = mean(p),
    prediction_min = min(p),
    prediction_max = max(p),
    accuracy_0_5 = mean(pred_class == y),
    rmse = sqrt(mean((p - y)^2)),
    brier_score = if (in_probability_range) mean((p - y)^2) else NA_real_,
    predictions_in_0_1 = in_probability_range,
    stringsAsFactors = FALSE
  )
}

#' Compare simple in-sample prediction summaries for binary models
#'
#' Provides a deliberately small set of common descriptive quantities for LPM,
#' logit and probit models. These are exploration aids rather than a model-selection
#' rule. The Brier score is reported only when fitted values lie in [0, 1]; this
#' avoids treating out-of-range LPM fitted values as valid probabilities.
#'
#' @param x An `econcompare` object containing binary models.
#' @param threshold Classification threshold used for the descriptive accuracy rate.
#' @return A data.frame with model-level in-sample prediction summaries.
#' @export
eco_binary_compare <- function(x, threshold = 0.5) {
  if (!inherits(x, "econcompare")) .ec_stop("`x` must be an econcompare object.")
  if (!identical(x$outcome_type, "binary")) .ec_stop("`eco_binary_compare()` is available only for binary-outcome comparisons.")
  if (length(threshold) != 1L || !is.finite(threshold) || threshold <= 0 || threshold >= 1) {
    .ec_stop("`threshold` must be a single number strictly between 0 and 1.")
  }

  pieces <- lapply(names(x$models), function(nm) {
    engine <- x$meta[[nm]]$engine
    if (!engine %in% c("lpm", "logit", "probit")) return(NULL)
    z <- .ec_binary_prediction_metrics(x$models[[nm]], engine, threshold = threshold)
    if (is.null(z)) return(NULL)
    z$model <- nm
    z$response_coding <- if (!is.null(x$meta[[nm]]$response_coding)) x$meta[[nm]]$response_coding else ""
    z[, c("model", "engine", "n", "event_rate", "mean_prediction", "prediction_min", "prediction_max",
          "accuracy_0_5", "rmse", "brier_score", "predictions_in_0_1", "response_coding"), drop = FALSE]
  })
  out <- do.call(rbind, pieces)
  if (is.null(out)) return(data.frame())
  rownames(out) <- NULL

  audit <- tryCatch(eco_sample_audit(x), error = function(e) NULL)
  if (!is.null(audit) && nrow(audit)) {
    out$sample_comparison <- vapply(out$model, function(nm) {
      i <- match(nm, audit$model)
      if (is.na(i)) return("unavailable")
      if (isTRUE(audit$matches_reference_rows[i])) return("same verified rows")
      if (identical(audit$matches_reference_rows[i], FALSE)) return("different verified rows")
      if (isTRUE(audit$matches_reference_n[i])) return("same n; row identity not verified")
      "different n"
    }, character(1))
  }
  out
}
