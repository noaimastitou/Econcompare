#' Lightweight diagnostics for collected models
#'
#' @param x An `econcompare` object.
#' @return A data.frame of diagnostics that can be computed generically.
#' @export
eco_diagnostics <- function(x) {
  if (!inherits(x, "econcompare")) .ec_stop("`x` must be an econcompare object.")
  one <- function(model, name) {
    resid <- tryCatch(stats::residuals(model), error = function(e) numeric())
    fitted <- tryCatch(stats::fitted(model), error = function(e) numeric())
    n <- tryCatch(stats::nobs(model), error = function(e) NA_integer_)
    sigma <- if (length(resid)) stats::sd(resid, na.rm = TRUE) else NA_real_
    cor_abs <- if (length(resid) > 2 && length(fitted) == length(resid)) suppressWarnings(stats::cor(abs(resid), fitted, use = "complete.obs")) else NA_real_
    data.frame(model = name,
      engine = if (!is.null(x$meta[[name]]$engine)) x$meta[[name]]$engine else name,
      nobs = n, residual_sd = sigma, cor_abs_resid_fitted = cor_abs,
      stringsAsFactors = FALSE)
  }
  out <- do.call(rbind, Map(one, x$models, names(x$models))); rownames(out) <- NULL; out
}
