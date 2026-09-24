#' @export
print.econcompare <- function(x, ...) {
  cat("econcompare", if (is.null(x$version)) "" else x$version, "\n")
  if (identical(x$analysis_type, "panel")) {
    cat("Analysis: static panel; id =", x$panel_id, "; time =", x$time_variable, "\n")
    cat("Models:", paste(names(x$models), collapse = ", "), "\n")
    cat("Inference:", x$panel_metadata$covariance, "\n")
    cat("Use eco_compare(x), eco_sample_audit(x), eco_panel_diagnostics(x), and x$term_status.\n")
    return(invisible(x))
  }
  if (identical(x$analysis_type, "time_series_system")) {
    cat("Analysis: multivariate time-series system\n")
    cat("Model:", toupper(if (is.null(x$temporal_family)) "system" else x$temporal_family), "\n")
    cat("Variables:", paste(x$variables, collapse = ", "), "\n")
    cat("Time index:", x$time_variable, "\n")
    if (identical(x$temporal_family, "vecm")) cat("Cointegration rank:", x$rank, "\n")
    cat("Equation results:", paste(names(x$models), collapse = ", "), "\n")
    cat("Use eco_compare(x), eco_system_diagnostics(x), and for VECM eco_vecm_cointegration(x).\n")
    return(invisible(x))
  }
  cat("Outcome group:", if (is.null(x$outcome_type)) "unknown" else x$outcome_type, "\n")
  cat("Models:", paste(names(x$models), collapse = ", "), "\n")
  if (!is.null(x$formula)) cat("Formula:", paste(deparse(x$formula), collapse = " "), "\n")
  if (identical(x$temporal_family, "ecm")) {
    cat("Temporal family: ECM\n")
    cat("Use eco_compare(x), eco_ecm_long_run(x), eco_time_diagnostics(x), or eco_view(x).\n")
  } else {
    cat("Use eco_compare(x), eco_diagnostics(x), eco_models(), or eco_view(x).\n")
  }
  invisible(x)
}
