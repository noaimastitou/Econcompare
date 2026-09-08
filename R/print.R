#' @export
print.econcompare <- function(x, ...) {
  cat("econcompare beta", if (is.null(x$version)) "0.1.x" else x$version, "\n")
  cat("Models:", paste(names(x$models), collapse = ", "), "\n")
  cat("Formula:", paste(deparse(x$formula), collapse = " "), "\n")
  cat("Use eco_compare(x), eco_diagnostics(x), eco_models(), or eco_view(x).\n")
  invisible(x)
}
