.ec_stop <- function(...) stop(..., call. = FALSE)
.ec_warn <- function(...) warning(..., call. = FALSE)

.ec_require <- function(pkg, feature) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    .ec_stop("Model type '", feature, "' requires package '", pkg,
             "'. Install it with install.packages(\"", pkg, "\").")
  }
}

.ec_escape_html <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

.ec_fmt <- function(x, digits = 4) {
  ifelse(is.na(x), "NA", formatC(x, digits = digits, format = "f"))
}
