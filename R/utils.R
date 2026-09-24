.ec_stop <- function(...) stop(..., call. = FALSE)
.ec_warn <- function(...) warning(..., call. = FALSE)

.ec_validate_data_columns <- function(data) {
  nms <- names(data)
  if (is.null(nms) || length(nms) != ncol(data) || any(is.na(nms)) || any(!nzchar(nms))) {
    .ec_stop("`data` must have a non-empty name for every column.")
  }
  if (anyDuplicated(nms)) {
    dup <- unique(nms[duplicated(nms)])
    .ec_stop("`data` must have unique column names. Duplicated name(s): ", paste(dup, collapse = ", "), ".")
  }
  invisible(TRUE)
}

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
  out <- formatC(x, digits = digits, format = "f")
  tiny <- !is.na(x) & is.finite(x) & x != 0 & abs(x) < 10^(-digits)
  out[tiny] <- formatC(x[tiny], digits = 2, format = "e")
  out[is.na(x)] <- "NA"
  out
}


.ec_version <- function() "0.14.4"

.ec_validate_generated_names <- function(x) {
  bad <- unique(x[duplicated(x)])
  if (length(bad)) .ec_stop("Generated-term name collision: ", paste(bad, collapse = ", "), ". Rename the conflicting input columns before fitting; no generated column was overwritten.")
  invisible(TRUE)
}

.ec_quote_name <- function(x) {
  x <- as.character(x)
  paste0("`", gsub("`", "\\\\`", x, fixed = TRUE), "`")
}

.ec_validate_iv_selection <- function(x, endogenous, instruments) {
  x <- unique(as.character(x))
  endogenous <- unique(as.character(endogenous))
  instruments <- unique(as.character(instruments))
  if (!length(x)) .ec_stop("Select at least one explanatory variable.")
  if (!length(endogenous)) .ec_stop("IV/2SLS requires at least one endogenous regressor.")
  if (!length(instruments)) .ec_stop("IV/2SLS requires at least one excluded instrument.")
  bad_endog <- setdiff(endogenous, x)
  if (length(bad_endog)) {
    .ec_stop("IV/2SLS endogenous regressors must also appear in the structural equation: ", paste(bad_endog, collapse = ", "), ".")
  }
  overlap <- intersect(instruments, x)
  if (length(overlap)) {
    .ec_stop("IV/2SLS excluded instruments must not also be structural regressors: ", paste(overlap, collapse = ", "), ".")
  }
  if (length(instruments) < length(endogenous)) {
    .ec_stop(
      "IV/2SLS fails the minimum order condition: at least one excluded instrument is required per endogenous regressor. ",
      "Selected ", length(endogenous), " endogenous regressor(s) but only ", length(instruments), " excluded instrument(s)."
    )
  }
  invisible(TRUE)
}

.ec_iv_formula <- function(y, x, endogenous, instruments) {
  .ec_validate_iv_selection(x, endogenous, instruments)
  if (as.character(y)[1L] %in% instruments) .ec_stop("The dependent variable cannot be used as an excluded IV instrument.")
  rhs_struct <- paste(.ec_quote_name(x), collapse = " + ")
  exogenous <- setdiff(x, endogenous)
  rhs_inst <- unique(c(exogenous, instruments))
  if (!length(rhs_inst)) .ec_stop("IV/2SLS requires at least one instrument on the first-stage side.")
  rhs_inst <- paste(.ec_quote_name(rhs_inst), collapse = " + ")
  stats::as.formula(paste(.ec_quote_name(y), "~", rhs_struct, "|", rhs_inst))
}

.ec_selection_regressor_choices <- function(all_vars, outcome, selection_y = NULL) {
  setdiff(unique(as.character(all_vars)), unique(c(as.character(outcome), as.character(selection_y))))
}

.ec_validate_heckman_selection <- function(outcome, selection_y, selection_x, outcome_x = NULL) {
  if (!length(selection_y) || !nzchar(as.character(selection_y)[1L])) {
    .ec_stop("Heckman requires a selection indicator.")
  }
  outcome <- as.character(outcome)[1L]
  selection_y <- as.character(selection_y)[1L]
  selection_x <- unique(as.character(selection_x))
  outcome_x <- unique(as.character(outcome_x))
  if (!length(selection_x)) .ec_stop("Heckman requires at least one selection regressor.")
  if (identical(selection_y, outcome)) {
    .ec_stop("The Heckman selection indicator must be different from the outcome variable.")
  }
  if (selection_y %in% selection_x) {
    .ec_stop("The Heckman selection indicator cannot also be used as its own selection regressor.")
  }
  if (outcome %in% selection_x) {
    .ec_stop("The outcome variable cannot be used as a Heckman selection regressor.")
  }
  if (length(outcome_x) && selection_y %in% outcome_x) {
    .ec_stop("The Heckman selection indicator cannot be used as an outcome-equation regressor because the observed outcome sample is conditional on selection.")
  }
  invisible(TRUE)
}

.ec_validate_heckman_formulas <- function(selection_formula, outcome_formula) {
  if (!inherits(selection_formula, "formula") || !inherits(outcome_formula, "formula")) {
    .ec_stop("Heckman selection and outcome equations must both be formulas.")
  }
  simple_response <- function(f, label) {
    lhs <- f[[2L]]
    if (!is.symbol(lhs)) .ec_stop("Heckman ", label, " equation requires a simple response variable on the left-hand side.")
    as.character(lhs)
  }
  selection_y <- simple_response(selection_formula, "selection")
  outcome_y <- simple_response(outcome_formula, "outcome")
  selection_x <- unique(all.vars(selection_formula[[3L]]))
  outcome_x <- unique(all.vars(outcome_formula[[3L]]))
  if (!length(selection_x)) .ec_stop("Heckman requires at least one selection regressor.")
  .ec_validate_heckman_selection(outcome_y, selection_y, selection_x, outcome_x = outcome_x)
  invisible(TRUE)
}

.ec_fmt_p <- function(x) {
  out <- .ec_fmt(x)
  out[!is.na(x) & x == 0] <- "<0.0001"
  out
}

.ec_provenance <- function(x) {
  packages <- intersect(c("stats", "survival", "plm", "vars", "urca", "lmtest", "sandwich", "estimatr",
    "fixest", "MASS", "quantreg", "ivreg", "nnet", "censReg", "sampleSelection"), loadedNamespaces())
  list(econcompare_version = .ec_version(), R_version = as.character(getRversion()),
    engine_versions = stats::setNames(vapply(packages, function(p) as.character(utils::packageVersion(p)), character(1)), packages),
    requested_formula = x$formula,
    fitted_formulas = lapply(x$models, function(m) tryCatch(stats::formula(m), error = function(e) NULL)),
    specifications = x$meta, sample_info = x$sample_info,
    note = "Versions refer to the source implementation and loaded engines at estimation time. Raw model objects retain engine-specific parameters.")
}
