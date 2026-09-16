.ec_outcome_type <- function(x) {
  y <- x[!is.na(x)]
  if (!length(y)) return("unknown")
  if (.ec_is_binary_indicator(x)) return("binary")
  if (is.ordered(x) && length(unique(y)) >= 3L) return("ordinal")
  if ((is.factor(x) || is.character(x)) && length(unique(y)) >= 3L) return("nominal")
  if (is.numeric(x) || is.integer(x)) return("continuous")
  "unknown"
}

.ec_formula_response_name <- function(formula) {
  lhs <- formula[[2L]]
  if (!is.symbol(lhs)) {
    .ec_stop("Categorical-outcome models currently require a simple response variable on the left-hand side of `formula`, e.g. `y ~ x1 + x2`.")
  }
  as.character(lhs)
}

.ec_binary_levels <- function(x) {
  y <- x[!is.na(x)]
  if (!length(y)) return(character())
  if (is.factor(x)) return(levels(droplevels(x)))
  if (is.character(x)) return(sort(unique(as.character(y))))
  if (is.logical(x)) return(c("FALSE", "TRUE"))
  if (is.numeric(x) || is.integer(x)) return(as.character(sort(unique(as.numeric(y)))))
  character()
}

.ec_validate_binary_response <- function(data, formula, binary_event = NULL) {
  yname <- .ec_formula_response_name(formula)
  if (!yname %in% names(data)) .ec_stop("Unknown response variable: `", yname, "`.")
  y <- data[[yname]]
  if (!.ec_is_binary_indicator(y)) {
    .ec_stop("Binary models require a dependent variable with exactly two observed states: logical, character, a two-level factor, or numeric coded 0/1.")
  }
  if (is.character(y) && is.null(binary_event)) {
    .ec_stop(
      "Character binary responses require an explicit `binary_event` so econcompare does not decide which category is the event. ",
      "Choose one of: ", paste(.ec_binary_levels(y), collapse = ", "), "."
    )
  }
  invisible(TRUE)
}

.ec_binary_model_data <- function(data, formula, binary_event = NULL) {
  .ec_validate_binary_response(data, formula, binary_event = binary_event)
  yname <- .ec_formula_response_name(formula)
  y <- data[[yname]]
  out <- data

  if (is.logical(y)) {
    out[[yname]] <- as.integer(y)
    coding <- "FALSE = 0; TRUE = 1"
    event <- "TRUE"
  } else if (is.factor(y)) {
    lev <- levels(droplevels(y))
    if (length(lev) != 2L) .ec_stop("Binary factor response must have exactly two observed levels.")
    event <- if (is.null(binary_event)) lev[2L] else as.character(binary_event)[1L]
    if (!event %in% lev) .ec_stop("`binary_event` must be one of the two observed factor levels: ", paste(lev, collapse = ", "), ".")
    other <- setdiff(lev, event)[1L]
    out[[yname]] <- ifelse(is.na(y), NA_real_, as.numeric(as.character(y) == event))
    coding <- paste0(other, " = 0; ", event, " = 1")
  } else if (is.character(y)) {
    lev <- sort(unique(y[!is.na(y)]))
    event <- as.character(binary_event)[1L]
    if (!event %in% lev) .ec_stop("`binary_event` must be one of the two observed character values: ", paste(lev, collapse = ", "), ".")
    other <- setdiff(lev, event)[1L]
    out[[yname]] <- ifelse(is.na(y), NA_real_, as.numeric(y == event))
    coding <- paste0(other, " = 0; ", event, " = 1")
  } else {
    vals <- sort(unique(as.numeric(y[!is.na(y)])))
    if (!identical(vals, c(0, 1))) .ec_stop("Numeric binary responses must be coded 0/1.")
    out[[yname]] <- as.numeric(y)
    coding <- "0 = 0; 1 = 1"
    event <- "1"
  }
  list(data = out, coding = coding, response = yname, event = event)
}

.ec_validate_nominal_response <- function(data, formula) {
  yname <- .ec_formula_response_name(formula)
  if (!yname %in% names(data)) .ec_stop("Unknown response variable: `", yname, "`.")
  y <- data[[yname]]
  yy <- y[!is.na(y)]
  if (is.numeric(y) || is.integer(y)) {
    .ec_stop(
      "Multinomial logit does not convert numeric outcomes into categories automatically. ",
      "If numeric values are substantive category codes, convert the response explicitly with `factor()` before calling econcompare."
    )
  }
  supported <- is.factor(y) || is.character(y)
  if (!supported || length(unique(yy)) < 3L) {
    .ec_stop("Multinomial logit requires a factor or character response with at least three observed categories.")
  }
  invisible(TRUE)
}

.ec_nominal_model_data <- function(data, formula) {
  .ec_validate_nominal_response(data, formula)
  yname <- .ec_formula_response_name(formula)
  out <- data
  out[[yname]] <- droplevels(as.factor(out[[yname]]))
  list(data = out, response = yname, levels = levels(out[[yname]]))
}

.ec_validate_ordinal_response <- function(data, formula) {
  yname <- .ec_formula_response_name(formula)
  if (!yname %in% names(data)) .ec_stop("Unknown response variable: `", yname, "`.")
  y <- data[[yname]]
  if (!is.ordered(y) || length(unique(y[!is.na(y)])) < 3L) {
    .ec_stop("Ordered logit/probit require an ordered factor response with at least three observed levels. Define the substantive category order before calling econcompare, e.g. ordered(x, levels = c(...)).")
  }
  invisible(TRUE)
}

.ec_ordinal_model_data <- function(data, formula) {
  .ec_validate_ordinal_response(data, formula)
  yname <- .ec_formula_response_name(formula)
  out <- data
  out[[yname]] <- droplevels(out[[yname]])
  if (!is.ordered(out[[yname]])) out[[yname]] <- ordered(out[[yname]], levels = levels(out[[yname]]))
  list(data = out, response = yname, levels = levels(out[[yname]]))
}

.ec_models_outcome_type <- function(models, reg = .ec_registry()) {
  types <- unique(reg$outcome_type[match(models, reg$engine)])
  types <- types[!is.na(types)]
  if (length(types) != 1L) {
    .ec_stop("Models from different outcome groups cannot be estimated in one comparison. Choose models for one objective only: continuous, binary, nominal, or ordinal.")
  }
  types
}

.ec_user_ordered <- function(x, order) {
  ord <- if (length(order) == 1L) trimws(strsplit(as.character(order), ",", fixed = TRUE)[[1L]]) else trimws(as.character(order))
  ord <- ord[nzchar(ord)]
  observed <- unique(as.character(x[!is.na(x)]))
  if (length(ord) != length(observed) || !setequal(ord, observed) || anyDuplicated(ord)) {
    .ec_stop("Ordinal category order must contain every observed outcome category exactly once.")
  }
  ordered(as.character(x), levels = ord)
}
