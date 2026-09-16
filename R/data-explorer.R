.ec_simple_type <- function(x) {
  nonmiss <- x[!is.na(x)]
  u <- unique(nonmiss)
  if (inherits(x, "Date") || inherits(x, "POSIXt")) return("Date / time")
  if (is.logical(x)) return("Binary")
  if (is.factor(x) || is.character(x)) {
    if (length(u) == 2L) return("Binary / categorical")
    if (is.ordered(x)) return("Ordinal categorical")
    return("Categorical")
  }
  if (is.numeric(x) || is.integer(x)) {
    if (.ec_is_binary_indicator(x)) return("Binary / numeric (0/1)")
    if (length(u) == 2L) return("Numeric (two observed values)")
    return("Numeric")
  }
  class(x)[1L]
}


.ec_relationship_status <- function(x, y) {
  if (!(is.numeric(x) || is.integer(x)) || !(is.numeric(y) || is.integer(y))) {
    return(list(ok = FALSE, n = 0L, message = "Both variables must be numeric."))
  }
  xx <- suppressWarnings(as.numeric(x)); yy <- suppressWarnings(as.numeric(y))
  ok <- is.finite(xx) & is.finite(yy)
  n <- sum(ok)
  if (n < 3L) {
    return(list(ok = FALSE, n = n, message = "Correlation is unavailable because fewer than three complete finite pairs are available."))
  }
  bad <- character()
  if (length(unique(xx[ok])) < 2L) bad <- c(bad, "X")
  if (length(unique(yy[ok])) < 2L) bad <- c(bad, "Y")
  if (length(bad)) {
    return(list(
      ok = FALSE, n = n,
      message = paste0(
        "Correlation is undefined because ",
        if (length(bad) == 2L) "both selected variables have zero variance" else paste0(bad, " has zero variance"),
        " among the complete finite pairs."
      )
    ))
  }
  list(ok = TRUE, n = n, message = "", x = xx[ok], y = yy[ok])
}

.ec_data_overview <- function(data) {
  missing_cells <- sum(is.na(data))
  complete_rows <- sum(stats::complete.cases(data))
  duplicate_rows <- sum(duplicated(data))
  constant_vars <- names(data)[vapply(data, function(x) {
    u <- unique(x[!is.na(x)])
    length(u) <= 1L
  }, logical(1))]
  data.frame(
    metric = c("Rows", "Variables", "Complete rows", "Missing cells", "Duplicate rows", "Constant variables"),
    value = c(nrow(data), ncol(data), complete_rows, missing_cells, duplicate_rows,
              if (length(constant_vars)) paste(constant_vars, collapse = ", ") else "None"),
    stringsAsFactors = FALSE
  )
}

.ec_category_profile <- function(x) {
  nonmiss <- x[!is.na(x)]
  if (!length(nonmiss)) return(data.frame())
  if (is.logical(nonmiss)) nonmiss <- factor(nonmiss, levels = c(FALSE, TRUE))
  if (is.factor(nonmiss)) nonmiss <- droplevels(nonmiss)
  z <- table(nonmiss, useNA = "no")
  total <- sum(z)
  data.frame(
    category = names(z),
    count = as.integer(z),
    share = as.numeric(z) / total,
    stringsAsFactors = FALSE
  )
}

.ec_binary_profile <- function(x) {
  if (!.ec_is_binary_indicator(x)) return(data.frame())
  z <- .ec_category_profile(x)
  if (!nrow(z)) return(z)
  z$share_label <- paste0(format(round(100 * z$share, 1), trim = TRUE), "%")
  z
}

.ec_variable_profile <- function(data, variable) {
  if (!variable %in% names(data)) return(data.frame())
  x <- data[[variable]]
  nonmiss <- x[!is.na(x)]
  n_nonmiss <- length(nonmiss)
  n_missing <- sum(is.na(x))
  n_unique <- length(unique(nonmiss))
  suggested <- .ec_outcome_type(x)
  temporal_role <- .ec_temporal_role(x, variable)
  if (!is.null(temporal_role)) suggested <- temporal_role

  if (.ec_is_binary_indicator(x)) {
    bp <- .ec_binary_profile(x)
    balance <- if (nrow(bp)) paste(paste0(bp$category, ": ", bp$share_label), collapse = "; ") else "NA"
    return(data.frame(
      measure = c("Type", "Suggested model group", "Observed", "Missing", "Unique values", "Category balance"),
      value = c(
        .ec_simple_type(x), suggested, n_nonmiss,
        paste0(n_missing, " (", format(round(100 * n_missing / max(1, length(x)), 1), trim = TRUE), "%)"),
        n_unique, balance
      ),
      stringsAsFactors = FALSE
    ))
  }

  if (is.numeric(x) || is.integer(x)) {
    vals <- suppressWarnings(as.numeric(nonmiss))
    q <- if (length(vals)) stats::quantile(vals, probs = c(.25, .75), names = FALSE, na.rm = TRUE) else c(NA_real_, NA_real_)
    return(data.frame(
      measure = c("Type", "Suggested model group", "Observed", "Missing", "Unique values", "Mean", "Median", "Std. deviation", "Minimum", "Q1", "Q3", "Maximum"),
      value = c(
        .ec_simple_type(x), suggested, n_nonmiss,
        paste0(n_missing, " (", format(round(100 * n_missing / max(1, length(x)), 1), trim = TRUE), "%)"),
        n_unique,
        if (length(vals)) .ec_fmt(mean(vals)) else "NA",
        if (length(vals)) .ec_fmt(stats::median(vals)) else "NA",
        if (length(vals) > 1L) .ec_fmt(stats::sd(vals)) else "NA",
        if (length(vals)) .ec_fmt(min(vals)) else "NA",
        .ec_fmt(q[1L]), .ec_fmt(q[2L]),
        if (length(vals)) .ec_fmt(max(vals)) else "NA"
      ),
      stringsAsFactors = FALSE
    ))
  }

  cp <- .ec_category_profile(x)
  top <- if (nrow(cp)) cp$category[which.max(cp$count)] else "NA"
  share <- if (nrow(cp)) max(cp$share) else NA_real_
  data.frame(
    measure = c("Type", "Suggested model group", "Observed", "Missing", "Unique values", "Most frequent value", "Share of most frequent value"),
    value = c(
      .ec_simple_type(x), suggested, n_nonmiss,
      paste0(n_missing, " (", format(round(100 * n_missing / max(1, length(x)), 1), trim = TRUE), "%)"),
      n_unique, top,
      if (is.finite(share)) paste0(format(round(100 * share, 1), trim = TRUE), "%") else "NA"
    ),
    stringsAsFactors = FALSE
  )
}

.ec_category_counts <- function(x, max_levels = 12L) {
  z <- .ec_category_profile(x)
  if (!nrow(z)) return(data.frame())
  z <- z[order(z$count, decreasing = TRUE), , drop = FALSE]
  utils::head(z, max_levels)
}

.ec_data_notices <- function(data) {
  msgs <- character()
  missing_n <- sum(is.na(data))
  if (missing_n > 0L) {
    msgs <- c(msgs, paste0(missing_n, " missing value(s) detected. Models may use fewer observations when selected variables contain missing values."))
  }
  dup <- sum(duplicated(data))
  if (dup > 0L) msgs <- c(msgs, paste0(dup, " duplicated row(s) detected. Check whether duplicates are expected before modelling."))
  constant <- names(data)[vapply(data, function(x) length(unique(x[!is.na(x)])) <= 1L, logical(1))]
  if (length(constant)) msgs <- c(msgs, paste0("Constant variable(s): ", paste(constant, collapse = ", "), ". They cannot explain variation in an outcome."))
  nonfinite <- names(data)[vapply(data, function(x) is.numeric(x) && any(!is.finite(x) & !is.na(x)), logical(1))]
  if (length(nonfinite)) msgs <- c(msgs, paste0("Non-finite numeric value(s) detected in: ", paste(nonfinite, collapse = ", "), ". Review these values before estimation."))
  if (!length(msgs)) msgs <- "No basic data-quality issue was detected by the simple pre-model checks. This is not a guarantee that the data are suitable for a particular econometric specification."
  msgs
}

.ec_all_variable_summary <- function(data) {
  rows <- lapply(names(data), function(nm) {
    x <- data[[nm]]
    nonmiss <- x[!is.na(x)]
    n_obs <- length(nonmiss)
    n_miss <- sum(is.na(x))
    n_unique <- length(unique(nonmiss))
    temporal_role <- .ec_temporal_role(x, nm)
    if (!is.null(temporal_role)) {
      ptime <- tryCatch(.ec_parse_time_vector(x, nm), error=function(e) NULL)
      first <- if (!is.null(ptime) && any(is.finite(ptime$parsed))) ptime$display[which.min(replace(ptime$parsed,!is.finite(ptime$parsed),Inf))] else if (length(nonmiss)) as.character(nonmiss[1L]) else "NA"
      return(data.frame(variable=nm, type=.ec_simple_type(x), `model group`=temporal_role, observed=n_obs, missing=n_miss, unique=n_unique, `Mean / top`=first, `Top share`="—", minimum="—", maximum="—", stringsAsFactors=FALSE, check.names=FALSE))
    }
    if (.ec_is_binary_indicator(x)) {
      bp <- .ec_binary_profile(x)
      top <- if (nrow(bp)) bp$category[which.max(bp$count)] else "NA"
      top_share <- if (nrow(bp)) paste0(format(round(100 * max(bp$share), 1), trim = TRUE), "%") else "NA"
      return(data.frame(
        variable = nm, type = .ec_simple_type(x), `model group` = .ec_outcome_type(x),
        observed = n_obs, missing = n_miss, unique = n_unique,
        `Mean / top` = top, `Top share` = top_share,
        minimum = if (is.numeric(x)) .ec_fmt(min(as.numeric(nonmiss))) else "—",
        maximum = if (is.numeric(x)) .ec_fmt(max(as.numeric(nonmiss))) else "—",
        stringsAsFactors = FALSE, check.names = FALSE
      ))
    }
    if (is.numeric(x) || is.integer(x)) {
      vals <- suppressWarnings(as.numeric(nonmiss))
      data.frame(
        variable = nm, type = .ec_simple_type(x), `model group` = .ec_outcome_type(x),
        observed = n_obs, missing = n_miss, unique = n_unique,
        `Mean / top` = if (length(vals)) .ec_fmt(mean(vals)) else "NA", `Top share` = "—",
        minimum = if (length(vals)) .ec_fmt(min(vals)) else "NA",
        maximum = if (length(vals)) .ec_fmt(max(vals)) else "NA",
        stringsAsFactors = FALSE, check.names = FALSE
      )
    } else {
      cp <- .ec_category_profile(x)
      top <- if (nrow(cp)) cp$category[which.max(cp$count)] else "NA"
      share <- if (nrow(cp)) paste0(format(round(100 * max(cp$share), 1), trim = TRUE), "%") else "NA"
      data.frame(
        variable = nm, type = .ec_simple_type(x), `model group` = .ec_outcome_type(x),
        observed = n_obs, missing = n_miss, unique = n_unique,
        `Mean / top` = top, `Top share` = share,
        minimum = "—", maximum = "—",
        stringsAsFactors = FALSE, check.names = FALSE
      )
    }
  })
  do.call(rbind, rows)
}

.ec_temporal_role <- function(x, name) {
  p <- tryCatch(.ec_parse_time_vector(x, name), error=function(e) NULL)
  if (!is.null(p) && isTRUE(p$valid)) {
    dup <- anyDuplicated(p$parsed[is.finite(p$parsed)]) > 0L
    return(if (dup) "temporal component / repeated candidate" else "time index candidate")
  }
  nn <- tolower(name)
  z <- suppressWarnings(as.numeric(x[!is.na(x)]))
  if (grepl("quarter|trimestre", nn) && length(z) && all(z %in% 1:4)) return("seasonal component")
  if (grepl("month|mois", nn) && length(z) && all(z %in% 1:12)) return("seasonal component")
  NULL
}
