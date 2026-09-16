.ec_type_override_choices <- function() {
  c(
    "Automatic detection" = "auto",
    "Continuous numeric" = "continuous",
    "Binary" = "binary",
    "Nominal categorical" = "nominal",
    "Ordinal categorical" = "ordinal",
    "Time index — automatic supported parser" = "time_auto",
    "Time index — year (YYYY)" = "time_year",
    "Time index — year-month (YYYY-MM)" = "time_year_month",
    "Time index — year-quarter (YYYY-Qn)" = "time_year_quarter",
    "Time index — ISO date (YYYY-MM-DD)" = "time_date_iso"
  )
}

.ec_coerce_numeric_strict <- function(x, variable) {
  if (is.numeric(x) || is.integer(x)) return(as.numeric(x))
  z <- trimws(as.character(x))
  z[is.na(x) | z == ""] <- NA_character_
  out <- suppressWarnings(as.numeric(z))
  bad <- !is.na(z) & !is.finite(out)
  if (any(bad)) .ec_stop("Variable `", variable, "` cannot be treated as continuous numeric because some observed values are not numeric.")
  out
}

.ec_validate_ordinal_levels <- function(x, variable, levels) {
  observed <- unique(as.character(x[!is.na(x)]))
  levels <- as.character(levels)
  levels <- levels[!is.na(levels) & nzchar(levels)]
  if (!length(observed)) .ec_stop("Variable `", variable, "` has no observed category to order.")
  if (!length(levels)) .ec_stop("Manual ordinal typing for `", variable, "` requires an explicit category order in `ordinal_levels`.")
  if (anyDuplicated(levels)) .ec_stop("Manual ordinal order for `", variable, "` contains duplicated categories.")
  if (!setequal(observed, levels) || length(observed) != length(levels)) {
    .ec_stop("Manual ordinal order for `", variable, "` must contain every observed category exactly once.")
  }
  levels
}

.ec_apply_one_type <- function(x, type, variable, ordinal_levels = NULL) {
  type <- as.character(type)[1L]
  if (is.na(type) || !nzchar(type) || identical(type, "auto")) return(x)

  if (identical(type, "continuous")) return(.ec_coerce_numeric_strict(x, variable))

  if (identical(type, "binary")) {
    z <- x[!is.na(x)]
    if (length(unique(z)) != 2L) .ec_stop("Variable `", variable, "` cannot be treated as binary because it does not contain exactly two observed states.")
    if (is.numeric(x) || is.integer(x)) {
      vals <- sort(unique(as.numeric(z)))
      if (identical(vals, c(0, 1))) return(as.numeric(x))
    }
    return(factor(x))
  }

  if (identical(type, "nominal")) return(factor(x))

  if (identical(type, "ordinal")) {
    lev <- .ec_validate_ordinal_levels(x, variable, ordinal_levels)
    return(ordered(as.character(x), levels = lev))
  }

  if (identical(type, "time_auto")) {
    p <- .ec_parse_time_vector(x, variable)
    if (!p$valid) .ec_stop("Variable `", variable, "` is marked as a time index, but its values are not recognized by the supported unambiguous parsers.")
    return(x)
  }

  z <- trimws(as.character(x)); z[is.na(x) | z == ""] <- NA_character_
  if (identical(type, "time_year")) {
    if (any(!is.na(z) & !grepl("^[12][0-9]{3}$", z))) .ec_stop("Variable `", variable, "` is not valid YYYY year data.")
    return(suppressWarnings(as.integer(z)))
  }
  if (identical(type, "time_year_month")) {
    if (any(!is.na(z) & !grepl("^[12][0-9]{3}[-/](0[1-9]|1[0-2])$", z))) .ec_stop("Variable `", variable, "` is not valid YYYY-MM year-month data.")
    return(gsub("/", "-", z))
  }
  if (identical(type, "time_year_quarter")) {
    if (any(!is.na(z) & !grepl("^[12][0-9]{3}[- ]?[Qq][1-4]$", z))) .ec_stop("Variable `", variable, "` is not valid YYYY-Qn year-quarter data.")
    yy <- substr(z, 1, 4); qq <- sub(".*[Qq]", "", z)
    return(ifelse(is.na(z), NA_character_, paste0(yy, "-Q", qq)))
  }
  if (identical(type, "time_date_iso")) {
    if (any(!is.na(z) & !grepl("^[12][0-9]{3}[-/](0[1-9]|1[0-2])[-/](0[1-9]|[12][0-9]|3[01])$", z))) .ec_stop("Variable `", variable, "` is not valid ISO YYYY-MM-DD date data.")
    out <- suppressWarnings(as.Date(gsub("/", "-", z), format = "%Y-%m-%d"))
    if (any(!is.na(z) & is.na(out))) .ec_stop("Variable `", variable, "` contains invalid calendar dates.")
    return(out)
  }
  .ec_stop("Unknown manual variable type override: `", type, "`.")
}

#' Apply explicit variable-type overrides
#'
#' This helper lets researchers correct a conservative or unsuitable automatic
#' type suggestion before modelling. Overrides are explicit and validated; the
#' original object is not modified in place.
#'
#' @param data A data.frame.
#' @param overrides A named character vector or named list. Names must be data
#'   columns and values one of `auto`, `continuous`, `binary`, `nominal`,
#'   `ordinal`, `time_auto`, `time_year`, `time_year_month`,
#'   `time_year_quarter`, or `time_date_iso`.
#' @param ordinal_levels Optional named list of explicit lowest-to-highest
#'   category orders for variables manually typed as `ordinal`. Manual ordinal
#'   typing never infers category rank from labels, factor order, or row order.
#' @return A copy of `data` with validated type conversions applied. Attribute
#'   `econcompare_type_overrides` records the requested overrides.
#' @export
#' @examples
#' d <- data.frame(year = c("2020", "2021"), code = c("A", "B"))
#' eco_apply_types(d, c(year = "time_year", code = "nominal"))
#' eco_apply_types(
#'   data.frame(rating = c("low", "high", "medium")),
#'   c(rating = "ordinal"),
#'   ordinal_levels = list(rating = c("low", "medium", "high"))
#' )
eco_apply_types <- function(data, overrides, ordinal_levels = NULL) {
  if (!is.data.frame(data)) .ec_stop("`data` must be a data.frame.")
  .ec_validate_data_columns(data)
  if (is.null(overrides) || !length(overrides)) return(data)
  if (is.list(overrides)) overrides <- unlist(overrides, use.names = TRUE)
  if (is.null(names(overrides)) || any(!nzchar(names(overrides)))) .ec_stop("`overrides` must be named by variable.")
  bad <- setdiff(names(overrides), names(data))
  if (length(bad)) .ec_stop("Unknown variable override(s): ", paste(bad, collapse = ", "), ".")
  allowed <- unname(.ec_type_override_choices())
  bad_type <- setdiff(unique(as.character(overrides)), allowed)
  if (length(bad_type)) .ec_stop("Unknown type override(s): ", paste(bad_type, collapse = ", "), ".")
  if (is.null(ordinal_levels)) ordinal_levels <- list()
  if (!is.list(ordinal_levels)) .ec_stop("`ordinal_levels` must be NULL or a named list.")
  if (length(ordinal_levels) && (is.null(names(ordinal_levels)) || any(!nzchar(names(ordinal_levels))))) .ec_stop("`ordinal_levels` must be named by variable.")
  bad_levels <- setdiff(names(ordinal_levels), names(data))
  if (length(bad_levels)) .ec_stop("Unknown ordinal-level variable(s): ", paste(bad_levels, collapse = ", "), ".")
  out <- data
  used_levels <- list()
  for (nm in names(overrides)) {
    lev <- if (identical(as.character(overrides[[nm]]), "ordinal")) ordinal_levels[[nm]] else NULL
    out[[nm]] <- .ec_apply_one_type(out[[nm]], overrides[[nm]], nm, ordinal_levels = lev)
    if (identical(as.character(overrides[[nm]]), "ordinal")) used_levels[[nm]] <- levels(out[[nm]])
  }
  attr(out, "econcompare_type_overrides") <- as.character(overrides)
  attr(out, "econcompare_ordinal_levels") <- used_levels
  out
}
