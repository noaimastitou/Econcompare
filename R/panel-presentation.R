# Pure presentation helpers: do not fit models or select an estimator.
.ec_panel_test_catalogue <- function() {
  c(effects_f = "Fixed effects vs pooled (classical F)", effects_lm = "Individual effects (classical LM)",
    serial = "Serial correlation (panel BG)", dependence = "Cross-sectional dependence (Pesaran CD)",
    hausman = "FE vs RE (Hausman)", mundlak = "Added individual means (Mundlak Wald)",
    iv_first_stage = "Excluded-instrument relevance (first stages)")
}

.ec_panel_capabilities <- function(x) {
  ids <- names(x$models); out <- list()
  add <- function(test, model, reason = "", audit_only = FALSE) {
    out[[length(out) + 1L]] <<- data.frame(test = test, model = model,
      selectable = !nzchar(reason), audit_only = audit_only, reason = reason)
  }
  for (nm in ids) {
    md <- x$meta[[nm]]
    linear <- inherits(x$models[[nm]], "plm") && nm != "panel_fe_iv"
    a <- x$model_audits[[nm]]
    serial_reason <- if (!linear) {
      if (nm == "panel_between") "Not applicable: individual means have no residual time series." else "Not implemented for this estimator. Error dependence has not been assessed."
    } else if (!isTRUE(a$summary$calendar_verified) || anyNA(a$by_individual$internal_missing_periods) || any(a$by_individual$internal_missing_periods > 0)) {
      "Not applicable: consecutive periods must be verified."
    } else if (min(a$by_individual$periods) <= 2L) "Not applicable: insufficient periods even for order 1." else ""
    add("serial", nm, serial_reason)
    cd_reason <- if (!linear) {
      if (nm == "panel_between") "Not applicable: individual means have no period-indexed residuals." else "Not implemented for this estimator. Cross-sectional dependence has not been assessed."
    } else if (min(a$by_individual$periods) < 3L) "Not applicable: at least three periods per individual are required." else ""
    add("dependence", nm, cd_reason, audit_only = linear && md$effect %in% c("time", "twoways"))
    if (md$model == "within") add("effects_f", nm,
      if (!"panel_pooling" %in% ids) "Required model absent: pooled OLS."
      else if (x$inference != "classical") "Not applicable to this inference choice: classical F only."
      else if (!identical(x$sample_rows[[nm]], x$sample_rows$panel_pooling)) "Not applicable: estimation samples differ." else "")
    if (nm == "panel_pooling") add("effects_lm", nm, if (x$inference != "classical") "Not applicable to this inference choice: classical LM only." else "")
  }
  if (any(c("panel_fe_individual", "panel_re") %in% ids)) {
    reason <- if (!all(c("panel_fe_individual", "panel_re") %in% ids)) "Required models absent: fit both individual FE and RE."
    else if (!identical(x$sample_rows$panel_fe_individual, x$sample_rows$panel_re)) "Not applicable: FE/RE samples differ."
    else if (x$inference != "classical" && !setequal(names(stats::coef(x$models$panel_fe_individual)), setdiff(names(stats::coef(x$models$panel_re)), "(Intercept)"))) "Not applicable: robust auxiliary Hausman needs matching slope sets." else ""
    add("hausman", "panel_fe_individual vs panel_re", reason)
  }
  if ("panel_mundlak" %in% ids) add("mundlak", "panel_mundlak")
  if ("panel_fe_iv" %in% ids) add("iv_first_stage", "panel_fe_iv")
  do.call(rbind, out)
}

.ec_panel_inference_table <- function(x) {
  do.call(rbind, lapply(names(x$meta), function(nm) {
    m <- x$meta[[nm]]
    data.frame(model = nm, inference = m$inference, covariance = m$covariance_note,
      reference_distribution = m$reference_distribution, inference_df = m$inference_df,
      groups = m$groups, stringsAsFactors = FALSE)
  }))
}

.ec_panel_fit_note <- function(x) {
  nonlinear <- all(names(x$models) %in% c("panel_clogit", "panel_poisson", "panel_fe_iv"))
  if (nonlinear) "Fit metrics withheld for these estimators are omitted from this display. Coefficient scales, inference and retained observations are reported explicitly."
  else "Reported R-squared describes each estimator's transformed equation; it does not rank models. Unavailable fit metrics are omitted."
}

.ec_panel_visible_metrics <- function(x) {
  metric <- intersect(c("r2", "adj_r2", "logLik", "aic", "bic", "deviance"), names(x))
  remove <- metric[vapply(x[metric], function(v) all(is.na(v)), logical(1))]
  x[, setdiff(names(x), remove), drop = FALSE]
}

.ec_panel_diagnostics_ui <- function(d) {
  main <- d[, intersect(c("model", "test", "status", "reason_code", "inference", "inference_df", "statistic", "p.value", "parameters", "interpretation", "note"), names(d)), drop = FALSE]
  technical <- d[, setdiff(names(d), "raw_p.value"), drop = FALSE]
  audit <- d[d$status == "computed_uninterpreted", intersect(c("model", "test", "statistic", "raw_p.value", "note"), names(d)), drop = FALSE]
  shiny::tagList(
    .ec_panel_table_ui(main),
    shiny::tags$details(class = "ec-raw", shiny::tags$summary("Hypotheses and inference details"), .ec_panel_table_ui(technical)),
    if (nrow(audit)) shiny::tags$details(class = "ec-raw", shiny::tags$summary("Raw engine values — audit only"),
      shiny::p(class = "ec-note", "These p-values do not support an automatic rejection or non-rejection. Calibration after time effects has not been validated."), .ec_panel_table_ui(audit)))
}

.ec_reproducibility_ui <- function(x) {
  p <- x$provenance
  versions <- if (length(p$engine_versions)) data.frame(package = names(p$engine_versions), version = unname(p$engine_versions)) else data.frame()
  shiny::tags$details(class = "ec-raw", shiny::tags$summary("Reproducibility: specification and versions"),
    shiny::p(class = "ec-note", paste("econcompare", x$version, "| R", p$R_version)),
    if (!is.null(x$formula)) shiny::tags$pre(paste(deparse(x$formula), collapse = " ")),
    if (length(x$iv_specification)) shiny::p(paste("Endogenous:", paste(x$iv_specification$endogenous, collapse = ", "), "| Excluded instruments:", paste(x$iv_specification$instruments, collapse = ", "))),
    .ec_panel_table_ui(versions),
    shiny::p(class = "ec-note", "The fitted object retains model metadata, exact sample records and provenance for this run."))
}

.ec_panel_diagnostic_prompt <- function(x, model = NULL) {
  cap <- if (is.null(model)) .ec_panel_capabilities(x) else .ec_panel_model_capabilities(x, model)
  if (any(cap$selectable)) "Select tests and run diagnostics. No estimator is selected automatically."
  else "No supported diagnostic is available for these fitted models. Error dependence has not been assessed."
}

# UI routing only: keep the public batch diagnostic API unchanged.
.ec_panel_model_capabilities <- function(x, model) {
  cap <- .ec_panel_capabilities(x)
  cap[cap$model == model | (cap$test == "hausman" & model %in% c("panel_fe_individual", "panel_re")), , drop = FALSE]
}

.ec_panel_diagnostics_for_model <- function(x, model, tests, serial_order = 1L) {
  if (length(model) != 1L || is.na(model) || !model %in% names(x$models)) .ec_stop("Choose a fitted panel model.")
  cap <- .ec_panel_model_capabilities(x, model)
  allowed <- unique(cap$test[cap$selectable])
  if (!length(tests) || anyNA(tests) || any(!tests %in% allowed)) .ec_stop("Choose available tests for the selected model.")
  pieces <- lapply(unique(tests), function(test) {
    selected <- model
    if (test == "effects_f") selected <- unique(c(model, "panel_pooling"))
    if (test == "hausman") selected <- c("panel_fe_individual", "panel_re")
    one <- x
    one$models <- x$models[selected]
    eco_panel_diagnostics(one, tests = test, serial_order = serial_order)
  })
  do.call(rbind, pieces)
}
