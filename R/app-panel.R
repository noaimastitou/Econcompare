# Presentation only: retain the original model objects and exported tables.
.ec_panel_display_models <- function(x) {
  registry <- eco_panel_models()
  labels <- registry$estimator[match(x, registry$engine)]
  labels[is.na(labels)] <- x[is.na(labels)]
  labels
}

.ec_panel_table_ui <- function(data, empty = "No records to display.") {
  if (!is.data.frame(data) || !nrow(data)) return(shiny::p(class = "ec-note", empty))
  show <- data
  if ("model" %in% names(show)) show$model <- .ec_panel_display_models(show$model)
  numbers <- vapply(show, is.numeric, logical(1))
  show[numbers] <- lapply(show[numbers], .ec_fmt)
  counts <- intersect(c("nobs", "groups", "inference_df", "df1", "df2", "row", "n_original", "n_used", "n_effective", "n_dropped", "observations", "periods", "internal_missing_periods"), names(data))
  for (nm in counts) show[[nm]] <- ifelse(is.na(data[[nm]]), "NA", formatC(data[[nm]], digits = 0, format = "f"))
  if ("inference_df" %in% names(data)) show$inference_df[is.infinite(data$inference_df)] <- "Normal z (no t df)"
  if ("p.value" %in% names(data)) show$p.value <- .ec_fmt_p(data$p.value)
  if ("raw_p.value" %in% names(data)) {
    show$raw_p.value <- .ec_fmt_p(data$raw_p.value)
    names(show)[names(show) == "raw_p.value"] <- "Raw engine p (audit only)"
  }
  if ("reason_code" %in% names(data)) {
    labels <- c(available = "Available", not_implemented = "Not implemented", not_applicable = "Not applicable", missing_models = "Required models absent", calibration_unverified = "Calibration unverified", calculation_failed = "Calculation failed")
    show$reason_code <- unname(labels[data$reason_code])
    names(show)[names(show) == "reason_code"] <- "Availability / calibration"
  }
  names(show) <- gsub("_", " ", names(show), fixed = TRUE)
  shiny::div(class = "ec-table-wrap ec-table ec-panel-table", tabindex = "0",
    role = "region", `aria-label` = "Panel results table; scroll horizontally for additional columns",
    shiny::HTML(.ec_html_table(show)))
}

.ec_panel_summary_ui <- function(audit) {
  s <- audit$summary
  labels <- c(observations = "Observations", individuals = "Individuals",
    periods = "Periods", balanced = "Same observed periods", index_valid = "Valid unique index", duplicate_rows = "Rows with duplicate keys",
    missing_id = "Missing individual IDs", missing_time = "Missing periods",
    singletons = "Singleton individuals", frequency = "Frequency", calendar_verified = "Calendar verified")
  shiny::div(class = "ec-profile-list ec-panel-summary",
    lapply(names(labels), function(k) {
      value <- if (k %in% names(s)) s[[k]][1L] else NA
      value <- if (is.na(value)) "Unavailable" else if (is.logical(value)) {
        if (value) "Yes" else "No"
      } else as.character(value)
      shiny::div(class = "ec-profile-row", shiny::tags$span(labels[[k]]), shiny::tags$b(value))
    }))
}

.ec_panel_results_ui <- function(fit) {
  cmp <- eco_compare(fit)
  coef <- cmp
  if ("term_label" %in% names(coef)) coef$term <- coef$term_label
  coef$model <- .ec_panel_display_models(coef$model)
  sample <- eco_sample_audit(fit)
  sample <- sample[, intersect(c("model", "n_original", "n_used", "n_effective", "n_dropped", "observation_unit", "sample_match", "sample_warning"), names(sample)), drop = FALSE]
  excluded <- do.call(rbind, lapply(names(fit$sample_exclusions), function(nm) {
    z <- fit$sample_exclusions[[nm]]
    if (nrow(z)) data.frame(model = nm, z, stringsAsFactors = FALSE) else NULL
  }))
  statistics <- unique(cmp[, intersect(c("model", "nobs", "groups", "r2", "adj_r2", "inference_df", "reference_distribution", "coefficient_scale", "observation_unit"), names(cmp)), drop = FALSE])
  statistics <- .ec_panel_visible_metrics(statistics)
  absorbed <- cmp[cmp$term_status != "estimated", c("model", "term", "term_status"), drop = FALSE]
  maps <- lapply(names(fit$transformations), function(nm) {
    shiny::tags$details(class = "ec-raw", shiny::tags$summary(paste(.ec_panel_display_models(nm), "source membership")),
      shiny::p(class = "ec-note", "Preview: first 20 records. The complete map is retained in fit$transformations. Row numbers refer to positions in the original input data."),
      .ec_panel_table_ui(utils::head(fit$transformations[[nm]], 20)))
  })
  section <- function(title, ...) shiny::div(class = "ec-explore-card", shiny::h4(title), ...)
  shiny::div(class = "ec-panel-results",
    shiny::div(class = "ec-chip-wrap",
      shiny::tags$span(class = "ec-chip ec-chip-soft", paste(length(fit$models), "models")),
      shiny::tags$span(class = "ec-chip ec-chip-soft", paste(fit$panel_metadata$groups, "individuals")),
      shiny::tags$span(class = "ec-chip ec-chip-soft", paste(fit$panel_metadata$periods, "periods")),
      shiny::tags$span(class = "ec-chip ec-chip-soft", if (fit$inference == "cluster_id") "Clustered by individual" else "Classical inference")),
    shiny::tabsetPanel(id = "panel_results_tabs",
      shiny::tabPanel("Coefficients",
        shiny::p(class = "ec-note", "Compare coefficients side by side. Absorbed terms are unavailable, not estimated zero effects. No causal interpretation is automatic."),
        shiny::div(class = "ec-table-wrap", tabindex = "0", .ec_coef_shiny_table(coef)),
        shiny::tags$details(class = "ec-raw", shiny::tags$summary("Absorbed terms"),
          .ec_panel_table_ui(absorbed, "No terms were absorbed."))),
      shiny::tabPanel("Model fit",
        shiny::p(class = "ec-note", .ec_panel_fit_note(fit)),
        .ec_panel_table_ui(statistics),
        section("Inference by model", .ec_panel_table_ui(.ec_panel_inference_table(fit))),
        shiny::div(class = "ec-tip-box", fit$panel_metadata$covariance, shiny::tags$br(), fit$panel_metadata$note)),
      shiny::tabPanel("Panel audit",
        shiny::div(class = "ec-explore-grid",
          section("Input data", .ec_panel_summary_ui(fit$panel_audit)),
          section("After common missing-value exclusions", .ec_panel_summary_ui(fit$estimation_audit))),
        section("Observations by individual",
          shiny::p(class = "ec-note", "Date labels refer to the input time index. Fixed-effect singleton exclusions are listed in Sample audit."),
          .ec_panel_table_ui(fit$estimation_audit$by_individual[, setdiff(names(fit$estimation_audit$by_individual), c("first_period", "last_period")), drop = FALSE]))),
      shiny::tabPanel("Sample audit",
        section("Estimation samples",
          shiny::p(class = "ec-note", "n used counts contributing source rows; n effective counts estimated observations: periods, consecutive differences or individual means. A smaller transformed sample is not necessarily missing data."),
          .ec_panel_table_ui(sample)),
        maps,
        section("Missing-value exclusions", .ec_panel_table_ui(fit$excluded_rows, "No observations excluded for missing model values.")),
        section("Estimator-specific source exclusions", .ec_panel_table_ui(excluded, "All complete source rows contribute to the fitted estimators."))),
      shiny::tabPanel("Diagnostics",
        shiny::selectInput("panel_diag_model", "Model to diagnose",
          choices = stats::setNames(names(fit$models), .ec_panel_display_models(names(fit$models)))),
        shiny::uiOutput("panel_diag_controls"),
        shiny::uiOutput("panel_diag_table")),
      shiny::tabPanel("Warnings / failures",
        section("Methodological information", .ec_panel_table_ui(fit$information, "No additional methodological information.")),
        section("Estimation warnings", .ec_panel_table_ui(fit$warnings, "No estimation warnings for this run.")),
        section("Estimation failures", .ec_panel_table_ui(fit$failures, "No estimation failures for this run.")))
    ))
}
