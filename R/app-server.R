.ec_app_server <- function(data, all_vars, numeric_vars, binary_candidates, registry, initial_y, initial_type, structure_info, initial_mode, initial_time) {
  function(input, output, session) {
    type_state <- shiny::reactiveValues(overrides = character(), ordinal_levels = list(), error = NULL)
    active_data <- shiny::reactive({
      if (!length(type_state$overrides)) return(data)
      eco_apply_types(data, type_state$overrides, ordinal_levels = type_state$ordinal_levels)
    })
    active_structure <- shiny::reactive(eco_data_structure(active_data()))
    .ec_register_data_explorer_server(input, output, active_data)

    output$variable_summary_ui <- shiny::renderUI({
      dat <- active_data()
      shiny::div(class = "ec-table-wrap ec-table", shiny::HTML(.ec_html_table(.ec_all_variable_summary(dat))))
    })

    shiny::observeEvent(input$type_override_var, {
      nm <- input$type_override_var
      current <- if (!is.null(nm) && nm %in% names(type_state$overrides)) unname(type_state$overrides[[nm]]) else "auto"
      shiny::updateSelectInput(session, "type_override_kind", selected = current)
    }, ignoreInit = TRUE)

    output$type_override_ordinal_ui <- shiny::renderUI({
      if (!identical(input$type_override_kind, "ordinal")) return(NULL)
      nm <- input$type_override_var
      if (is.null(nm) || !nzchar(nm) || !nm %in% names(data)) return(NULL)
      observed <- unique(as.character(data[[nm]][!is.na(data[[nm]])]))
      if (!length(observed)) return(shiny::div(class="ec-status ec-warn", "This variable has no observed categories to order."))
      selected <- type_state$ordinal_levels[[nm]]
      if (is.null(selected)) selected <- character()
      shiny::selectizeInput(
        "type_override_ordinal_levels",
        .ec_labeled(
          "Manual ordinal order (lowest → highest)",
          "Manual ordinal typing requires an explicit substantive order. econcompare never infers this order from labels, factor levels, numeric codes, or row appearance.",
          "Select every observed category exactly once, then drag selected labels if you need to reorder them."
        ),
        choices = stats::setNames(observed, observed),
        selected = selected,
        multiple = TRUE,
        width = "100%",
        options = list(plugins = list("remove_button", "drag_drop"), create = FALSE, persist = FALSE, placeholder = "Select lowest category first...")
      )
    })

    shiny::observeEvent(input$apply_type_override, {
      nm <- input$type_override_var; tp <- input$type_override_kind
      if (is.null(nm) || !nzchar(nm)) return()
      type_state$error <- NULL
      candidate <- type_state$overrides
      candidate_levels <- type_state$ordinal_levels
      if (identical(tp, "auto")) {
        candidate <- candidate[names(candidate) != nm]
        candidate_levels[[nm]] <- NULL
      } else {
        candidate[nm] <- tp
        if (identical(tp, "ordinal")) {
          candidate_levels[[nm]] <- if (is.null(input$type_override_ordinal_levels)) character() else input$type_override_ordinal_levels
        } else {
          candidate_levels[[nm]] <- NULL
        }
      }
      ans <- tryCatch(eco_apply_types(data, candidate, ordinal_levels = candidate_levels), error = function(e) e)
      if (inherits(ans, "error")) {
        type_state$error <- conditionMessage(ans)
      } else {
        type_state$overrides <- candidate
        type_state$ordinal_levels <- candidate_levels
        if (!is.null(input$y) && input$y %in% names(ans)) {
          detected_now <- .ec_outcome_type(ans[[input$y]])
          if (detected_now %in% c("continuous","binary","nominal","ordinal")) shiny::updateSelectInput(session, "outcome_type", selected=detected_now)
        }
      }
    })
    shiny::observeEvent(input$clear_type_overrides, { type_state$overrides <- character(); type_state$ordinal_levels <- list(); type_state$error <- NULL })
    output$type_override_status <- shiny::renderUI({
      if (!is.null(type_state$error)) return(shiny::div(class="ec-status ec-error", type_state$error))
      if (!length(type_state$overrides)) return(shiny::p(class="ec-field-caption", "No manual type override is active."))
      details <- paste0(names(type_state$overrides), " = ", unname(type_state$overrides))
      ord_names <- intersect(names(type_state$ordinal_levels), names(type_state$overrides))
      if (length(ord_names)) {
        details <- c(details, vapply(ord_names, function(nm) paste0(nm, " order: ", paste(type_state$ordinal_levels[[nm]], collapse=" < ")), character(1)))
      }
      shiny::div(class="ec-status ec-warn", paste0("Manual overrides: ", paste(details, collapse = "; "), ". These overrides affect exploration and estimation in this app session."))
    })

    output$structure_hint <- shiny::renderUI({
      si <- active_structure()
      label <- switch(si$structure,
        time_series = "time series",
        panel_candidate = "possible panel (confirm indexes)",
        cross_section = "cross-sectional",
        ambiguous_temporal = "ambiguous temporal structure",
        si$structure
      )
      cls <- if (identical(si$structure, "ambiguous_temporal")) "ec-status ec-warn" else "ec-field-caption"
      shiny::div(class = cls,
        paste0("Detected structure: ", label, " (confidence: ", si$confidence, "). ", si$reason,
               " Detection is advisory; the econometric mode remains your explicit choice."))
    })

    output$time_index_ui <- shiny::renderUI({
      if (!identical(input$analysis_mode, "time_series")) return(NULL)
      si <- active_structure(); candidates <- si$candidates$variable
      choices <- unique(c(candidates, all_vars))
      current_time <- shiny::isolate(input$time_var)
      selected <- if (!is.null(current_time) && nzchar(current_time) && current_time %in% choices) current_time else if (nzchar(initial_time) && initial_time %in% choices) initial_time else if (length(candidates)) candidates[1L] else ""
      shiny::tagList(
        shiny::selectInput(
          "time_var",
          .ec_labeled(
            "Time index",
            "Choose the variable that identifies observation time. econcompare validates this column conservatively, sorts the estimation copy by time, rejects missing or duplicated time points for single-series analysis, and never uses the index as an ordinary regressor.",
            "Preferred forms: Date/POSIXct, four-digit year, YYYY-MM, YYYY-Q1, or ISO YYYY-MM-DD. Ambiguous locale dates such as 01/02/2020 are not guessed automatically."
          ),
          choices = c("Choose time index..." = "", stats::setNames(choices, choices)),
          selected = selected
        ),
        shiny::uiOutput("time_audit_ui")
      )
    })


    output$time_trajectory_plot <- shiny::renderPlot({
      shiny::req(input$analysis_mode, input$time_var, input$y)
      if (!identical(input$analysis_mode, "time_series") || !nzchar(input$time_var)) return(invisible(NULL))
      prep <- tryCatch(.ec_prepare_time_data(active_data(), input$time_var), error=function(e) NULL)
      if (is.null(prep)) return(invisible(NULL))
      vars <- unique(c(input$y, if (is.null(input$x)) character() else input$x))
      vars <- vars[vars %in% names(prep$data) & vapply(prep$data[vars], is.numeric, logical(1))]
      if (!length(vars)) return(invisible(NULL))
      vars <- head(vars, 6L)
      mat <- sapply(vars, function(v) {
        z <- prep$data[[v]]
        sdv <- stats::sd(z, na.rm=TRUE)
        if (!is.finite(sdv) || sdv == 0) return(rep(NA_real_, length(z)))
        (z - mean(z, na.rm=TRUE)) / sdv
      })
      if (is.vector(mat)) mat <- matrix(mat, ncol=1L, dimnames=list(NULL, vars))
      xseq <- prep$parsed
      yr <- range(mat, na.rm=TRUE)
      if (!all(is.finite(yr)) || !all(is.finite(xseq))) return(invisible(NULL))
      graphics::matplot(xseq, mat, type="l", lty=1, xaxt="n", xlab=paste0("Time (", prep$audit$frequency, ")"), ylab="Standardized value", main="Standardized temporal trajectories")
      at_i <- unique(round(seq(1, length(xseq), length.out=min(7L,length(xseq)))))
      graphics::axis(1, at=xseq[at_i], labels=prep$display[at_i], las=2, cex.axis=.75)
      graphics::legend("topleft", legend=colnames(mat), lty=1, col=seq_len(ncol(mat)), bty="n", cex=.8)
    })

    output$time_audit_ui <- shiny::renderUI({
      if (!identical(input$analysis_mode, "time_series") || is.null(input$time_var) || !nzchar(input$time_var)) return(NULL)
      aud <- tryCatch(eco_time_audit(active_data(), input$time_var), error = function(e) e)
      if (inherits(aud, "error")) return(shiny::div(class = "ec-status ec-error", conditionMessage(aud)))
      show <- aud
      for (nm in names(show)) if (is.logical(show[[nm]])) show[[nm]] <- ifelse(is.na(show[[nm]]), "NA", ifelse(show[[nm]], "Yes", "No"))
      shiny::tagList(
        shiny::p(class = "ec-field-caption", paste0("Time audit: ", aud$frequency, "; ", aud$unique_time, " unique time points; ", aud$duplicate_time, " duplicate(s); ", aud$missing_time, " missing time value(s).")),
        if (aud$duplicate_time > 0L) shiny::div(class = "ec-status ec-warn", "Repeated time points are not treated as a single time series. They may indicate panel data: choose Panel data econometrics and explicitly select individual/time indexes."),
        if (!isTRUE(aud$regular_spacing) || (!is.na(aud$missing_periods) && aud$missing_periods > 0L)) shiny::div(class="ec-status ec-warn", "The time grid is irregular or contains missing periods. Lagged models are blocked because an observation lag would not necessarily equal one calendar-period lag."),
        if (!isTRUE(aud$sorted_ascending)) shiny::div(class = "ec-field-caption", "Rows are not currently sorted by time. econcompare will sort an internal estimation copy; your original data are not modified.")
      )
    })

    shiny::observeEvent(list(input$y, input$analysis_mode, input$time_var, input$panel_id, input$panel_time), {
      excluded <- input$y
      if (identical(input$analysis_mode, "panel")) excluded <- c(excluded, input$panel_id, input$panel_time)
      if (identical(input$analysis_mode, "time_series") && !is.null(input$time_var) && nzchar(input$time_var)) excluded <- c(excluded, input$time_var)
      shiny::updateSelectizeInput(session, "x", choices = setdiff(all_vars, excluded), server = TRUE)
      dat <- active_data(); detected <- .ec_outcome_type(dat[[input$y]])
      if (detected %in% c("continuous", "binary", "nominal", "ordinal")) {
        shiny::updateSelectInput(session, "outcome_type", selected = detected)
      }
    }, ignoreInit = FALSE)
  
  
    output$outcome_hint <- shiny::renderUI({
      shiny::req(input$y)
      dat <- active_data(); detected <- .ec_outcome_type(dat[[input$y]])
      label <- switch(detected, continuous = "continuous", binary = "binary", nominal = "nominal categorical", ordinal = "ordinal categorical", "ambiguous")
      dat <- active_data(); x <- dat[[input$y]]
      extra <- NULL
      if (identical(input$outcome_type, "nominal") && (is.numeric(x) || is.integer(x))) {
        extra <- shiny::div(class = "ec-status ec-warn",
          "This outcome is stored as numeric. econcompare will not convert numeric values into nominal categories automatically. If the numbers are category codes, convert the variable to factor() before launching the app."
        )
      }
      shiny::tagList(
        shiny::p(class = "ec-field-caption", paste0("Suggested from the data: ", label, ". The suggestion is descriptive; your selection in Analysis objective is the modelling decision.")),
        extra
      )
    })
  
    output$binary_event_ui <- shiny::renderUI({
      shiny::req(input$y, input$outcome_type)
      if (!identical(input$outcome_type, "binary")) return(NULL)
      dat <- active_data(); x <- dat[[input$y]]
      if (!.ec_is_binary_indicator(x)) return(NULL)
      lev <- .ec_binary_levels(x)
      if (is.logical(x) || ((is.numeric(x) || is.integer(x)) && identical(as.numeric(lev), c(0, 1)))) {
        return(shiny::p(class = "ec-field-caption", "Binary event is unambiguous: TRUE or 1 is modelled as the event."))
      }
      selected <- if (is.factor(x)) tail(levels(droplevels(x)), 1L) else ""
      choices <- if (is.character(x)) c("Choose event..." = "", stats::setNames(lev, lev)) else lev
      shiny::selectInput(
        "binary_event",
        .ec_labeled(
          "Binary event (coded 1)",
          "For a two-category outcome, you must decide which category is the event whose probability is modelled. econcompare does not make that substantive choice silently.",
          "For factors, the existing factor-level order is shown as the default; for character data you must choose explicitly."
        ),
        choices = choices,
        selected = selected
      )
    })
  
    output$ordinal_order_ui <- shiny::renderUI({
      shiny::req(input$y, input$outcome_type)
      if (!identical(input$outcome_type, "ordinal")) return(NULL)

      dat <- active_data(); x <- dat[[input$y]]
      observed <- unique(as.character(x[!is.na(x)]))
      if (!length(observed)) return(NULL)

      # Existing ordered-factor levels are researcher-supplied information, so they
      # can safely pre-populate the control. Otherwise econcompare leaves the order
      # empty and asks the researcher to define it explicitly.
      selected <- if (is.ordered(x)) levels(droplevels(x)) else character()
      choice_values <- if (is.ordered(x)) levels(droplevels(x)) else observed
      counts <- .ec_category_profile(as.character(x))
      counts$share <- paste0(format(round(100 * counts$share, 1), trim = TRUE), "%")
      names(counts) <- c("Category", "N", "Share")

      shiny::div(
        class = "ec-ordinal-settings",
        shiny::p(
          class = "ec-field-caption",
          paste0(
            "Observed categories (inventory only; no rank is inferred): ",
            paste(observed, collapse = ", ")
          )
        ),
        shiny::div(class = "ec-table-wrap ec-table-compact", shiny::HTML(.ec_html_table(counts))),
        shiny::selectizeInput(
          "ordinal_order",
          .ec_labeled(
            "Ordinal category order (lowest → highest)",
            "Select every observed category exactly once, from the lowest category to the highest. econcompare uses only the order you specify here and never infers a substantive rank from labels or numeric codes.",
            "Select categories in ascending order. You can drag selected labels to reorder them."
          ),
          choices = stats::setNames(choice_values, choice_values),
          selected = selected,
          multiple = TRUE,
          width = "100%",
          options = list(
            plugins = list("remove_button", "drag_drop"),
            create = FALSE,
            persist = FALSE,
            placeholder = "Select lowest category first..."
          )
        ),
        shiny::p(
          class = "ec-field-caption",
          if (length(selected))
            "The order is pre-filled because this variable is already an ordered factor in R. You can still change it before estimation."
          else
            "No order has been pre-filled because the data do not already contain an ordered-factor definition."
        )
      )
    })
  
    output$panel_audit_ui <- shiny::renderUI({
      if (!identical(input$analysis_mode, "panel")) return(NULL)
      if (is.null(input$panel_id) || !nzchar(input$panel_id) || is.null(input$panel_time) || !nzchar(input$panel_time)) {
        candidates <- .ec_panel_candidates(active_data())
        return(shiny::p(class = "ec-note", if (nrow(candidates)) paste("Possible indexes to confirm:", paste(paste(candidates$id, candidates$time, sep = " + "), collapse = "; ")) else "Choose the individual and period indexes explicitly."))
      }
      a <- tryCatch(eco_panel_audit(active_data(), input$panel_id, input$panel_time), error = function(e) e)
      if (inherits(a, "error")) return(shiny::div(class = "ec-status ec-error", conditionMessage(a)))
      shiny::tagList(.ec_panel_summary_ui(a),
        if (nrow(a$issues)) shiny::div(class = "ec-status ec-error", "Each individual-period pair must be unique and non-missing. For quarterly data, select a date or year-quarter index rather than year alone."))
    })

    output$model_selector <- shiny::renderUI({
      if (identical(input$analysis_mode, "panel")) {
        z <- eco_panel_models()
        family <- if (is.null(input$panel_outcome)) "continuous" else input$panel_outcome
        z <- z[z$outcome_type == family, , drop = FALSE]
        defaults <- switch(family, binary = "panel_clogit", count = "panel_poisson", c("panel_pooling", "panel_fe_individual"))
        return(shiny::tagList(
          shiny::checkboxGroupInput("models", NULL, choices = stats::setNames(z$engine, z$estimator), selected = defaults),
          if (!all(z$available)) shiny::p(paste("Missing optional engines:", paste(unique(z$package[!z$available]), collapse = ", ")))))
      }
      if (identical(input$analysis_mode, "time_series")) {
        fam <- if (is.null(input$time_family)) "single" else input$time_family
        if (identical(fam, "ecm")) {
          return(shiny::tagList(
            shiny::checkboxGroupInput("models", NULL, choices = c("Error-correction model (ECM)" = "ecm"), selected = "ecm"),
            shiny::p(class = "ec-field-caption", "ECM is estimated as a transparent two-step single-equation model. The long-run levels relationship is not treated as automatic proof of cointegration.")
          ))
        }
        if (identical(fam, "system")) {
          return(shiny::tagList(
            shiny::radioButtons("models", NULL, choices = c("Vector autoregression (VAR)" = "var", "Vector error-correction model (VECM)" = "vecm"), selected = "var"),
            shiny::p(class = "ec-field-caption", "VAR/VECM treat all selected series as a joint endogenous system. VECM requires an explicit cointegration rank.")
          ))
        }
        z <- eco_time_models()
        choices <- stats::setNames(z$engine, z$estimator)
        return(shiny::tagList(
          shiny::checkboxGroupInput("models", NULL, choices = choices, selected = "time_static"),
          shiny::p(class = "ec-field-caption", "These are explanatory temporal regressions. ARIMA/forecasting engines are intentionally outside econcompare's scope.")
        ))
      }
      shiny::req(input$outcome_type)
      z <- registry[registry$outcome_type == input$outcome_type, , drop = FALSE]
      available <- z$engine[z$available]
      choices <- stats::setNames(available, z$estimator[match(available, z$engine)])
      unavailable <- z[!z$available, , drop = FALSE]
      default <- switch(input$outcome_type,
        continuous = "ols", binary = "logit", nominal = "multinomial_logit", ordinal = "ordered_logit", character())
      if (!default %in% available) default <- if (length(available)) available[1L] else character()
      shiny::tagList(
        shiny::checkboxGroupInput("models", NULL, choices = choices, selected = default),
        if (nrow(unavailable)) shiny::p(class = "ec-field-caption", paste0("Unavailable locally: ", paste(paste0(unavailable$estimator, " (", unavailable$package, ")"), collapse = "; ")))
      )
    })
  
    shiny::observeEvent(input$reset, {
      type_state$overrides <- character(); type_state$error <- NULL
      shiny::updateSelectInput(session, "analysis_mode", selected = initial_mode)
      shiny::updateSelectInput(session, "panel_outcome", selected = "continuous")
      shiny::updateSelectInput(session, "time_family", selected = "single")
      shiny::updateSelectInput(session, "time_var", selected = initial_time)
      shiny::updateSelectInput(session, "y", selected = initial_y)
      shiny::updateSelectInput(session, "outcome_type", selected = initial_type)
      shiny::updateSelectizeInput(session, "x", selected = character(0), choices = setdiff(all_vars, initial_y), server = TRUE)
      state$fit <- NULL
      state$panel_diag <- NULL
      state$comparison <- NULL
      state$error <- NULL
      state$warnings <- data.frame(model=character(), engine=character(), message=character(), stringsAsFactors=FALSE)
      state$extraction_warnings <- .ec_empty_extraction_issue()
      state$extraction_failures <- .ec_empty_extraction_issue()
      state$diag_results <- list()
      state$diag_selected <- list()
      state$diag_errors <- list()
      state$time_diag <- NULL
      state$stationarity <- NULL
      state$time_diag_error <- NULL
      state$system_diag <- NULL
      state$system_diag_error <- NULL
      state$ran <- FALSE
    })
  
    output$model_badges <- shiny::renderUI({
      .ec_choice_badges(input$models)
    })
  
    output$model_options <- shiny::renderUI({
      mods <- input$models
      dat_now <- active_data()
      numeric_now <- names(dat_now)[vapply(dat_now, is.numeric, logical(1))]
      if (!length(mods)) return(NULL)
      if (identical(input$analysis_mode, "panel")) {
        old <- shiny::isolate(list(inference = input$panel_inference, endogenous = input$panel_endogenous, instruments = input$panel_instruments, na = input$panel_na))
        inference_choices <- c("Choose explicitly..." = "", "Classical model-based" = "classical")
        if (!"panel_clogit" %in% mods) inference_choices <- c(inference_choices, "Individual cluster" = "cluster_id")
        instrument_choices <- setdiff(numeric_now, c(input$y, input$x, input$panel_id, input$panel_time))
        return(shiny::tagList(
        shiny::selectInput("panel_inference", "Coefficient inference", choices = inference_choices, selected = if (length(old$inference) && old$inference %in% inference_choices) old$inference else ""),
        if ("panel_fe_iv" %in% mods) shiny::tagList(
          shiny::selectizeInput("panel_endogenous", "Endogenous regressors", choices = input$x, selected = intersect(old$endogenous, input$x), multiple = TRUE),
          shiny::selectizeInput("panel_instruments", "Excluded instruments", choices = instrument_choices, selected = intersect(old$instruments, instrument_choices), multiple = TRUE),
          shiny::p(class = "ec-note", "Instruments must be justified by the research design. They are never selected automatically. Missing instrument values enter the common-sample filter for this run.")),
        if ("panel_clogit" %in% mods) shiny::p(class = "ec-note", "Exact conditional logit: choose classical inference. Individuals with all 0 or all 1 outcomes are excluded. No unconditional probability is estimated."),
        shiny::selectInput("panel_na", "Missing model values", choices = c("Stop and inspect" = "fail", "Explicitly omit; report common sample" = "omit"), selected = if (length(old$na) && old$na %in% c("fail", "omit")) old$na else "fail"),
        shiny::p(class = "ec-help", "Fixed effects may absorb invariant regressors. Random effects require orthogonality with individual heterogeneity. Cluster inference requires independent individuals and can be unreliable with few clusters.")
      ))
      }
      blocks <- list(
        shiny::div(class = "ec-section",
          shiny::div(class = "ec-step", shiny::div(class = "ec-step-n", "3"), shiny::div(class = "ec-h", "Set model parameters")),
          shiny::p(class = "ec-help", "Only the controls relevant to your selected models are shown below.")
        )
      )
  
      if (identical(input$analysis_mode, "time_series")) {
        fam <- if (is.null(input$time_family)) "single" else input$time_family
        if (identical(fam, "ecm")) {
          blocks <- c(blocks, list(
            shiny::numericInput("ecm_p", .ec_labeled("Short-run lags of Delta Y (p)", "Number of lagged first differences of the dependent variable included in the ECM short-run equation.", "p = 0 is allowed."), value = 1, min = 0, step = 1),
            shiny::numericInput("ecm_q", .ec_labeled("Short-run lags of Delta X (q)", "Default number of lagged first differences of each explanatory variable, in addition to the contemporaneous difference.", "The programmatic API q_by_var supports variable-specific orders."), value = 0, min = 0, step = 1),
            shiny::checkboxInput("ecm_long_intercept", "Include intercept in long-run levels relationship", value = TRUE),
            shiny::selectInput("time_inference", "ECM coefficient inference", choices=c("Classical OLS"="classical","HAC / Newey-West"="HAC"), selected="classical"),
            shiny::conditionalPanel(condition="input.time_inference == 'HAC'", shiny::numericInput("time_hac_lag", "HAC truncation lag", value=4, min=0, step=1)),
            shiny::div(class="ec-tip-box", shiny::strong("ECM prerequisite · "), "Use an ECM only when a long-run relationship is economically and statistically defensible. econcompare does not infer cointegration from the first-step levels regression alone.")
          ))
          return(shiny::tagList(blocks))
        }
        if (identical(fam, "system")) {
          if (identical(mods, "vecm")) {
            blocks <- c(blocks, list(
              shiny::numericInput("system_p", .ec_labeled("VAR lag order in levels (K)", "Johansen/VECM uses the lag order of the underlying VAR in levels. This must be at least 2.", "econcompare does not choose K automatically."), value=2, min=2, step=1),
              shiny::numericInput("vecm_rank", .ec_labeled("Cointegration rank (r)", "Number of cointegrating relations imposed in the VECM. The researcher must choose this explicitly after considering Johansen tests and substantive knowledge.", "With k variables, r must lie between 1 and k-1."), value=1, min=1, step=1),
              shiny::selectInput("vecm_test", "Johansen statistic", choices=c("Trace"="trace","Maximum eigenvalue"="eigen"), selected="trace"),
              shiny::selectInput("vecm_ecdet", "Deterministic term in cointegration relations", choices=c("Constant in cointegration relations"="const","No deterministic term in cointegration relations"="none","Trend in cointegration relations"="trend"), selected="const"),
              shiny::selectInput("vecm_spec", "Johansen specification", choices=c("Transitory"="transitory","Long-run"="longrun"), selected="transitory"),
              shiny::helpText("Transitory uses lag-1 levels; long-run uses lag-K levels. Both parameterizations retain the same long-run matrix but express short-run coefficients differently. Deterministic terms in the differenced equations also depend on the selected Johansen case."),
              shiny::div(class="ec-tip-box", shiny::strong("Rank decision · "), "Johansen statistics are evidence for the researcher, not an automatic rank selector. Cointegrating vectors are normalization-dependent.")
            ))
          } else {
            blocks <- c(blocks, list(
              shiny::numericInput("system_p", .ec_labeled("VAR lag order (p)", "Number of lags of every endogenous variable included in each VAR equation.", "Use information criteria as decision aids, not an automatic rule."), value=2, min=1, step=1),
              shiny::selectInput("var_deterministic", "VAR deterministic terms", choices=c("Intercept"="const","Trend"="trend","Intercept + trend"="both","None"="none"), selected="const"),
              shiny::div(class="ec-tip-box", shiny::strong("System interpretation · "), "All selected series are endogenous in the VAR. econcompare estimates the reduced-form system and does not impose a structural identification or Cholesky ordering.")
            ))
          }
          return(shiny::tagList(blocks))
        }
        if (any(mods %in% c("dynamic_regression", "ardl"))) blocks <- c(blocks, list(
          shiny::numericInput("time_p", .ec_labeled("Dependent-variable lags (p)", "Number of lags of Y included in dynamic regression / ARDL. Lag choice is a modelling decision; econcompare does not optimize it automatically.", "Start small relative to the number of time observations."), value = 1, min = 1, step = 1)
        ))
        if (any(mods %in% c("distributed_lag", "ardl"))) blocks <- c(blocks, list(
          shiny::numericInput("time_q", .ec_labeled("Explanatory-variable lags (q)", "Number of lags included for each explanatory variable in distributed-lag / ARDL specifications. The Shiny control applies one common q to each X; the programmatic API `q_by_var` supports variable-specific lag orders.", "q = 1 includes X_t and X_(t-1)."), value = 1, min = 1, step = 1)
        ))
        blocks <- c(blocks, list(
          shiny::selectInput("time_inference", .ec_labeled("Coefficient inference", "Classical OLS standard errors or HAC/Newey-West standard errors. HAC changes inference, not the OLS coefficient estimates.", "Use HAC when serial correlation / heteroskedasticity-robust inference is substantively appropriate."), choices=c("Classical OLS"="classical","HAC / Newey-West"="HAC"), selected="classical"),
          shiny::conditionalPanel(condition="input.time_inference == 'HAC'", shiny::numericInput("time_hac_lag", "HAC truncation lag", value=4, min=0, step=1)),
          shiny::div(class="ec-tip-box", shiny::strong("Interpretation · "), "Lagged specifications lose initial observations mechanically. econcompare blocks calendar gaps rather than redefining L1 as the previous observed row.")
        ))
        return(shiny::tagList(blocks))
      }

      if ("wls" %in% mods) {
        blocks <- c(blocks, list(
          shiny::selectInput(
            "wls_weight",
            .ec_labeled(
              "WLS · weight variable",
              "We use WLS to correct for heteroscedasticity by giving more weight to precise observations and less weight to noisy ones, so the estimator becomes more efficient when variance differs across observations.",
              "Choose a numeric variable containing the weights to be used in weighted least squares."
            ),
            choices = numeric_now
          )
        ))
      }
      if ("ols_robust" %in% mods) {
        blocks <- c(blocks, list(
          shiny::selectInput(
            "robust_se",
            .ec_labeled(
              "Robust OLS · standard errors",
              "Robust standard errors leave the OLS coefficients unchanged, but adjust the uncertainty measures when heteroscedasticity is present.",
              "HC3 is often a safe default in small samples."
            ),
            choices = c("HC0", "HC1", "HC2", "HC3"),
            selected = "HC3"
          )
        ))
      }
      if ("quantile" %in% mods) {
        blocks <- c(blocks, list(
          shiny::textInput(
            "taus",
            .ec_labeled(
              "Quantile regression · tau values",
              "Quantile regression lets you study how covariates affect different points of the outcome distribution, not only the conditional mean. Enter values between 0 and 1.",
              "Example: 0.25, 0.50, 0.75 compares the lower, median and upper parts of the distribution."
            ),
            value = "0.25, 0.50, 0.75"
          ),
          shiny::selectInput(
            "quantile_se",
            .ec_labeled(
              "Quantile regression · inference",
              "This controls the standard-error method used by quantreg::summary.rq(). Different methods can produce different uncertainty estimates, especially in smaller samples.",
              "NID is retained as the default for continuity. Bootstrap can provide bootstrap standard errors; econcompare does not currently reconstruct a bootstrap confidence interval from them."
            ),
            choices = c("nid", "iid", "ker", "boot"),
            selected = "nid"
          )
        ))
      }
      if ("ivreg" %in% mods) {
        blocks <- c(blocks, list(
          shiny::selectizeInput(
            "iv_endog",
            .ec_labeled(
              "IV / 2SLS · endogenous regressor(s)",
              "Select the regressor(s) you believe may be correlated with the error term. IV estimation tries to isolate exogenous variation for those variables.",
              "These variables are still part of the structural equation, but they need instruments."
            ),
            choices = input$x,
            multiple = TRUE
          ),
          shiny::selectizeInput(
            "iv_instruments",
            .ec_labeled(
              "IV / 2SLS · excluded instrument(s)",
              "Instruments must be correlated with the endogenous regressor(s) and plausibly exogenous to the structural error. They are excluded from the main equation but included in the first stage.",
              "Choose variables that help explain the endogenous regressor without directly affecting the outcome, except through that regressor."
            ),
            choices = setdiff(all_vars, c(input$y, input$x)),
            multiple = TRUE
          )
        ))
      }
      if ("tobit" %in% mods) {
        blocks <- c(blocks, list(
          shiny::textInput(
            "tobit_left",
            .ec_labeled(
              "Tobit · left bound",
              "Use a Tobit model when the dependent variable is censored. The left bound is the lower censoring threshold: values below it are recorded at the threshold.",
              "Use -Inf if there is no left censoring."
            ),
            value = "-Inf"
          ),
          shiny::textInput(
            "tobit_right",
            .ec_labeled(
              "Tobit · right bound",
              "The right bound is the upper censoring threshold. A right-censored variable cannot be observed above this bound; values above it are recorded at the threshold.",
              "Use Inf if there is no right censoring."
            ),
            value = "Inf"
          )
        ))
      }
      if ("heckman" %in% mods) {
        selection_choices <- setdiff(binary_candidates, input$y)
        if (!length(selection_choices)) selection_choices <- c("No compatible binary indicator available" = "")
        blocks <- c(blocks, list(
          shiny::selectInput(
            "selection_y",
            .ec_labeled(
              "Heckman · selection indicator",
              "This variable describes whether an observation is selected into the outcome equation. It must be a binary variable and cannot also be used as its own selection regressor.",
              "Choose an observed binary selection indicator. econcompare excludes it automatically from the selection-regressor list."
            ),
            choices = selection_choices
          ),
          shiny::uiOutput("heckman_selection_x_ui"),
          shiny::selectInput(
            "heckman_method",
            .ec_labeled(
              "Heckman · estimation method",
              "The two-step estimator is transparent and convenient for diagnostic work. Maximum likelihood can be more efficient when its distributional assumptions are credible.",
              "Use 2step for a transparent baseline; consider ml when the likelihood specification is substantively justified."
            ),
            choices = c("2step", "ml"),
            selected = "2step"
          )
        ))
      }
      shiny::tagList(blocks)
    })

    output$heckman_selection_x_ui <- shiny::renderUI({
      mods <- input$models
      if (is.null(mods) || !"heckman" %in% mods) return(NULL)
      sy <- if (is.null(input$selection_y)) "" else input$selection_y
      choices <- .ec_selection_regressor_choices(all_vars, input$y, sy)
      selected <- shiny::isolate(input$selection_x)
      selected <- intersect(if (is.null(selected)) character() else selected, choices)
      shiny::selectizeInput(
        "selection_x",
        .ec_labeled(
          "Heckman · selection regressors",
          "These variables explain the selection process. The selection indicator itself and the outcome variable are excluded automatically.",
          "An exclusion restriction — at least one selection regressor omitted from the outcome equation — is usually desirable for stronger identification."
        ),
        choices = choices,
        selected = selected,
        multiple = TRUE,
        width = "100%",
        options = list(plugins = list("remove_button"))
      )
    })
  
    state <- shiny::reactiveValues(
      fit = NULL,
      run_config = NULL,
      run_data = NULL,
      run_time = NULL,
      comparison = NULL,
      error = NULL,
      warnings = data.frame(model=character(), engine=character(), message=character(), stringsAsFactors=FALSE),
      extraction_warnings = .ec_empty_extraction_issue(),
      extraction_failures = .ec_empty_extraction_issue(),
      ran = FALSE,
      diag_results = list(),
      diag_selected = list(),
      diag_errors = list(),
      time_diag = NULL,
      stationarity = NULL,
      time_diag_error = NULL,
      panel_diag = NULL,
      system_diag = NULL,
      system_diag_error = NULL
    )
  
    current_config <- shiny::reactive({
      values <- shiny::reactiveValuesToList(input)
      keys <- c("analysis_mode", "binary_event", "ecm_long_intercept", "ecm_p", "ecm_q", "heckman_method", "iv_endog", "iv_instruments", "models", "ordinal_order", "outcome_type", "panel_id", "panel_outcome", "panel_endogenous", "panel_instruments", "panel_inference", "panel_na", "panel_time", "quantile_se", "robust_se", "selection_x", "selection_y", "system_p", "taus", "time_family", "time_hac_lag", "time_inference", "time_p", "time_q", "time_var", "tobit_left", "tobit_right", "var_deterministic", "vecm_ecdet", "vecm_rank", "vecm_spec", "vecm_test", "wls_weight", "x", "y")
      list(inputs = stats::setNames(lapply(keys, function(k) values[[k]]), keys),
        overrides = type_state$overrides, ordinal_levels = type_state$ordinal_levels)
    })
    results_stale <- shiny::reactive({
      !is.null(state$fit) && !identical(current_config(), state$run_config)
    })
    diagnostic_ready <- function() {
      if (isTRUE(results_stale())) {
        shiny::showNotification("Settings changed. Run comparison again before requesting diagnostics.", type = "warning")
        return(FALSE)
      }
      TRUE
    }

    cmp_all <- shiny::reactive({
      shiny::req(state$fit, state$comparison)
      state$comparison
    })
  
    sample_all <- shiny::reactive({
      shiny::req(state$fit)
      eco_sample_audit(state$fit)
    })
  
    shiny::observeEvent(input$run, {
      state$ran <- TRUE
      state$error <- NULL
      state$warnings <- data.frame(model=character(), engine=character(), message=character(), stringsAsFactors=FALSE)
      state$extraction_warnings <- .ec_empty_extraction_issue()
      state$extraction_failures <- .ec_empty_extraction_issue()
      state$fit <- NULL
      state$panel_diag <- NULL
      state$comparison <- NULL
      state$diag_results <- list()
      state$diag_selected <- list()
      state$diag_errors <- list()
      state$time_diag <- NULL
      state$stationarity <- NULL
      state$time_diag_error <- NULL
      state$system_diag <- NULL
      state$system_diag_error <- NULL
  
      tryCatch({
        state$run_config <- current_config()
        state$run_data <- active_data()
        state$run_time <- Sys.time()
        y <- input$y
        x <- input$x
        if (!length(y) || !length(x)) .ec_stop("Choose one dependent variable and at least one explanatory variable.")

        if (identical(input$analysis_mode, "panel")) {
          if (is.null(input$panel_inference) || !nzchar(input$panel_inference)) .ec_stop("Choose the panel inference method explicitly.")
          fit <- eco_panel_run(active_data(), .ec_formula(y, x), id = input$panel_id, time = input$panel_time,
            models = input$models, inference = input$panel_inference,
            outcome_type = if (is.null(input$panel_outcome)) "continuous" else input$panel_outcome,
            endogenous = if ("panel_fe_iv" %in% input$models) input$panel_endogenous else NULL,
            instruments = if ("panel_fe_iv" %in% input$models) input$panel_instruments else NULL,
            na_action = if (is.null(input$panel_na)) "fail" else input$panel_na, error_policy = "collect")
          state$fit <- fit; state$comparison <- eco_compare(fit)
          state$warnings <- fit$warnings; state$panel_diag <- NULL
          shiny::updateTabsetPanel(session, "workspace_tab", selected = "2 · Compare models")
          return()
        }
        if (identical(input$analysis_mode, "time_series")) {
          tv <- input$time_var
          if (is.null(tv) || !nzchar(tv)) .ec_stop("Choose an explicit time index for time-series econometrics.")
          if (tv %in% c(y, x)) .ec_stop("The time index must be structural metadata, not a model variable.")
          dat <- active_data()
          vars_selected <- unique(c(y, x))
          nonnum_all <- vars_selected[!vapply(dat[vars_selected], is.numeric, logical(1))]
          if (length(nonnum_all)) .ec_stop("Time-series econometrics requires numeric model variables: ", paste(nonnum_all, collapse=", "), ". Use Manual variable typing if storage is wrong.")
          mods <- unique(input$models)
          if (!length(mods)) .ec_stop("Choose a time-series econometric model.")
          fam <- if (is.null(input$time_family)) "single" else input$time_family
          fml <- .ec_formula(y, x)

          if (identical(fam, "ecm")) {
            if (!identical(mods, "ecm")) .ec_stop("ECM mode accepts the ECM specification only.")
            fit <- eco_ecm_run(
              data=dat, formula=fml, time=tv,
              p=if (is.null(input$ecm_p)) 1L else input$ecm_p,
              q=if (is.null(input$ecm_q)) 0L else input$ecm_q,
              long_run_intercept=isTRUE(input$ecm_long_intercept),
              inference=if (is.null(input$time_inference)) "classical" else input$time_inference,
              hac_lag=if (!is.null(input$time_inference) && identical(input$time_inference,"HAC")) input$time_hac_lag else NULL
            )
          } else if (identical(fam, "system")) {
            if (length(mods) != 1L || !mods %in% c("var", "vecm")) .ec_stop("Dynamic-system mode requires exactly one of VAR or VECM.")
            fit <- if (identical(mods, "var")) {
              eco_system_run(dat, variables=vars_selected, time=tv, model="var",
                             p=if (is.null(input$system_p)) 2L else input$system_p,
                             deterministic=if (is.null(input$var_deterministic)) "const" else input$var_deterministic)
            } else {
              eco_system_run(dat, variables=vars_selected, time=tv, model="vecm",
                             p=if (is.null(input$system_p)) 2L else input$system_p,
                             rank=if (is.null(input$vecm_rank)) 1L else input$vecm_rank,
                             johansen_type=if (is.null(input$vecm_test)) "trace" else input$vecm_test,
                             ecdet=if (is.null(input$vecm_ecdet)) "const" else input$vecm_ecdet,
                             spec=if (is.null(input$vecm_spec)) "transitory" else input$vecm_spec)
            }
          } else {
            allowed <- eco_time_models()$engine
            bad <- setdiff(mods, allowed); if (length(bad)) .ec_stop("Selected model(s) do not belong to the single-equation time-series mode: ", paste(bad, collapse=", "), ".")
            p_lag <- if (any(mods %in% c("dynamic_regression", "ardl"))) input$time_p else 1L
            q_lag <- if (any(mods %in% c("distributed_lag", "ardl"))) input$time_q else 1L
            fit <- eco_time_run(data=dat, formula=fml, time=tv, models=mods, p=p_lag, q=q_lag,
                                inference=if (is.null(input$time_inference)) "classical" else input$time_inference,
                                hac_lag=if (!is.null(input$time_inference) && identical(input$time_inference,"HAC")) input$time_hac_lag else NULL,
                                error_policy="collect")
          }
          comparison <- eco_compare(fit, error_policy="collect")
          state$fit <- fit; state$comparison <- comparison; state$warnings <- fit$warnings
          state$extraction_warnings <- attr(comparison, "extraction_warnings"); state$extraction_failures <- attr(comparison, "extraction_failures")
          if (is.null(state$extraction_warnings)) state$extraction_warnings <- .ec_empty_extraction_issue()
          if (is.null(state$extraction_failures)) state$extraction_failures <- .ec_empty_extraction_issue()
          shiny::updateTabsetPanel(session, "workspace_tab", selected = "2 · Compare models")
          return()
        }

        app_data <- active_data()
        binary_event <- NULL
        if (identical(input$outcome_type, "binary")) {
          if (!.ec_is_binary_indicator(app_data[[y]])) .ec_stop("The selected outcome does not currently contain exactly two observed states.")
          if (is.character(app_data[[y]]) || is.factor(app_data[[y]])) {
            binary_event <- input$binary_event
            if (is.null(binary_event) || !nzchar(binary_event)) .ec_stop("Choose which binary category is the event coded 1.")
          }
        }
        if (identical(input$outcome_type, "nominal") && (is.numeric(app_data[[y]]) || is.integer(app_data[[y]]))) {
          .ec_stop("Nominal outcomes stored as numeric are not converted automatically. Convert the variable to factor() before launching econcompare.")
        }
        if (identical(input$outcome_type, "ordinal")) {
          ordinal_order <- if (is.null(input$ordinal_order)) character() else input$ordinal_order
          app_data[[y]] <- .ec_user_ordered(app_data[[y]], ordinal_order)
        }
  
        mods <- unique(input$models)
        if (!length(mods)) .ec_stop("Choose at least one model for the selected analysis objective.")
        args <- list()
        iv_formula <- NULL
        selection_formula <- NULL
  
        if ("wls" %in% mods) {
          if (is.null(input$wls_weight) || !nzchar(input$wls_weight)) .ec_stop("WLS requires a weight variable.")
          args$wls <- list(weights = input$wls_weight)
        }
        if ("ols_robust" %in% mods) args$ols_robust <- list(se_type = input$robust_se)
        if ("quantile" %in% mods) {
          taus <- .ec_parse_numeric_list(input$taus)
          if (is.null(taus) || any(taus <= 0 | taus >= 1)) .ec_stop("Quantile tau values must lie strictly between 0 and 1.")
          args$quantile <- list(tau = taus, se = input$quantile_se)
        }
        if ("ivreg" %in% mods) {
          endog <- input$iv_endog
          inst <- input$iv_instruments
          if (!length(endog)) .ec_stop("IV/2SLS requires at least one endogenous regressor.")
          if (!length(inst)) .ec_stop("IV/2SLS requires at least one excluded instrument.")
          iv_formula <- .ec_iv_formula(y, x, input$iv_endog, input$iv_instruments)
        }
        if ("tobit" %in% mods) {
          args$tobit <- list(left = .ec_parse_bound(input$tobit_left, "left"), right = .ec_parse_bound(input$tobit_right, "right"))
          if (is.finite(args$tobit$left) && is.finite(args$tobit$right) && args$tobit$left >= args$tobit$right) {
            .ec_stop("Tobit left bound must be lower than the right bound.")
          }
        }
        if ("heckman" %in% mods) {
          sy <- input$selection_y
          sx <- input$selection_x
          .ec_validate_heckman_selection(y, sy, sx, outcome_x = x)
          if (!sy %in% binary_candidates) .ec_stop("Heckman selection indicator must be a binary variable with two observed states.")
          selection_formula <- .ec_formula(sy, sx)
          args$heckman <- list(method = input$heckman_method)
        }
  
        fml <- .ec_formula(y, x)
        chosen_type <- unique(registry$outcome_type[match(mods, registry$engine)])
        if (length(chosen_type) != 1L || !identical(chosen_type, input$outcome_type)) .ec_stop("Selected models do not match the selected analysis objective.")
        fit <- eco_run(data = app_data, formula = fml, models = mods, model_args = args,
                       iv_formula = iv_formula, selection_formula = selection_formula, outcome_formula = fml,
                       binary_event = binary_event,
                       error_policy = "collect")
        comparison <- eco_compare(fit, error_policy = "collect")
        state$fit <- fit
        state$comparison <- comparison
        state$warnings <- fit$warnings
        state$extraction_warnings <- attr(comparison, "extraction_warnings")
        state$extraction_failures <- attr(comparison, "extraction_failures")
        if (is.null(state$extraction_warnings)) state$extraction_warnings <- .ec_empty_extraction_issue()
        if (is.null(state$extraction_failures)) state$extraction_failures <- .ec_empty_extraction_issue()
        shiny::updateTabsetPanel(session, "workspace_tab", selected = "2 · Compare models")
      }, error = function(e) {
        state$error <- conditionMessage(e)
      })
    })
  
    output$run_status <- shiny::renderUI({
      if (!state$ran) return(NULL)
      if (isTRUE(results_stale())) return(shiny::div(class = "ec-status ec-warn", "Settings changed. Results below belong to the previous run; run comparison again to update them."))
      if (!is.null(state$error)) return(shiny::div(class = "ec-status ec-error", state$error))
      if (!is.null(state$fit) && (nrow(state$fit$failures) || nrow(state$extraction_failures))) {
        return(shiny::div(
          class = "ec-status ec-warn",
          paste0(
            length(state$fit$models), " model result(s) estimated; ",
            nrow(state$fit$failures), " estimation failure(s) and ",
            nrow(state$extraction_failures), " extraction failure(s). See Warnings / failures."
          )
        ))
      }
      if (!is.null(state$fit)) {
        sa <- eco_sample_audit(state$fit)
        if (inherits(state$fit, "econcompare_panel") && length(unique(sa$observation_unit)) > 1L) {
          return(shiny::div(class = "ec-status ec-warn", paste(length(state$fit$models), "model result(s) ready with different observation units. Inspect source exclusions and effective counts in Sample audit.")))
        }
        if (nrow(sa) && (any(sa$matches_reference_n %in% FALSE, na.rm = TRUE) || any(sa$matches_reference_rows %in% FALSE, na.rm = TRUE))) return(shiny::div(class = "ec-status ec-warn", paste(length(state$fit$models), "model result(s) ready; estimation-sample mismatch detected across models. See Sample audit.")))
      }
      if ((is.data.frame(state$warnings) && nrow(state$warnings)) || nrow(state$extraction_warnings)) {
        return(shiny::div(class = "ec-status ec-warn", paste(length(state$fit$models), "model result(s) ready with warning(s). See Warnings / failures.")))
      }
      if (!is.null(state$fit)) return(shiny::div(class = "ec-status ec-ok", paste(length(state$fit$models), "model result(s) ready.")))
      NULL
    })
  
    output$run_meta <- shiny::renderUI({
      if (is.null(state$fit)) return(NULL)
      shiny::tagList(.ec_reproducibility_ui(state$fit), shiny::div(class = "ec-chip-wrap",
        shiny::tags$span(class = "ec-chip ec-chip-soft", if (!is.null(state$fit$formula)) paste("Fitted:", paste(deparse(state$fit$formula), collapse = " ")) else paste("Fitted variables:", paste(state$fit$variables, collapse = ", "))),
        shiny::tags$span(class = "ec-chip ec-chip-soft", paste(length(state$fit$models), "estimated result(s)")),
        shiny::tags$span(class = "ec-chip ec-chip-soft", if (isTRUE(state$fit$analysis_type %in% c("time_series", "time_series_system"))) paste("Time index:", state$fit$time_variable) else paste("Outcome:", state$fit$outcome_type)),
        if (identical(state$fit$analysis_type, "time_series")) shiny::tags$span(class="ec-chip ec-chip-soft", paste("Inference:", state$fit$inference)),
        if (identical(state$fit$analysis_type, "time_series_system")) shiny::tags$span(class="ec-chip ec-chip-soft", paste("System:", toupper(state$fit$temporal_family))),
        if (length(state$fit$type_overrides)) shiny::tags$span(class="ec-chip ec-chip-soft", paste(length(state$fit$type_overrides), "manual type override(s)")),
        shiny::tags$span(class = "ec-chip ec-chip-soft", paste("Generated", format(state$run_time, "%H:%M")))
      ))
    })
  
    output$diag_controls_ui <- shiny::renderUI({
      if (is.null(state$fit) || is.null(input$diag_model) || !input$diag_model %in% names(state$fit$models)) return(NULL)
      choices <- .ec_diag_choices_for_model(state$fit, input$diag_model)
      selected <- state$diag_selected[[input$diag_model]]
      if (is.null(selected)) selected <- character()
      if (!nrow(choices)) {
        return(shiny::div(class = "ec-empty ec-empty-small", "No validated diagnostics are currently available for this estimator."))
      }
      shiny::tagList(
        shiny::div(class = "ec-diag-picker-head",
          shiny::div(
            shiny::h4("Choose a few essential checks"),
            shiny::p(class = "ec-note", "econcompare intentionally shows a short checklist rather than every available test. Choose only the questions that matter for your comparison; technical diagnostics remain available through the programmatic API.")
          )
        ),
        shiny::checkboxGroupInput(
          "diag_tests", NULL,
          choices = .ec_diag_choice_labels(choices),
          selected = intersect(selected, choices$id)
        ),
        shiny::div(class = "ec-diag-actions",
          shiny::actionButton("diag_recommended", "Select core checks", class = "btn-default"),
          shiny::actionButton("diag_clear", "Clear selection", class = "btn-default"),
          shiny::actionButton("diag_run", "Run selected diagnostics", class = "btn-primary")
        ),
        shiny::p(class = "ec-field-caption", "Core checks are suggestions only. Nothing runs until you click Run selected diagnostics. Use the coefficient table and technical details when you need to go deeper.")
      )
    })
  
    shiny::observeEvent(input$diag_recommended, {
      shiny::req(state$fit, input$diag_model)
      choices <- .ec_diag_choices_for_model(state$fit, input$diag_model)
      ids <- choices$id[choices$recommended]
      errs <- state$diag_errors
      errs[[input$diag_model]] <- NULL
      state$diag_errors <- errs
      shiny::updateCheckboxGroupInput(session, "diag_tests", selected = ids)
    })
  
    shiny::observeEvent(input$diag_clear, {
      shiny::req(input$diag_model)
      errs <- state$diag_errors
      errs[[input$diag_model]] <- NULL
      state$diag_errors <- errs
      shiny::updateCheckboxGroupInput(session, "diag_tests", selected = character())
    })
  
    shiny::observeEvent(input$diag_run, {
      if (!diagnostic_ready()) return()
      shiny::req(state$fit, input$diag_model)
      model_name <- input$diag_model
      tests <- input$diag_tests
      if (!length(tests)) {
        errs <- state$diag_errors
        errs[[model_name]] <- "Select at least one diagnostic before running the diagnostic workspace."
        state$diag_errors <- errs
        return()
      }
      errs <- state$diag_errors
      errs[[model_name]] <- NULL
      state$diag_errors <- errs
      selected <- state$diag_selected
      selected[[model_name]] <- tests
      state$diag_selected <- selected
      ans <- tryCatch(
        eco_diagnostics(state$fit, models = model_name, tests = tests),
        error = function(e) e
      )
      if (inherits(ans, "error")) {
        errs <- state$diag_errors
        errs[[model_name]] <- conditionMessage(ans)
        state$diag_errors <- errs
        return()
      }
      results <- state$diag_results
      results[[model_name]] <- ans
      state$diag_results <- results
    })
  
    output$diag_model_ui <- shiny::renderUI({
      if (is.null(state$fit) || is.null(input$diag_model) || !input$diag_model %in% names(state$fit$models)) return(NULL)
      model_name <- input$diag_model
      z <- state$diag_results[[model_name]]
      selected <- state$diag_selected[[model_name]]
      if (is.null(selected)) selected <- character()
  
      diag_error <- state$diag_errors[[model_name]]
      if (!is.null(diag_error) && nzchar(diag_error)) {
        return(shiny::div(class = "ec-status ec-error", diag_error))
      }
      if (is.null(z)) {
        return(shiny::div(class = "ec-empty ec-empty-small",
          shiny::tags$b("No diagnostics run for this model yet."),
          "Choose one or more essential checks above. You can use Select core checks as a shortcut, then run the selection explicitly."
        ))
      }
  
      engine <- .ec_model_engine(state$fit, model_name)
      show_coef <- "coefficient_significance" %in% selected
      ci <- if (show_coef) .ec_coef_inference_table(
        state$fit, model_name,
        include_ci = TRUE
      ) else data.frame()
      ci_show <- ci
      if (nrow(ci_show)) {
        num <- vapply(ci_show, is.numeric, logical(1))
        ci_show[num] <- lapply(ci_show[num], .ec_fmt)
      }
      raw <- z
      if (nrow(raw)) {
        num <- vapply(raw, is.numeric, logical(1)); raw[num] <- lapply(raw[num], .ec_fmt)
      }
      shiny::tagList(
        shiny::div(class = "ec-synthesis",
          shiny::div(class = "ec-synthesis-title", paste("Diagnostic synthesis ·", model_name)),
          shiny::p(.ec_diag_synthesis(z, engine))
        ),
        if (nrow(ci_show)) shiny::div(class = "ec-inference-box",
          shiny::h4(if (identical(engine, "heckman")) "Outcome-equation coefficient inference" else if (identical(engine, "tobit")) "Latent-outcome coefficient inference" else "Coefficient-level inference"),
          shiny::p(class = "ec-note", "Only structural slope coefficients relevant to the selected model are shown here. Intercepts, ancillary parameters, and Heckman selection-equation parameters are excluded from this slope-inference table."),
          shiny::div(class = "ec-table-wrap ec-table", shiny::HTML(.ec_html_table(ci_show)))
        ),
        .ec_diag_cards_ui(z),
        shiny::tags$details(class = "ec-raw",
          shiny::tags$summary("Raw selected-diagnostic table"),
          shiny::div(class = "ec-table-wrap ec-table", shiny::HTML(.ec_html_table(raw)))
        )
      )
    })
  
    output$time_diag_ui <- shiny::renderUI({
      if (is.null(state$fit) || !identical(state$fit$analysis_type, "time_series")) return(NULL)
      vars <- unique(all.vars(state$fit$formula))
      vars <- setdiff(vars, state$fit$time_variable)
      shiny::tagList(
        shiny::div(class="ec-diag-toolbar",
          shiny::numericInput("time_bg_order", "Breusch-Godfrey lag order", value=1, min=1, step=1, width="260px"),
          shiny::selectizeInput("time_stationarity_vars", "Series to test", choices=stats::setNames(vars, vars), selected=vars[1L], multiple=TRUE, width="360px", options=list(plugins=list("remove_button"), placeholder="Choose one or more series...")),
          shiny::numericInput("time_adf_k", "ADF lag order (k)", value=1, min=0, step=1, width="260px"),
          shiny::selectInput("time_adf_deterministic", "ADF deterministic component", choices=c("No deterministic term"="none","Intercept / drift"="drift","Intercept + trend"="trend"), selected="drift", width="260px"),
          shiny::selectInput("time_kpss_null", "KPSS null", choices=c("Level stationarity"="Level","Trend stationarity"="Trend"), selected="Level", width="260px"),
          shiny::actionButton("run_time_bg", "Run residual autocorrelation test"),
          shiny::actionButton("run_stationarity", "Run ADF + KPSS")
        ),
        shiny::p(class="ec-note", "ADF and KPSS have different null hypotheses. Select only the series you want to test, specify the ADF deterministic component and lag order, and choose the KPSS level/trend null explicitly. econcompare reports results and never differences a variable automatically."),
        shiny::uiOutput("time_diag_results")
      )
    })
    output$time_diag_results <- shiny::renderUI({
      if (is.null(state$fit)) return(NULL)
      shiny::tagList(
        if (!is.null(state$time_diag_error)) shiny::div(class="ec-status ec-warn", state$time_diag_error),
        if (is.data.frame(state$time_diag) && nrow(state$time_diag)) shiny::tagList(
          shiny::h4("Residual serial correlation"),
          shiny::div(class="ec-table-wrap ec-table", shiny::HTML(.ec_html_table(state$time_diag)))
        ),
        if (is.data.frame(state$stationarity) && nrow(state$stationarity)) shiny::tagList(
          shiny::h4("Stationarity diagnostics"),
          shiny::div(class="ec-table-wrap ec-table", shiny::HTML(.ec_html_table(state$stationarity)))
        ),
        if (is.null(state$time_diag) && is.null(state$stationarity) && is.null(state$time_diag_error)) shiny::div(class="ec-empty ec-empty-small", "No temporal diagnostic has been run yet. Choose only the checks needed for your empirical question.")
      )
    })

    shiny::observeEvent(input$run_time_bg, {
      if (!diagnostic_ready()) return()
      if (is.null(state$fit) || !identical(state$fit$analysis_type, "time_series")) return()
      state$time_diag_error <- NULL
      ans <- tryCatch(eco_time_diagnostics(state$fit, bg_order=input$time_bg_order), error=function(e)e)
      if (inherits(ans,"error")) state$time_diag_error <- conditionMessage(ans) else state$time_diag <- ans
    })

    shiny::observeEvent(input$run_stationarity, {
      if (!diagnostic_ready()) return()
      if (is.null(state$fit) || !identical(state$fit$analysis_type, "time_series")) return()
      state$time_diag_error <- NULL
      vars <- if (is.null(input$time_stationarity_vars)) character() else input$time_stationarity_vars
      if (!length(vars)) { state$time_diag_error <- "Choose at least one series for ADF/KPSS diagnostics."; return() }
      dat <- state$run_data; ans <- tryCatch(eco_stationarity_tests(dat, vars, state$fit$time_variable, adf_k=input$time_adf_k, adf_deterministic=input$time_adf_deterministic, kpss_null=input$time_kpss_null), error=function(e)e)
      if (inherits(ans,"error")) state$time_diag_error <- conditionMessage(ans) else state$stationarity <- ans
    })

    output$system_diag_ui <- shiny::renderUI({
      if (is.null(state$fit) || !identical(state$fit$analysis_type, "time_series_system")) return(NULL)
      vars <- state$fit$variables
      shiny::tagList(
        shiny::div(class="ec-diag-toolbar",
          shiny::numericInput("system_serial_lags", "Multivariate residual-test lag order", value=8, min=1, step=1, width="280px"),
          shiny::actionButton("run_system_diag", "Run system diagnostics"),
          shiny::selectizeInput("system_stationarity_vars", "Series to test with ADF/KPSS", choices=stats::setNames(vars, vars), selected=vars[1L], multiple=TRUE, width="360px", options=list(plugins=list("remove_button"))),
          shiny::numericInput("system_adf_k", "ADF lag order (k)", value=1, min=0, step=1, width="220px"),
          shiny::selectInput("system_adf_deterministic", "ADF deterministic component", choices=c("No deterministic term"="none","Intercept / drift"="drift","Intercept + trend"="trend"), selected="drift", width="260px"),
          shiny::selectInput("system_kpss_null", "KPSS null", choices=c("Level stationarity"="Level","Trend stationarity"="Trend"), selected="Level", width="240px"),
          shiny::actionButton("run_system_stationarity", "Run ADF + KPSS")
        ),
        shiny::p(class="ec-note", "VAR/VECM diagnostics are system-level checks. The Portmanteau test concerns residual serial correlation in the joint system. ADF/KPSS remain series-level diagnostics and do not mechanically determine whether VAR or VECM is appropriate."),
        shiny::uiOutput("system_diag_results")
      )
    })
    output$system_diag_results <- shiny::renderUI({
      if (is.null(state$fit)) return(NULL)
      shiny::tagList(
        if (!is.null(state$system_diag_error)) shiny::div(class="ec-status ec-warn", state$system_diag_error),
        if (is.list(state$system_diag)) shiny::tagList(
          shiny::h4("Companion roots"),
          shiny::p(class="ec-note", state$system_diag$root_note),
          if (is.data.frame(state$system_diag$roots) && nrow(state$system_diag$roots)) shiny::div(class="ec-table-wrap ec-table", shiny::HTML(.ec_html_table(state$system_diag$roots))),
          shiny::h4("Residual serial correlation"),
          shiny::div(class="ec-table-wrap ec-table", shiny::HTML(.ec_html_table(state$system_diag$serial_correlation)))
        ),
        if (is.data.frame(state$stationarity) && nrow(state$stationarity)) shiny::tagList(
          shiny::h4("Series stationarity diagnostics"),
          shiny::div(class="ec-table-wrap ec-table", shiny::HTML(.ec_html_table(state$stationarity)))
        ),
        if (identical(state$fit$temporal_family, "vecm") && is.data.frame(state$fit$johansen_table)) shiny::tagList(
          shiny::h4("Johansen rank evidence"),
          shiny::p(class="ec-note", "The fitted rank is researcher-specified. This table reports the Johansen statistics used as evidence; econcompare does not overwrite the chosen rank."),
          shiny::div(class="ec-table-wrap ec-table", shiny::HTML(.ec_html_table(state$fit$johansen_table)))
        )
      )
    })

    shiny::observeEvent(input$run_system_diag, {
      if (!diagnostic_ready()) return()
      if (is.null(state$fit) || !identical(state$fit$analysis_type, "time_series_system")) return()
      state$system_diag_error <- NULL
      ans <- tryCatch(eco_system_diagnostics(state$fit, serial_lags=input$system_serial_lags), error=function(e)e)
      if (inherits(ans,"error")) state$system_diag_error <- conditionMessage(ans) else state$system_diag <- ans
    })

    shiny::observeEvent(input$run_system_stationarity, {
      if (!diagnostic_ready()) return()
      if (is.null(state$fit) || !identical(state$fit$analysis_type, "time_series_system")) return()
      state$system_diag_error <- NULL
      vars <- if (is.null(input$system_stationarity_vars)) character() else input$system_stationarity_vars
      if (!length(vars)) { state$system_diag_error <- "Choose at least one system series for ADF/KPSS diagnostics."; return() }
      ans <- tryCatch(eco_stationarity_tests(state$run_data, vars, state$fit$time_variable, adf_k=input$system_adf_k, adf_deterministic=input$system_adf_deterministic, kpss_null=input$system_kpss_null), error=function(e)e)
      if (inherits(ans,"error")) state$system_diag_error <- conditionMessage(ans) else state$stationarity <- ans
    })

    panel_model <- shiny::reactive({
      if (!inherits(state$fit, "econcompare_panel")) return(NULL)
      nm <- input$panel_diag_model
      if (is.null(nm)) return(names(state$fit$models)[1L])
      if (length(nm) != 1L || !nm %in% names(state$fit$models)) return(NULL)
      nm
    })
    output$panel_diag_controls <- shiny::renderUI({
      nm <- panel_model(); shiny::req(nm)
      cap <- .ec_panel_model_capabilities(state$fit, nm)
      available <- unique(cap$test[cap$selectable])
      limits <- cap[!cap$selectable, c("model", "test", "reason"), drop = FALSE]
      labels <- .ec_panel_test_catalogue()[available]
      if (any(cap$audit_only & cap$selectable)) labels[available == "dependence"] <- paste(labels[available == "dependence"], "— audit only; conclusion suspended")
      shiny::tagList(
        shiny::p(class = "ec-note", "Tests apply to the selected model. FE/pooled and FE/RE comparisons use the required companion model from this run."),
        if (length(available)) shiny::checkboxGroupInput("panel_tests", "Tests",
          choices = stats::setNames(available, unname(labels)), selected = character()),
        if ("serial" %in% available) shiny::numericInput("panel_serial_order", "Serial order", 1, min = 1, step = 1),
        if (length(available)) shiny::actionButton("panel_run_diagnostics", "Run selected tests", class = "btn-primary"),
        if (nrow(limits)) shiny::tags$details(class = "ec-raw", open = "open",
          shiny::tags$summary("Diagnostic limitations"), .ec_panel_table_ui(limits)))
    })

    shiny::observeEvent(input$panel_run_diagnostics, {
      if (!diagnostic_ready()) return()
      shiny::req(state$fit)
      if (!inherits(state$fit, "econcompare_panel")) return()
      if (!length(input$panel_tests)) {
        shiny::showNotification("Select at least one diagnostic question.", type = "message")
        return()
      }
      state$panel_diag <- tryCatch(.ec_panel_diagnostics_for_model(state$fit, panel_model(), tests = input$panel_tests,
        serial_order = if (is.null(input$panel_serial_order)) 1L else input$panel_serial_order), error = function(e) e)
    })
    shiny::observeEvent(list(input$panel_diag_model, input$panel_tests, input$panel_serial_order), {
      state$panel_diag <- NULL
    }, ignoreInit = TRUE, priority = 10)
    shiny::observeEvent(input$time_bg_order, { state$time_diag <- NULL }, ignoreInit = TRUE, priority = 10)
    shiny::observeEvent(input$system_serial_lags, { state$system_diag <- NULL }, ignoreInit = TRUE, priority = 10)
    shiny::observeEvent(list(input$time_stationarity_vars, input$time_adf_k, input$time_adf_deterministic,
      input$time_kpss_null, input$system_stationarity_vars, input$system_adf_k,
      input$system_adf_deterministic, input$system_kpss_null), {
      state$stationarity <- NULL
    }, ignoreInit = TRUE, priority = 10)

    output$panel_diag_table <- shiny::renderUI({
      if (is.null(state$panel_diag)) {
        if (!inherits(state$fit, "econcompare_panel")) return(NULL)
        return(shiny::p(class = "ec-note", .ec_panel_diagnostic_prompt(state$fit, panel_model())))
      }
      if (inherits(state$panel_diag, "error")) return(shiny::div(class = "ec-status ec-error", conditionMessage(state$panel_diag)))
      .ec_panel_diagnostics_ui(state$panel_diag)
    })

    output$results_ui <- shiny::renderUI({
      if (is.null(state$fit)) {
        return(shiny::div(class = "ec-empty",
          shiny::tags$b("Econometric model comparison workspace"),
          "Configure and compare econometric models that match the observational structure and empirical question you are studying.",
          shiny::tags$ol(
            shiny::tags$li("Choose an outcome variable and a small set of regressors."),
            shiny::tags$li("Choose whether the outcome is continuous, binary, nominal categorical or ordinal categorical."),
            shiny::tags$li("Configure the relevant parameters and click Run comparison."),
            shiny::tags$li("Read coefficients side by side, then inspect fit, diagnostics and warnings.")
          )
        ))
      }
      if (inherits(state$fit, "econcompare_panel")) return(shiny::tagList(
        shiny::p(class = "ec-note", paste("Estimated formula:", paste(deparse(state$fit$formula), collapse = " "),
          "| Individual index:", state$fit$panel_id, "| Time index:", state$fit$time_variable)),
        .ec_panel_results_ui(state$fit)))
      cmp <- cmp_all()
      if (!nrow(cmp)) {
        return(shiny::tagList(
          shiny::div(class = "ec-status ec-error", "The models were estimated, but no model result could be extracted safely."),
          if (nrow(state$extraction_failures)) shiny::div(class = "ec-table-wrap", shiny::HTML(.ec_html_table(state$extraction_failures)))
        ))
      }
  
      fit_tab <- .ec_model_fit_table(cmp, state$fit$outcome_type)
      fit_show <- fit_tab
      num <- vapply(fit_show, is.numeric, logical(1)); fit_show[num] <- lapply(fit_show[num], .ec_fmt)
      binary_tab <- if (identical(state$fit$outcome_type, "binary")) tryCatch(eco_binary_compare(state$fit), error = function(e) data.frame()) else data.frame()
      binary_show <- binary_tab
      if (nrow(binary_show)) {
        bnum <- vapply(binary_show, is.numeric, logical(1))
        binary_show[bnum] <- lapply(binary_show[bnum], .ec_fmt)
      }
      coef_body <- if (identical(state$fit$outcome_type, "ordinal")) {
        parts <- .ec_split_ordinal_compare(cmp)
        shiny::tagList(
          shiny::h4("Slope coefficients"),
          shiny::p(class = "ec-note", "These coefficients describe how regressors shift the model's latent index. Ordered logit and ordered probit use different latent scales, so their raw magnitudes should not be compared directly."),
          if (nrow(parts$slopes)) shiny::div(class = "ec-table-wrap", .ec_coef_shiny_table(parts$slopes)),
          if (nrow(parts$thresholds)) shiny::tags$details(
            class = "ec-raw",
            shiny::tags$summary("Category thresholds · technical parameters"),
            shiny::p(class = "ec-note", "Thresholds delimit adjacent outcome categories on the latent scale. They are not regressor effects and are shown separately to reduce interpretation errors."),
            shiny::div(class = "ec-table-wrap", .ec_coef_shiny_table(parts$thresholds))
          )
        )
      } else {
        shiny::div(class = "ec-table-wrap", .ec_coef_shiny_table(cmp))
      }
      shiny::tabsetPanel(
        shiny::tabPanel("Coefficients",
          shiny::p(class = "ec-note", if (identical(state$fit$analysis_type, "time_series_system")) "Coefficients are grouped by equation within one jointly specified dynamic system. Equations are not competing standalone models." else "Coefficient statistics are grouped by model. Compare raw magnitudes only when the models use a comparable coefficient scale; logit, probit, LPM, multinomial and ordered coefficients are not interchangeable."),
          if (identical(state$fit$outcome_type, "binary")) shiny::div(class = "ec-tip-box",
            shiny::strong("Binary comparison · "),
            "For LPM, logit and probit, use predicted outcomes and common descriptive metrics for comparison rather than ranking models by raw coefficient magnitude."
          ),
          coef_body
        ),
        if (identical(state$fit$outcome_type, "binary")) shiny::tabPanel("Binary comparison",
          shiny::p(class = "ec-note", "These are in-sample descriptive summaries, not an automatic model-selection rule. Accuracy uses a 0.5 threshold. The Brier score is reported only when fitted values stay within [0,1]; this matters for the LPM."),
          if (nrow(binary_show)) shiny::div(class = "ec-table-wrap ec-table", shiny::HTML(.ec_html_table(binary_show))) else shiny::p(class = "ec-note", "Common binary prediction summaries are unavailable for the fitted models.")
        ),
        shiny::tabPanel("Model fit",
          shiny::p(class = "ec-note", .ec_model_fit_note(state$fit$outcome_type)),
          if (identical(state$fit$analysis_type, "time_series") && !isTRUE(state$fit$sample_comparable)) shiny::div(class="ec-status ec-warn", "Compared temporal models use different effective samples because lagging/missingness differs. Direct AIC/BIC comparisons can be misleading unless the estimation sample is aligned."),
          if (identical(state$fit$analysis_type, "time_series") && !identical(state$fit$temporal_family, "ecm")) shiny::div(class="ec-status ec-warn", "Time-series level regressions can be spurious when persistent/non-stationary series share trends. Review stationarity and, when relevant, cointegration before interpreting conventional significance as evidence of a stable economic relationship."),
          if (identical(state$fit$temporal_family, "ecm")) shiny::div(class="ec-status ec-warn", "The ECM is conditional on a defensible long-run relationship. The first-step levels regression is not, by itself, evidence of cointegration; second-step inference is conditional on the constructed error-correction term."),
          shiny::div(class = "ec-table-wrap ec-table", shiny::HTML(.ec_html_table(fit_show)))
        ),
        if (identical(state$fit$temporal_family, "ecm")) shiny::tabPanel("ECM long run",
          shiny::p(class="ec-note", "This is the first-step levels relationship used to construct ECT. Its coefficient table is shown separately from the short-run ECM."),
          {
            lr <- tryCatch(eco_ecm_long_run(state$fit), error=function(e) data.frame())
            if (nrow(lr)) shiny::div(class="ec-table-wrap ec-table", shiny::HTML(.ec_html_table(lr))) else shiny::p(class="ec-note", "Long-run table unavailable.")
          }
        ),
        if (identical(state$fit$temporal_family, "vecm")) shiny::tabPanel("Cointegration",
          shiny::p(class="ec-note", "Cointegrating vectors are normalization-dependent. Signs and scale are not unique; interpret the normalized long-run relations rather than the normalization itself."),
          {
            bt <- tryCatch(as.data.frame(eco_vecm_cointegration(state$fit)), error=function(e) data.frame())
            if (nrow(bt)) shiny::div(class="ec-table-wrap ec-table", shiny::HTML(.ec_html_table(cbind(term=rownames(bt), bt)))) else shiny::p(class="ec-note", "Cointegrating-vector table unavailable.")
          }
        ),
        shiny::tabPanel(if (isTRUE(state$fit$analysis_type %in% c("time_series", "time_series_system"))) "Diagnostics" else "Cross-section diagnostics",
          if (identical(state$fit$analysis_type, "time_series")) shiny::tagList(
            shiny::p(class = "ec-note", "Temporal diagnostics are deliberately user-controlled. Run Breusch-Godfrey for residual serial correlation and complementary ADF/KPSS diagnostics on the series you select. ADF uses the explicit deterministic specification you choose; no transformation is performed automatically."),
            shiny::uiOutput("time_diag_ui")
          ) else if (identical(state$fit$analysis_type, "time_series_system")) shiny::tagList(
            shiny::p(class="ec-note", "System diagnostics are deliberately user-controlled. Inspect companion roots, multivariate residual serial correlation, series-level stationarity evidence, and for VECM the retained Johansen rank evidence."),
            shiny::uiOutput("system_diag_ui")
          ) else shiny::tagList(
          shiny::div(class = "ec-diag-toolbar",
            shiny::selectInput("diag_model", "Model to diagnose", choices = names(state$fit$models), selected = names(state$fit$models)[1L], width = "320px")
          ),
          shiny::uiOutput("diag_controls_ui"),
          shiny::uiOutput("diag_model_ui")
          )
        ),
        if (isTRUE(state$fit$analysis_type %in% c("time_series", "time_series_system"))) shiny::tabPanel("Time structure",
          shiny::p(class="ec-note", "The time index is validated before estimation. Missing/duplicated periods and internal calendar gaps are not silently repaired. Lagged models require a complete regular grid."),
          shiny::div(class="ec-table-wrap ec-table", shiny::HTML(.ec_html_table(state$fit$time_audit)))
        ),
        shiny::tabPanel("Sample audit",
          shiny::p(class = "ec-note", "Model differences are harder to interpret when estimators use different observations. The first successfully fitted model is used only as a technical sample anchor, not as an econometric reference model."),
          {
            sa <- sample_all()
            show_cols <- intersect(c("model", "engine", "n_original", "n_used", "n_effective", "zero_weight_n", "n_dropped", "row_identity", "row_identity_source", "reference_model", "matches_reference_n", "matches_reference_rows", "comparison_certainty", "sample_match", "sample_warning"), names(sa))
            shiny::div(class = "ec-table-wrap ec-table", shiny::HTML(.ec_html_table(sa[, show_cols, drop = FALSE])))
          }
        ),
        shiny::tabPanel("Warnings / failures",
          shiny::p(class = "ec-note", "Estimation and result-extraction issues are attached to the model that generated them. A failing alternative model or a failing extraction does not discard successful results from other models."),
          shiny::h4("Estimation warnings"),
          if (is.data.frame(state$warnings) && nrow(state$warnings)) shiny::div(class = "ec-table-wrap", shiny::HTML(.ec_html_table(state$warnings))) else shiny::p(class = "ec-note", "No estimation warning was captured for this run."),
          if (nrow(state$extraction_warnings)) shiny::div(
            shiny::h4("Extraction warnings"),
            shiny::p(class = "ec-note", "These warnings occurred while building coefficient or fit summaries after estimation."),
            shiny::div(class = "ec-table-wrap", shiny::HTML(.ec_html_table(state$extraction_warnings)))
          ),
          if (!is.null(state$fit$failures) && nrow(state$fit$failures)) shiny::div(
            shiny::h4("Failed specifications"),
            shiny::div(class = "ec-table-wrap", shiny::HTML(.ec_html_table(state$fit$failures)))
          ),
          if (nrow(state$extraction_failures)) shiny::div(
            shiny::h4("Failed result extractions"),
            shiny::p(class = "ec-note", "The underlying model object is still retained and may remain usable outside the comparison table."),
            shiny::div(class = "ec-table-wrap", shiny::HTML(.ec_html_table(state$extraction_failures)))
          )
        )
      )
    })
  }
  
}
