.ec_parse_numeric_list <- function(x, default = NULL) {
  if (is.null(x) || !nzchar(trimws(x))) return(default)
  vals <- suppressWarnings(as.numeric(trimws(strsplit(x, ",", fixed = TRUE)[[1]])))
  vals <- vals[is.finite(vals)]
  if (!length(vals)) default else vals
}

.ec_parse_bound <- function(x, side = c("left", "right")) {
  side <- match.arg(side)
  if (is.null(x) || !nzchar(trimws(x))) return(if (side == "left") -Inf else Inf)
  z <- tolower(trimws(x))
  if (z %in% c("-inf", "-infinity")) return(-Inf)
  if (z %in% c("inf", "+inf", "infinity", "+infinity")) return(Inf)
  out <- suppressWarnings(as.numeric(z))
  if (is.na(out)) .ec_stop("Invalid ", side, " Tobit bound: `", x, "`.")
  out
}

.ec_formula <- function(y, x) {
  if (!length(x)) .ec_stop("Select at least one explanatory variable.")
  stats::reformulate(x, response = y)
}

.ec_help_icon <- function(text) {
  shiny::tags$span(
    class = "ec-q",
    `aria-label` = text,
    `data-tip` = text,
    tabindex = "0",
    "?"
  )
}

.ec_labeled <- function(label, tip = NULL, caption = NULL) {
  shiny::tagList(
    shiny::div(class = "ec-label-row",
      shiny::tags$span(class = "ec-label-text", label),
      if (!is.null(tip)) .ec_help_icon(tip)
    ),
    if (!is.null(caption)) shiny::div(class = "ec-field-caption", caption)
  )
}

.ec_choice_badges <- function(models) {
  if (!length(models)) {
    return(shiny::div(class = "ec-chip-wrap",
      shiny::tags$span(class = "ec-chip ec-chip-soft", "OLS reference is always included")
    ))
  }
  shiny::div(class = "ec-chip-wrap",
    shiny::tags$span(class = "ec-chip", "OLS"),
    lapply(models, function(m) shiny::tags$span(class = "ec-chip ec-chip-soft", m))
  )
}

.ec_preview_table <- function(data, n = 8L) {
  z <- utils::head(data, n)
  num <- vapply(z, is.numeric, logical(1))
  z[num] <- lapply(z[num], .ec_fmt)
  .ec_html_table(z)
}

.ec_app_css <- function() {
  paste0(
    ":root{",
      "--ec-bg:#f3f6fb;--ec-card:#ffffff;--ec-text:#132238;--ec-muted:#5f6f86;",
      "--ec-border:#dfe7f1;--ec-dark:#132238;--ec-soft:#eef4fb;--ec-accent:#1f6feb;",
      "--ec-accent-soft:#eaf2ff;--ec-success:#e8f7ee;--ec-shadow:0 10px 30px rgba(19,34,56,.08)",
    "}",
    "body{background:linear-gradient(180deg,#f7f9fd 0%,#f2f5fa 100%);color:var(--ec-text);font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif}",
    ".container-fluid{padding:0!important}",
    ".ec-shell{padding:22px;max-width:1680px;margin:auto}",
    ".ec-top{display:flex;justify-content:space-between;gap:18px;align-items:stretch;margin-bottom:18px}",
    ".ec-hero{flex:1;background:linear-gradient(135deg,#132238 0%,#1e3557 100%);color:#fff;border-radius:24px;padding:26px 28px;box-shadow:var(--ec-shadow)}",
    ".ec-title{font-size:34px;font-weight:820;letter-spacing:-.03em;line-height:1.05;margin:0}",
    ".ec-sub{opacity:.86;margin-top:8px;font-size:16px;max-width:760px}",
    ".ec-hero-pills{display:flex;flex-wrap:wrap;gap:8px;margin-top:18px}",
    ".ec-pill{display:inline-flex;align-items:center;gap:8px;padding:8px 12px;border-radius:999px;background:rgba(255,255,255,.12);font-size:12px;font-weight:700;backdrop-filter:blur(6px)}",
    ".ec-metric-stack{width:360px;display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:12px}",
    ".ec-metric{background:var(--ec-card);border:1px solid var(--ec-border);border-radius:20px;padding:18px 16px;box-shadow:var(--ec-shadow)}",
    ".ec-metric-k{font-size:24px;font-weight:820;letter-spacing:-.02em}",
    ".ec-metric-l{font-size:12px;color:var(--ec-muted);font-weight:700;text-transform:uppercase;letter-spacing:.04em;margin-top:4px}",
    ".ec-grid{display:grid;grid-template-columns:minmax(320px,390px) minmax(0,1fr);gap:18px;align-items:start}",
    ".ec-sidebar,.ec-main{background:var(--ec-card);border:1px solid var(--ec-border);border-radius:24px;box-shadow:var(--ec-shadow)}",
    ".ec-sidebar{padding:18px;position:sticky;top:12px}",
    ".ec-main{padding:18px;min-height:640px}",
    ".ec-section{padding:0 0 16px;margin-bottom:16px;border-bottom:1px solid var(--ec-border)}.ec-section:last-child{border-bottom:0;margin-bottom:0;padding-bottom:0}",
    ".ec-step{display:flex;align-items:center;gap:10px;margin-bottom:10px}",
    ".ec-step-n{display:inline-flex;align-items:center;justify-content:center;width:28px;height:28px;border-radius:50%;background:var(--ec-accent-soft);color:var(--ec-accent);font-weight:800;font-size:13px}",
    ".ec-h{font-weight:820;font-size:18px;letter-spacing:-.02em}",
    ".ec-help{font-size:13px;color:var(--ec-muted);margin:0 0 10px;line-height:1.5}",
    ".ec-mini-card{background:#f8fbff;border:1px solid var(--ec-border);border-radius:16px;padding:12px 14px;margin-top:12px}",
    ".ec-chip-wrap{display:flex;flex-wrap:wrap;gap:8px;margin-top:10px}",
    ".ec-chip{display:inline-flex;align-items:center;padding:6px 10px;border-radius:999px;background:var(--ec-dark);color:#fff;font-size:12px;font-weight:700}",
    ".ec-chip-soft{background:var(--ec-soft);color:var(--ec-text)}",
    ".ec-label-row{display:flex;align-items:center;gap:8px;margin-bottom:6px}",
    ".ec-label-text{font-weight:740;font-size:14px;color:var(--ec-text)}",
    ".ec-field-caption{font-size:12px;line-height:1.45;color:var(--ec-muted);margin:-2px 0 8px}",
    ".ec-q{display:inline-flex;align-items:center;justify-content:center;width:18px;height:18px;border-radius:50%;background:var(--ec-accent-soft);color:var(--ec-accent);font-size:11px;font-weight:900;cursor:help;position:relative;border:1px solid #cfe0ff}",
    ".ec-q:hover::after,.ec-q:focus::after{content:attr(data-tip);position:absolute;left:24px;top:-6px;min-width:260px;max-width:360px;background:#11243c;color:#fff;padding:10px 12px;border-radius:12px;font-size:12px;line-height:1.45;font-weight:500;box-shadow:0 12px 30px rgba(0,0,0,.18);z-index:30;white-space:normal}",
    ".ec-q:hover::before,.ec-q:focus::before{content:'';position:absolute;left:18px;top:5px;border-top:7px solid transparent;border-bottom:7px solid transparent;border-right:7px solid #11243c;z-index:31}",
    ".btn-primary{background:linear-gradient(135deg,#132238 0%,#27456f 100%)!important;border:0!important;border-radius:14px!important;font-weight:760!important;padding:11px 14px!important;box-shadow:0 10px 20px rgba(19,34,56,.14)!important}",
    ".btn-default{border-radius:14px!important;border:1px solid var(--ec-border)!important;background:#fff!important}",
    ".form-control,.selectize-input{border-radius:14px!important;border:1px solid var(--ec-border)!important;box-shadow:none!important;padding-top:10px!important;padding-bottom:10px!important}",
    ".selectize-dropdown,.dropdown-menu{border-radius:14px!important;border-color:var(--ec-border)!important;box-shadow:var(--ec-shadow)!important}",
    ".checkbox{margin-top:0!important;margin-bottom:8px!important}.checkbox label{line-height:1.45;color:var(--ec-text)}",
    ".nav-tabs{border-bottom:1px solid var(--ec-border);display:flex;gap:8px;flex-wrap:wrap}",
    ".nav-tabs>li>a{border:1px solid transparent!important;color:var(--ec-muted);font-weight:760;padding:10px 14px;border-radius:12px!important;background:transparent!important}",
    ".nav-tabs>li.active>a{color:var(--ec-text)!important;border-color:var(--ec-border)!important;background:#f9fbff!important;box-shadow:inset 0 -2px 0 var(--ec-accent)!important}",
    ".ec-empty{padding:84px 30px;text-align:center;color:var(--ec-muted)}.ec-empty b{display:block;color:var(--ec-text);font-size:22px;margin-bottom:10px}",
    ".ec-empty ol{display:inline-block;text-align:left;margin:10px auto 0;padding-left:22px;line-height:1.8}",
    ".ec-status{padding:12px 14px;border-radius:14px;background:var(--ec-soft);font-size:13px;margin-top:12px;white-space:pre-wrap;line-height:1.5;border:1px solid transparent}",
    ".ec-error{background:#fff1f2;color:#9f1239;border-color:#fecdd3}.ec-warn{background:#fffbeb;color:#92400e;border-color:#fde68a}.ec-ok{background:var(--ec-success);color:#116149;border-color:#bbf7d0}",
    ".ec-main-head{display:flex;justify-content:space-between;gap:12px;align-items:flex-start;margin-bottom:8px}",
    ".ec-main-title{font-size:20px;font-weight:800;letter-spacing:-.02em}",
    ".ec-main-sub{font-size:13px;color:var(--ec-muted);line-height:1.5;max-width:900px}",
    ".ec-note{font-size:12px;color:var(--ec-muted);margin:0 0 12px;line-height:1.5}",
    ".ec-table-wrap{overflow-x:auto}",
    ".ec-table,.ec-table table{border-collapse:separate;border-spacing:0;width:100%;font-size:13px;min-width:720px}",
    ".ec-table th,.ec-table td,.ec-table table th,.ec-table table td{padding:10px 10px;border-bottom:1px solid var(--ec-border);white-space:nowrap}",
    ".ec-table th,.ec-table table th{background:#f8fbff;font-weight:780;position:sticky;top:0;z-index:1}",
    ".ec-table td.num{text-align:right;font-variant-numeric:tabular-nums}",
    ".coef-table .model-group{text-align:center;border-left:2px solid #d7dce3;background:#eef4fb}",
    ".coef-table .model-start{border-left:2px solid #d7dce3}",
    ".coef-table .term-head,.coef-table .term{position:sticky;left:0;background:white;font-weight:720;z-index:2}",
    ".coef-table .term-head{background:#f8fbff;z-index:4}",
    ".coef-table tbody tr:hover td{background:#f7fbff}.coef-table tbody tr:hover .term{background:#f7fbff}",
    ".ec-kicker{font-size:11px;text-transform:uppercase;letter-spacing:.08em;color:var(--ec-muted);font-weight:800;margin-bottom:6px}",
    ".ec-tip-box{background:#f8fbff;border:1px dashed #c8d8ef;border-radius:16px;padding:12px 14px;font-size:12px;color:var(--ec-muted);line-height:1.55;margin-top:8px}",
    "@media(max-width:1080px){.ec-top{flex-direction:column}.ec-metric-stack{width:auto;grid-template-columns:repeat(2,minmax(0,1fr))}.ec-grid{grid-template-columns:1fr}.ec-sidebar{position:static}}",
    "@media(max-width:640px){.ec-shell{padding:12px}.ec-hero{padding:20px}.ec-title{font-size:28px}.ec-metric-stack{grid-template-columns:1fr 1fr}}"
  )
}

.ec_coef_shiny_table <- function(df) {
  shiny::HTML(.ec_coef_html_table(df))
}

#' Launch the interactive econcompare model laboratory
#'
#' @param data A data.frame to analyse. For example `mtcars`.
#' @param launch.browser Passed to [shiny::runApp()]. In RStudio, the default
#'   opens the application in the Viewer pane when available.
#' @return A Shiny application object, invisibly after the app exits.
#' @export
#' @examples
#' \dontrun{
#' eco_app(mtcars)
#' }
eco_app <- function(data, launch.browser = getOption("shiny.launch.browser", interactive())) {
  .ec_require("shiny", "interactive app")
  if (missing(data) || !is.data.frame(data)) .ec_stop("`data` must be supplied as a data.frame, e.g. eco_app(mtcars).")

  data <- as.data.frame(data)
  all_vars <- names(data)
  numeric_vars <- all_vars[vapply(data, is.numeric, logical(1))]
  binary_candidates <- all_vars[vapply(data, function(x) {
    ux <- unique(stats::na.omit(x))
    length(ux) > 1 && length(ux) <= 2
  }, logical(1))]
  if (length(all_vars) < 2L) .ec_stop("`data` needs at least two columns.")

  registry <- .ec_registry()
  optional_engines <- setdiff(registry$engine, "ols")
  labels <- stats::setNames(optional_engines, registry$estimator[match(optional_engines, registry$engine)])

  ui <- shiny::fluidPage(
    shiny::tags$head(shiny::tags$style(shiny::HTML(.ec_app_css()))),
    shiny::div(class = "ec-shell",
      shiny::div(class = "ec-top",
        shiny::div(class = "ec-hero",
          shiny::div(class = "ec-kicker", "Interactive econometrics workspace"),
          shiny::h1(class = "ec-title", "econcompare lab"),
          shiny::div(class = "ec-sub",
            "Build an OLS reference model, add econometric alternatives, and compare them visually. The interface combines a professional modelling workflow with concise methodological guidance so that each specification choice remains transparent."
          ),
          shiny::div(class = "ec-hero-pills",
            shiny::div(class = "ec-pill", "OLS as reference"),
            shiny::div(class = "ec-pill", "Side-by-side coefficients"),
            shiny::div(class = "ec-pill", "Interactive parameter guidance"),
            shiny::div(class = "ec-pill", "Warnings captured explicitly")
          )
        ),
        shiny::div(class = "ec-metric-stack",
          shiny::div(class = "ec-metric", shiny::div(class = "ec-metric-k", nrow(data)), shiny::div(class = "ec-metric-l", "Rows")),
          shiny::div(class = "ec-metric", shiny::div(class = "ec-metric-k", ncol(data)), shiny::div(class = "ec-metric-l", "Variables")),
          shiny::div(class = "ec-metric", shiny::div(class = "ec-metric-k", length(numeric_vars)), shiny::div(class = "ec-metric-l", "Numeric variables")),
          shiny::div(class = "ec-metric", shiny::div(class = "ec-metric-k", "Cross-section"), shiny::div(class = "ec-metric-l", "Current mode"))
        )
      ),
      shiny::div(class = "ec-grid",
        shiny::div(class = "ec-sidebar",
          shiny::div(class = "ec-section",
            shiny::div(class = "ec-step", shiny::div(class = "ec-step-n", "1"), shiny::div(class = "ec-h", "Choose variables")),
            shiny::p(class = "ec-help", "Start with your outcome variable, then choose the regressors you want to compare across models."),
            shiny::selectInput(
              "y",
              .ec_labeled(
                "Dependent variable",
                "This is the outcome you want to explain. In an OLS interpretation, coefficients measure how each explanatory variable is associated with this variable, holding the others constant.",
                "Example: choose mpg if you want to explain fuel efficiency."
              ),
              choices = all_vars,
              selected = all_vars[1]
            ),
            shiny::selectizeInput(
              "x",
              .ec_labeled(
                "Explanatory variables",
                "These are the covariates used on the right-hand side of the regression. Select the variables you want to compare across the different estimators.",
                "Tip: start with a small set of meaningful regressors, then expand gradually."
              ),
              choices = setdiff(all_vars, all_vars[1]),
              multiple = TRUE,
              options = list(plugins = list("remove_button"))
            )
          ),
          shiny::div(class = "ec-section",
            shiny::div(class = "ec-step", shiny::div(class = "ec-step-n", "2"), shiny::div(class = "ec-h", "Pick comparison models")),
            shiny::p(class = "ec-help", "OLS is included automatically. Add only the models you want to compare against the OLS baseline."),
            shiny::checkboxGroupInput("models", NULL, choices = labels),
            shiny::uiOutput("model_badges"),
            shiny::div(class = "ec-tip-box",
              shiny::strong("Econometric note · "),
              "Compare models when they answer nearly the same empirical question. A change in coefficient size or significance is often more informative than the fit statistic alone."
            )
          ),
          shiny::uiOutput("model_options"),
          shiny::div(class = "ec-section",
            shiny::div(class = "ec-step", shiny::div(class = "ec-step-n", "4"), shiny::div(class = "ec-h", "Run")),
            shiny::p(class = "ec-help", "Estimate the selected models and open a side-by-side comparison."),
            shiny::div(style = "display:flex; gap:10px;",
              shiny::actionButton("run", "Run comparison", class = "btn-primary", width = "100%"),
              shiny::actionButton("reset", "Reset", width = "110px")
            ),
            shiny::uiOutput("run_status")
          )
        ),
        shiny::div(class = "ec-main",
          shiny::div(class = "ec-main-head",
            shiny::div(
              shiny::div(class = "ec-main-title", "Comparison workspace"),
              shiny::div(class = "ec-main-sub",
                "Use the tabs to inspect coefficients, fit statistics, diagnostics, warnings and a quick data preview. Hover over the question-mark helpers in the sidebar whenever you want guidance on a modelling option."
              )
            ),
            shiny::uiOutput("run_meta")
          ),
          shiny::uiOutput("results_ui")
        )
      )
    )
  )

  server <- function(input, output, session) {
    shiny::observeEvent(input$y, {
      shiny::updateSelectizeInput(session, "x", choices = setdiff(all_vars, input$y), server = TRUE)
    }, ignoreInit = FALSE)

    shiny::observeEvent(input$reset, {
      shiny::updateSelectInput(session, "y", selected = all_vars[1])
      shiny::updateSelectizeInput(session, "x", selected = character(0), choices = setdiff(all_vars, all_vars[1]), server = TRUE)
      shiny::updateCheckboxGroupInput(session, "models", selected = character(0))
      state$fit <- NULL
      state$error <- NULL
      state$warnings <- character()
      state$ran <- FALSE
    })

    output$model_badges <- shiny::renderUI({
      .ec_choice_badges(input$models)
    })

    output$model_options <- shiny::renderUI({
      mods <- input$models
      if (!length(mods)) return(NULL)
      blocks <- list(
        shiny::div(class = "ec-section",
          shiny::div(class = "ec-step", shiny::div(class = "ec-step-n", "3"), shiny::div(class = "ec-h", "Set model parameters")),
          shiny::p(class = "ec-help", "Only the controls relevant to your selected models are shown below.")
        )
      )

      if ("wls" %in% mods) {
        blocks <- c(blocks, list(
          shiny::selectInput(
            "wls_weight",
            .ec_labeled(
              "WLS · weight variable",
              "We use WLS to correct for heteroscedasticity by giving more weight to precise observations and less weight to noisy ones, so the estimator becomes more efficient when variance differs across observations.",
              "Choose a numeric variable containing the weights to be used in weighted least squares."
            ),
            choices = numeric_vars
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
        blocks <- c(blocks, list(
          shiny::selectInput(
            "selection_y",
            .ec_labeled(
              "Heckman · selection indicator",
              "This variable describes whether an observation is selected into the outcome equation. In a sample-selection setting, the outcome is observed only for selected units.",
              "A binary indicator is usually the most natural choice here."
            ),
            choices = if (length(binary_candidates)) binary_candidates else setdiff(all_vars, input$y)
          ),
          shiny::selectizeInput(
            "selection_x",
            .ec_labeled(
              "Heckman · selection regressors",
              "These variables explain the selection process. Ideally, at least one variable affects selection but does not enter the outcome equation directly.",
              "Adding an exclusion restriction generally helps identification."
            ),
            choices = setdiff(all_vars, input$y),
            multiple = TRUE
          ),
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

    state <- shiny::reactiveValues(fit = NULL, error = NULL, warnings = character(), ran = FALSE)

    shiny::observeEvent(input$run, {
      state$ran <- TRUE
      state$error <- NULL
      state$warnings <- character()
      state$fit <- NULL

      tryCatch({
        y <- input$y
        x <- input$x
        if (!length(y) || !length(x)) .ec_stop("Choose one dependent variable and at least one explanatory variable.")

        mods <- unique(c("ols", input$models))
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
          args$quantile <- list(tau = taus)
        }
        if ("ivreg" %in% mods) {
          endog <- input$iv_endog
          inst <- input$iv_instruments
          if (!length(endog)) .ec_stop("IV/2SLS requires at least one endogenous regressor.")
          if (!length(inst)) .ec_stop("IV/2SLS requires at least one excluded instrument.")
          exog <- setdiff(x, endog)
          rhs_inst <- unique(c(exog, inst))
          iv_formula <- stats::as.formula(paste(y, "~", paste(x, collapse = " + "), "|", paste(rhs_inst, collapse = " + ")))
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
          if (!length(sy) || !length(sx)) .ec_stop("Heckman requires a selection indicator and at least one selection regressor.")
          selection_formula <- .ec_formula(sy, sx)
          args$heckman <- list(method = input$heckman_method)
        }

        fml <- .ec_formula(y, x)
        captured <- character()
        fit <- withCallingHandlers(
          eco_run(data = data, formula = fml, models = mods, model_args = args,
                  iv_formula = iv_formula, selection_formula = selection_formula, outcome_formula = fml),
          warning = function(w) {
            captured <<- c(captured, conditionMessage(w))
            invokeRestart("muffleWarning")
          }
        )
        state$fit <- fit
        state$warnings <- unique(captured)
      }, error = function(e) {
        state$error <- conditionMessage(e)
      })
    })

    output$run_status <- shiny::renderUI({
      if (!state$ran) return(NULL)
      if (!is.null(state$error)) return(shiny::div(class = "ec-status ec-error", state$error))
      if (length(state$warnings)) return(shiny::div(class = "ec-status ec-warn", paste("Model warnings:", paste0("\n• ", state$warnings, collapse = ""))))
      if (!is.null(state$fit)) return(shiny::div(class = "ec-status ec-ok", paste(length(state$fit$models), "model result(s) ready.")))
      NULL
    })

    output$run_meta <- shiny::renderUI({
      if (is.null(state$fit)) return(NULL)
      shiny::div(class = "ec-chip-wrap",
        shiny::tags$span(class = "ec-chip ec-chip-soft", paste(length(state$fit$models), "estimated result(s)")),
        shiny::tags$span(class = "ec-chip ec-chip-soft", paste("Generated", format(Sys.time(), "%H:%M")))
      )
    })

    output$results_ui <- shiny::renderUI({
      if (is.null(state$fit)) {
        return(shiny::div(class = "ec-empty",
          shiny::tags$b("Econometric model comparison workspace"),
          "Configure, estimate and audit OLS-referenced cross-sectional specifications from one reproducible workspace.",
          shiny::tags$ol(
            shiny::tags$li("Choose an outcome variable and a small set of regressors."),
            shiny::tags$li("Add one or several econometric alternatives to the OLS baseline."),
            shiny::tags$li("Configure the relevant parameters and click Run comparison."),
            shiny::tags$li("Read coefficients side by side, then inspect fit, diagnostics and warnings.")
          )
        ))
      }
      cmp <- tryCatch(eco_compare(state$fit), error = function(e) NULL)
      if (is.null(cmp)) return(shiny::div(class = "ec-status ec-error", "The models were estimated but their results could not be extracted."))

      fit_cols <- intersect(c("model", "engine", "family", "nobs", "r2", "adj_r2", "logLik", "aic", "bic", "deviance", "unavailable_stats"), names(cmp))
      fit_tab <- unique(cmp[fit_cols, drop = FALSE])
      num <- vapply(fit_tab, is.numeric, logical(1)); fit_tab[num] <- lapply(fit_tab[num], .ec_fmt)
      diag <- eco_diagnostics(state$fit); dn <- vapply(diag, is.numeric, logical(1)); diag[dn] <- lapply(diag[dn], .ec_fmt)

      shiny::tabsetPanel(
        shiny::tabPanel("Coefficients",
          shiny::p(class = "ec-note", "Each variable is shown once. For each model, coefficient statistics are grouped together so that differences are easy to compare visually."),
          shiny::div(class = "ec-table-wrap ec-table", .ec_coef_shiny_table(cmp))
        ),
        shiny::tabPanel("Model fit",
          shiny::p(class = "ec-note", "NA means that the statistic is not available for that estimator. Use unavailable_stats to distinguish truly unavailable quantities from measured ones. Fit measures should be compared only when they are defined on a compatible basis."),
          shiny::div(class = "ec-table-wrap ec-table", shiny::HTML(.ec_html_table(fit_tab)))
        ),
        shiny::tabPanel("Diagnostics",
          shiny::p(class = "ec-note", "Diagnostics help you understand why a coefficient or standard error changed relative to the OLS benchmark."),
          shiny::div(class = "ec-table-wrap ec-table", shiny::HTML(.ec_html_table(diag)))
        ),
        shiny::tabPanel("Warnings",
          shiny::p(class = "ec-note", "Warnings come from the underlying estimators. They are preserved so that analysts can relate numerical or inferential issues to the specification that generated them."),
          if (length(state$warnings)) shiny::tags$ul(lapply(state$warnings, shiny::tags$li)) else shiny::p(class = "ec-note", "No estimation warning was captured for this run.")
        ),
        shiny::tabPanel("Data preview",
          shiny::p(class = "ec-note", "A quick look at the first rows of the dataset currently loaded in the app."),
          shiny::div(class = "ec-table-wrap ec-table", shiny::HTML(.ec_preview_table(data)))
        )
      )
    })
  }

  app <- shiny::shinyApp(ui = ui, server = server)
  shiny::runApp(app, launch.browser = launch.browser)
  invisible(app)
}
