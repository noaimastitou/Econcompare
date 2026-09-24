.ec_app_ui <- function(data, all_vars, numeric_vars, initial_y, initial_type, structure_info, initial_mode, initial_time) {
  shiny::fluidPage(
    shiny::tags$head(shiny::tags$style(shiny::HTML(.ec_app_css())), shiny::tags$script(shiny::HTML(.ec_app_tooltip_js()))),
    shiny::div(class = "ec-shell",
      shiny::div(class = "ec-top",
        shiny::div(class = "ec-hero",
          shiny::div(class = "ec-kicker", "Interactive econometrics workspace"),
          shiny::h1(class = "ec-title", "econcompare lab"),
          shiny::div(class = "ec-sub",
            "Explore the data, identify its observational structure, and compare econometric models suited to cross-sectional, explanatory time-series or static panel analysis. Time-series tools focus on relationships among variables over time, not forecasting."
          ),
          shiny::div(class = "ec-hero-pills",
            shiny::div(class = "ec-pill", "Cross-section + time-series + panel"),
            shiny::div(class = "ec-pill", "Side-by-side coefficients"),
            shiny::div(class = "ec-pill", "Explore data first"),
            shiny::div(class = "ec-pill", "Warnings captured explicitly")
          )
        ),
        shiny::div(class = "ec-metric-stack",
          shiny::div(class = "ec-metric", shiny::div(class = "ec-metric-k", nrow(data)), shiny::div(class = "ec-metric-l", "Rows")),
          shiny::div(class = "ec-metric", shiny::div(class = "ec-metric-k", ncol(data)), shiny::div(class = "ec-metric-l", "Variables")),
          shiny::div(class = "ec-metric", shiny::div(class = "ec-metric-k", length(numeric_vars)), shiny::div(class = "ec-metric-l", "Numeric variables")),
          shiny::div(class = "ec-metric", shiny::div(class = "ec-metric-k", if (identical(initial_mode, "time_series")) "Time series" else "Cross-section"), shiny::div(class = "ec-metric-l", "Detected mode"))
        )
      ),
      shiny::div(class = "ec-grid",
        shiny::div(class = "ec-sidebar",
          shiny::div(class = "ec-section",
            shiny::div(class = "ec-step", shiny::div(class = "ec-step-n", "1"), shiny::div(class = "ec-h", "Choose variables")),
            shiny::p(class = "ec-help", "Start by confirming the observational structure, then choose the outcome and explanatory variables."),
            shiny::selectInput(
              "analysis_mode",
              .ec_labeled(
                "Data structure / econometric mode",
                "econcompare makes a conservative suggestion from the data, but the researcher confirms the mode. Time-series mode requires one explicit, unique temporal index and models relationships among variables over time; it is not a forecasting workflow.",
                "Use Cross-sectional for one observational slice; use Time-series econometrics when rows are ordered observations of the same system over time; use Panel for repeated individuals with explicit individual and period indexes."
              ),
              choices = c("Cross-sectional econometrics" = "cross_section", "Time-series econometrics" = "time_series", "Panel data econometrics" = "panel"),
              selected = initial_mode
            ),
            shiny::uiOutput("structure_hint"),
            shiny::div(class = "ec-tip-box",
              shiny::strong("Manual variable typing · "),
              "Automatic detection is advisory. If a variable is stored or detected incorrectly, you can override its type explicitly; econcompare validates the conversion before using it."
            ),
            shiny::selectInput("type_override_var", "Variable to re-type", choices = all_vars, selected = all_vars[1L]),
            shiny::selectInput("type_override_kind", "Manual type", choices = .ec_type_override_choices(), selected = "auto"),
            shiny::uiOutput("type_override_ordinal_ui"),
            shiny::div(style = "display:flex; gap:8px;",
              shiny::actionButton("apply_type_override", "Apply type", width = "50%"),
              shiny::actionButton("clear_type_overrides", "Clear overrides", width = "50%")
            ),
            shiny::uiOutput("type_override_status"),
            shiny::uiOutput("time_index_ui"),
            shiny::conditionalPanel(
              condition = "input.analysis_mode == 'panel'",
              shiny::selectInput("panel_id", "Individual identifier", choices = c("Choose..." = "", all_vars)),
              shiny::selectInput("panel_time", "Time / period identifier", choices = c("Choose..." = "", all_vars)),
              shiny::selectInput("panel_outcome", "Panel outcome", choices = c("Continuous" = "continuous", "Binary (numeric 0/1; 1 = event)" = "binary", "Count (non-negative integers)" = "count"), selected = "continuous"),
              shiny::uiOutput("panel_audit_ui"),
              shiny::p(class = "ec-help", "Static panel models with an explicitly chosen outcome family. Select both indexes explicitly; no automatic aggregation or imputation.")
            ),
            shiny::selectInput(
              "y",
              .ec_labeled(
                "Dependent variable",
                "This is the outcome you want to explain. Its type helps econcompare suggest a relevant analysis objective, but you remain free to choose the objective yourself.",
                "Example: choose mpg if you want to explain fuel efficiency."
              ),
              choices = all_vars,
              selected = initial_y
            ),
            shiny::conditionalPanel(
              condition = "input.analysis_mode == 'cross_section'",
              shiny::selectInput(
                "outcome_type",
                .ec_labeled(
                  "Analysis objective",
                  "econcompare suggests a group from the dependent variable, but the value selected here is your modelling choice. Models are then filtered to the same type of outcome so incompatible families are not mixed.",
                  "Continuous, binary, nominal categorical, or ordinal categorical. For ordinal outcomes, you explicitly specify the category order before estimation."
                ),
                choices = c(
                  "Continuous outcome" = "continuous",
                  "Binary outcome" = "binary",
                  "Nominal categorical outcome" = "nominal",
                  "Ordinal categorical outcome" = "ordinal"
                ),
                selected = initial_type
              ),
              shiny::uiOutput("outcome_hint"),
              shiny::uiOutput("binary_event_ui"),
              shiny::uiOutput("ordinal_order_ui")
            ),
            shiny::conditionalPanel(
              condition = "input.analysis_mode == 'time_series'",
              shiny::selectInput(
                "time_family",
                .ec_labeled(
                  "Temporal model family",
                  "Single-equation regressions keep one dependent variable. ECM adds an explicit long-run error-correction term. VAR/VECM are multivariate systems in which all selected series are endogenous.",
                  "Use VAR/VECM only when a system interpretation is substantively appropriate; econcompare does not choose lags or cointegration rank automatically."
                ),
                choices = c(
                  "Single-equation temporal regressions" = "single",
                  "Error-correction model (ECM)" = "ecm",
                  "Multivariate dynamic system (VAR / VECM)" = "system"
                ),
                selected = "single"
              ),
              shiny::div(class = "ec-tip-box", shiny::strong("Time-series objective · "),
                "Explain temporal relationships among numeric variables. For VAR/VECM, the variable selected above as the dependent variable is simply the first member of the system; it has no privileged structural status."
              )
            ),
            shiny::selectizeInput(
              "x",
              .ec_labeled(
                "Explanatory variables",
                "These are the covariates used on the right-hand side of the regression. Select the variables you want to compare across the different estimators.",
                "Tip: start with a small set of meaningful regressors, then expand gradually."
              ),
              choices = setdiff(all_vars, initial_y),
              multiple = TRUE,
              options = list(plugins = list("remove_button"))
            )
          ),
          shiny::div(class = "ec-section",
            shiny::div(class = "ec-step", shiny::div(class = "ec-step-n", "2"), shiny::div(class = "ec-h", "Pick comparison models")),
            shiny::p(class = "ec-help", "Choose one or more models for the selected econometric mode. No model is inserted automatically."),
            shiny::uiOutput("model_selector"),
            shiny::uiOutput("model_badges"),
            shiny::div(class = "ec-tip-box",
              shiny::strong("Econometric note · "),
              "Cross-sectional models are compared only within the same outcome objective. Time-series models compare explicit contemporaneous/lagged specifications; changing lag order changes the effective estimation sample."
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
              shiny::div(class = "ec-main-title", "Econometric workspace"),
              shiny::div(class = "ec-main-sub",
                "Start by understanding the data and its temporal structure, then compare a small set of coherent econometric specifications. Technical details remain available on demand."
              )
            ),
            shiny::uiOutput("run_meta")
          ),
          shiny::tabsetPanel(
            id = "workspace_tab",
            shiny::tabPanel("1 · Explore data",
              shiny::div(class = "ec-explore-intro",
                shiny::h3("Understand the data before modelling"),
                shiny::p("This explorer focuses on a few descriptive facts that matter before choosing a model. It does not decide which specification is correct."),
                shiny::div(class = "ec-explore-metrics",
                  shiny::div(class = "ec-mini-metric", shiny::tags$b(nrow(data)), shiny::tags$span("Rows")),
                  shiny::div(class = "ec-mini-metric", shiny::tags$b(ncol(data)), shiny::tags$span("Variables")),
                  shiny::div(class = "ec-mini-metric", shiny::tags$b(sum(stats::complete.cases(data))), shiny::tags$span("Complete rows")),
                  shiny::div(class = "ec-mini-metric", shiny::tags$b(sum(is.na(data))), shiny::tags$span("Missing cells"))
                )
              ),
              shiny::div(class = "ec-explore-card ec-explore-wide",
                shiny::h4("Quick variable overview"),
                shiny::p(class = "ec-note", "Scan the main descriptive facts for every variable. Binary and categorical variables also show category concentration so imbalanced outcomes are visible before modelling."),
                shiny::uiOutput("variable_summary_ui")
              ),
              shiny::div(class = "ec-explore-grid",
                shiny::div(class = "ec-explore-card",
                  shiny::h4("Variable profile"),
                  shiny::p(class = "ec-note", "Choose one variable to inspect its central tendency, spread, range and missingness."),
                  shiny::selectInput("explore_var", "Variable", choices = all_vars, selected = all_vars[1L]),
                  shiny::uiOutput("explore_profile"),
                  shiny::uiOutput("explore_category_balance")
                ),
                shiny::div(class = "ec-explore-card",
                  shiny::h4("Distribution"),
                  shiny::p(class = "ec-note", "Look at the shape of one variable before deciding how to model it."),
                  shiny::plotOutput("explore_distribution", height = "300px")
                )
              ),
              if (length(numeric_vars) >= 2L) shiny::div(class = "ec-explore-card ec-explore-wide",
                shiny::h4("Relationship between two numeric variables"),
                shiny::p(class = "ec-note", "A scatterplot and simple Pearson correlation help you see whether two variables move together. This is descriptive, not causal."),
                shiny::div(class = "ec-rel-controls",
                  shiny::selectInput("explore_x", "X variable", choices = numeric_vars, selected = numeric_vars[1L]),
                  shiny::selectInput("explore_y", "Y variable", choices = numeric_vars, selected = numeric_vars[min(2L, length(numeric_vars))])
                ),
                shiny::uiOutput("explore_correlation"),
                shiny::plotOutput("explore_scatter", height = "320px")
              ),
              shiny::conditionalPanel(
                condition = "input.analysis_mode == 'time_series'",
                shiny::div(class = "ec-explore-card ec-explore-wide",
                  shiny::h4("Temporal trajectories"),
                  shiny::p(class = "ec-note", "Standardized trajectories help compare co-movement when variables use different units. This is descriptive only; trends can create spurious correlation when series are non-stationary."),
                  shiny::plotOutput("time_trajectory_plot", height = "340px")
                )
              ),
              shiny::div(class = "ec-explore-card ec-explore-wide",
                shiny::h4("Basic data checks"),
                shiny::p(class = "ec-note", "These checks highlight issues worth reviewing before estimation. They are not pass/fail grades."),
                shiny::uiOutput("explore_notices")
              )
            ),
            shiny::tabPanel("2 · Compare models", shiny::uiOutput("results_ui"))
          )
        )
      )
    )
  )
  
}
