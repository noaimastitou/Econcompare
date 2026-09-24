test_that("selected analysis objective is sufficient to run the app", {
  skip_if_not_installed("shiny")
  all_vars <- names(mtcars)
  numeric_vars <- all_vars[vapply(mtcars, is.numeric, logical(1))]
  registry <- eco_models()
  server <- .ec_app_server(mtcars, all_vars, numeric_vars, character(), registry, "mpg", "continuous", eco_data_structure(mtcars), "cross_section", "")
  shiny::testServer(server, {
    session$setInputs(y = "mpg", outcome_type = "continuous", x = c("wt", "hp"), models = "ols", run = 1)
    expect_null(state$error)
    expect_s3_class(state$fit, "econcompare")
    expect_true(nrow(state$comparison) > 0L)
    expect_error(output$run_meta, NA)
    expect_error(output$results_ui, NA)
  })
})

test_that("outcome-specific controls are mounted in the app UI", {
  skip_if_not_installed("shiny")
  ui <- .ec_app_ui(
    mtcars,
    names(mtcars),
    names(mtcars)[vapply(mtcars, is.numeric, logical(1))],
    "mpg",
    "continuous",
    eco_data_structure(mtcars),
    "cross_section",
    ""
  )
  html <- as.character(ui)
  expect_match(html, "ordinal_order_ui", fixed = TRUE)
  expect_match(html, "binary_event_ui", fixed = TRUE)
})

test_that("ordinal Shiny control accepts an explicit ordered vector", {
  x <- c("Medium", "Low", "Very high", "High", "Medium")
  z <- .ec_user_ordered(x, c("Low", "Medium", "High", "Very high"))
  expect_true(is.ordered(z))
  expect_identical(levels(z), c("Low", "Medium", "High", "Very high"))
})

test_that("responsive app CSS does not force table wrappers beyond their columns", {
  skip_if_not_installed("shiny")
  css <- .ec_app_css()
  expect_match(css, "grid-template-columns:repeat(2,minmax(0,1fr))", fixed = TRUE)
  expect_match(css, ".ec-table-wrap{overflow-x:auto", fixed = TRUE)
  expect_match(css, ".ec-table-compact table{min-width:0", fixed = TRUE)
  expect_false(grepl(".ec-table,.ec-table table", css, fixed = TRUE))
})

test_that("Heckman selection controls cannot reuse the indicator as a regressor", {
  choices <- .ec_selection_regressor_choices(c("wage", "employed", "age", "distance"), "wage", "employed")
  expect_setequal(choices, c("age", "distance"))
  expect_error(.ec_validate_heckman_selection("wage", "employed", c("employed", "age")), "cannot also be used")
  expect_error(.ec_validate_heckman_selection("wage", "employed", c("wage", "age")), "outcome variable")
  expect_error(.ec_validate_heckman_selection("wage", "employed", c("age", "distance"), outcome_x = c("employed", "education")), "outcome-equation regressor")
  expect_silent(.ec_validate_heckman_selection("wage", "employed", c("age", "distance"), outcome_x = c("education", "experience")))
  expect_error(.ec_validate_heckman_formulas(employed ~ employed + age, wage ~ education), "own selection regressor")
  expect_silent(.ec_validate_heckman_formulas(employed ~ age + distance, wage ~ education + age))
})

test_that("IV UI formula helper enforces the minimum order condition", {
  expect_error(
    .ec_iv_formula("y", c("x1", "x2", "x3"), c("x1", "x2"), "z1"),
    "minimum order condition"
  )
  expect_error(
    .ec_iv_formula("y", c("x1", "x2"), "x1", "x2"),
    "excluded instruments must not also be structural regressors"
  )
  f <- .ec_iv_formula("y", c("x1", "x2"), "x1", "z1")
  expect_s3_class(f, "formula")
})

test_that("ordinal app workflow uses the researcher-selected category order", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("MASS")
  d <- mtcars
  d$gear_label <- factor(as.character(d$gear))
  all_vars <- names(d)
  numeric_vars <- all_vars[vapply(d, is.numeric, logical(1))]
  binary_candidates <- all_vars[vapply(d, .ec_is_binary_indicator, logical(1))]
  server <- .ec_app_server(d, all_vars, numeric_vars, binary_candidates, eco_models(), "mpg", "continuous", eco_data_structure(d), "cross_section", "")
  shiny::testServer(server, {
    session$setInputs(
      y = "gear_label", outcome_type = "ordinal", ordinal_order = c("3", "4", "5"),
      x = "wt", models = "ordered_logit", run = 1
    )
    expect_null(state$error)
    expect_s3_class(state$fit$models$ordered_logit, "polr")
    mf <- stats::model.frame(state$fit$models$ordered_logit)
    expect_identical(levels(stats::model.response(mf)), c("3", "4", "5"))
  })
})

test_that("time-series Shiny workflow uses the explicit temporal index", {
  skip_if_not_installed("shiny")
  set.seed(2)
  d <- data.frame(year=2000:2025, x=rnorm(26), y=rnorm(26))
  all_vars <- names(d)
  numeric_vars <- all_vars[vapply(d, is.numeric, logical(1))]
  server <- .ec_app_server(d, all_vars, numeric_vars, character(), eco_models(), "y", "continuous",
                           eco_data_structure(d), "time_series", "year")
  shiny::testServer(server, {
    session$setInputs(analysis_mode="time_series", time_var="year", y="y", x="x",
                      models=c("time_static", "ardl"), time_p=1, time_q=1, run=1)
    expect_null(state$error)
    expect_s3_class(state$fit, "econcompare")
    expect_identical(state$fit$analysis_type, "time_series")
    expect_identical(state$fit$time_variable, "year")
  })
})

test_that("manual type override controls are present in the app UI", {
  skip_if_not_installed("shiny")
  d <- data.frame(year=2000:2005, y=rnorm(6), x=rnorm(6))
  si <- eco_data_structure(d)
  ui <- .ec_app_ui(d,names(d),c("year","y","x"),"y","continuous",si,"time_series","year")
  txt <- as.character(ui)
  expect_match(txt,"type_override_var",fixed=TRUE)
  expect_match(txt,"type_override_kind",fixed=TRUE)
  expect_match(txt,"apply_type_override",fixed=TRUE)
  expect_match(txt,"type_override_ordinal_ui",fixed=TRUE)
})


test_that("time-series diagnostic UI exposes individual-series and deterministic ADF controls", {
  skip_if_not_installed("shiny")
  set.seed(12)
  d <- data.frame(year=2000:2025, x=rnorm(26), y=rnorm(26))
  all_vars <- names(d)
  numeric_vars <- all_vars[vapply(d, is.numeric, logical(1))]
  server <- .ec_app_server(d, all_vars, numeric_vars, character(), eco_models(), "y", "continuous",
                           eco_data_structure(d), "time_series", "year")
  shiny::testServer(server, {
    session$setInputs(analysis_mode="time_series", time_var="year", y="y", x="x",
                      models="time_static", run=1)
    expect_null(state$error)
    txt <- as.character(output$time_diag_ui)
    expect_match(txt, "time_stationarity_vars", fixed=TRUE)
    expect_match(txt, "time_adf_deterministic", fixed=TRUE)
  })
})

test_that("manual ordinal override in Shiny requires and preserves explicit category order", {
  skip_if_not_installed("shiny")
  d <- data.frame(id=1:6, rating=c("High","Low","Medium","High","Medium","Low"), y=rnorm(6))
  all_vars <- names(d)
  numeric_vars <- all_vars[vapply(d, is.numeric, logical(1))]
  server <- .ec_app_server(d, all_vars, numeric_vars, character(), eco_models(), "y", "continuous",
                           eco_data_structure(d), "cross_section", "")
  shiny::testServer(server, {
    session$setInputs(type_override_var="rating", type_override_kind="ordinal",
                      type_override_ordinal_levels=c("Low","Medium","High"), apply_type_override=1)
    expect_null(type_state$error)
    expect_identical(type_state$ordinal_levels$rating, c("Low","Medium","High"))
    expect_true(is.ordered(active_data()$rating))
    expect_identical(levels(active_data()$rating), c("Low","Medium","High"))
  })
})
