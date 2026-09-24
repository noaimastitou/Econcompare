test_that("panel routing runs only the chosen model and explicit comparisons", {
  skip_if_not_installed("plm")
  f <- eco_panel_run(panel_fixture(), y ~ x + w, "id", "period",
    models = c("panel_pooling", "panel_fe_individual", "panel_re", "panel_mundlak"))
  d <- .ec_panel_diagnostics_for_model(f, "panel_fe_individual", c("serial", "dependence", "effects_f"))
  expect_true(all(d$model == "panel_fe_individual"))
  expect_setequal(d$test, c("panel_bg", "pesaran_cd", "effects_f"))
  ref <- eco_panel_diagnostics(f, tests = "effects_f")
  expect_equal(d$statistic[d$test == "effects_f"], ref$statistic)
  h <- .ec_panel_diagnostics_for_model(f, "panel_re", "hausman")
  expect_equal(h, eco_panel_diagnostics(f, tests = "hausman"))
  expect_error(.ec_panel_diagnostics_for_model(f, "panel_pooling", "mundlak"), "selected model")
  calls <- list()
  local_mocked_bindings(eco_panel_diagnostics = function(x, tests, ...) {
    calls[[length(calls) + 1L]] <<- list(models = names(x$models), test = tests)
    data.frame(model = names(x$models)[1], test = tests)
  })
  .ec_panel_diagnostics_for_model(f, "panel_fe_individual", c("serial", "effects_f", "hausman"))
  expect_equal(calls[[1]]$models, "panel_fe_individual")
  expect_setequal(calls[[2]]$models, c("panel_fe_individual", "panel_pooling"))
  expect_setequal(calls[[3]]$models, c("panel_fe_individual", "panel_re"))
})

test_that("switching panel model clears previous diagnostic results", {
  skip_if_not_installed("shiny"); skip_if_not_installed("plm")
  d <- panel_fixture(); av <- names(d); nv <- av[vapply(d, is.numeric, logical(1))]
  server <- .ec_app_server(d, av, nv, character(), eco_models(), "y", "continuous", eco_data_structure(d), "cross_section", "")
  shiny::testServer(server, {
    session$setInputs(analysis_mode = "panel", panel_id = "id", panel_time = "period", y = "y", x = "x",
      models = c("panel_pooling", "panel_fe_individual"), panel_inference = "classical", panel_na = "fail", run = 1)
    session$setInputs(panel_diag_model = "panel_fe_individual", panel_tests = "serial", panel_serial_order = 1)
    session$setInputs(panel_run_diagnostics = 1)
    expect_true(all(state$panel_diag$model == "panel_fe_individual"))
    session$setInputs(panel_diag_model = "panel_pooling")
    expect_null(state$panel_diag)
    expect_match(output$panel_diag_controls$html, "effects_lm", fixed = TRUE)
    expect_false(grepl('value="effects_f"', output$panel_diag_controls$html, fixed = TRUE))
  })
})

test_that("stationarity runs use the selected series without rebuilding controls", {
  skip_if_not_installed("shiny")
  set.seed(144); d <- data.frame(year = 2000:2049, y = rnorm(50), x = rnorm(50), z = rnorm(50))
  calls <- list()
  local_mocked_bindings(eco_stationarity_tests = function(data, variables, ...) {
    calls[[length(calls) + 1L]] <<- variables
    data.frame(variable = variables, test = "test stub", p.value = .5)
  })
  av <- names(d)
  server <- .ec_app_server(d, av, av, character(), eco_models(), "y", "continuous", eco_data_structure(d), "time_series", "year")
  shiny::testServer(server, {
    session$setInputs(analysis_mode = "time_series", time_var = "year", y = "y", x = c("x", "z"), models = "time_static", run = 1)
    expect_null(state$error)
    before <- output$time_diag_ui
    session$setInputs(time_stationarity_vars = "x", time_adf_k = 1, time_adf_deterministic = "drift", time_kpss_null = "Level")
    session$setInputs(run_stationarity = 1)
    expect_equal(calls[[1]], "x")
    expect_identical(output$time_diag_ui, before)
    expect_match(output$time_diag_results$html, "test stub", fixed = TRUE)
    session$setInputs(time_stationarity_vars = c("x", "z"))
    expect_null(state$stationarity)
    session$setInputs(run_stationarity = 2)
    expect_equal(calls[[2]], c("x", "z"))
    expect_equal(state$stationarity$variable, c("x", "z"))
    expect_identical(output$time_diag_ui, before)
    session$setInputs(time_stationarity_vars = character())
    session$setInputs(run_stationarity = 3)
    expect_length(calls, 2L)
    expect_match(state$time_diag_error, "Choose at least one")
  })
})

test_that("system stationarity results do not reset multiple-series controls", {
  skip_if_not_installed("shiny"); skip_if_not_installed("vars")
  set.seed(145); d <- data.frame(year = 2000:2059, y = rnorm(60), x = rnorm(60), z = rnorm(60))
  chosen <- NULL
  local_mocked_bindings(eco_stationarity_tests = function(data, variables, ...) {
    chosen <<- variables
    data.frame(variable = variables, test = "system stub", p.value = .5)
  })
  av <- names(d)
  server <- .ec_app_server(d, av, av, character(), eco_models(), "y", "continuous", eco_data_structure(d), "time_series", "year")
  shiny::testServer(server, {
    session$setInputs(analysis_mode = "time_series", time_family = "system", time_var = "year", y = "y", x = c("x", "z"), models = "var", system_p = 1, run = 1)
    expect_null(state$error)
    before <- output$system_diag_ui
    session$setInputs(system_stationarity_vars = c("x", "z"), system_adf_k = 1, system_adf_deterministic = "drift", system_kpss_null = "Level")
    session$setInputs(run_system_stationarity = 1)
    expect_equal(chosen, c("x", "z"))
    expect_identical(output$system_diag_ui, before)
    expect_match(output$system_diag_results$html, "system stub", fixed = TRUE)
  })
})

test_that("panel Interpretation content is removed without relocating it", {
  skip_if_not_installed("shiny"); skip_if_not_installed("plm")
  f <- eco_panel_run(panel_fixture(), y ~ x + w, "id", "period", models = "panel_mundlak")
  html <- as.character(.ec_panel_results_ui(f))
  expect_false(grepl('Interpretation|What each model estimates|Mundlak components|Mundlak mean-term names|Multiplicative associations|IV first stages', html))
  expect_match(html, "panel_diag_model", fixed = TRUE)
  expect_match(html, "panel_diag_controls", fixed = TRUE)
  expect_true(length(f$components) > 0L)
})
