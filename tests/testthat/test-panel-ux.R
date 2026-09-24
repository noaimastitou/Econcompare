test_that("nonlinear presentation explains limits without unrelated empty sections", {
  skip_if_not_installed("survival"); skip_if_not_installed("shiny")
  d <- panel_special_fixture()
  fit <- eco_panel_run(d, binary ~ x, "id", "period", outcome_type = "binary")
  html <- as.character(.ec_panel_results_ui(fit))
  expect_false(grepl("Multiplicative associations|What each model estimates", html))
  expect_match(html, "panel_diag_model")
  expect_false(grepl("Mundlak components|IV first stages|panel_run_diagnostics", html))
  cap <- .ec_panel_capabilities(fit)
  expect_false(any(cap$selectable))
  ds <- eco_panel_diagnostics(fit, tests = c("serial", "dependence", "mundlak"))
  expect_true(all(ds$inference == "No inference performed"))
  expect_true(all(is.na(ds$inference_df)))
  expect_false(any(grepl("BG on the transformed", ds$note)))
  tab <- .ec_panel_visible_metrics(eco_compare(fit))
  expect_false(any(c("r2", "adj_r2", "aic") %in% names(tab)))
  expect_true(all(c("r2", "adj_r2", "aic") %in% names(eco_compare(fit))))
})

test_that("CD audit values are separated from interpretable diagnostic results", {
  skip_if_not_installed("plm"); skip_if_not_installed("shiny")
  fit <- eco_panel_run(panel_fixture(), y ~ x, "id", "period", models = "panel_fe_time")
  ds <- eco_panel_diagnostics(fit, tests = "dependence")
  expect_equal(ds$status, "computed_uninterpreted")
  expect_true(is.na(ds$p.value)); expect_true(is.finite(ds$raw_p.value))
  html <- as.character(.ec_panel_diagnostics_ui(ds))
  expect_match(html, "Raw engine values")
  expect_match(html, "Hypotheses and inference details")
  cap <- .ec_panel_capabilities(fit)
  expect_true(cap$audit_only[cap$test == "dependence"])
  expect_false(cap$selectable[cap$test == "effects_f"])
})

test_that("complete separation is rejected by exact conditional logit", {
  skip_if_not_installed("survival")
  d <- panel_fixture(); d$binary <- as.integer(d$x > 0)
  expect_error(eco_panel_run(d, binary ~ x, "id", "period", outcome_type = "binary"),
    "converg|infinite|iteration|separation")
})

test_that("Poisson optimizer nonconvergence is not returned as a successful fit", {
  skip_if_not_installed("fixest")
  local_mocked_bindings(.ec_panel_poisson_engine = function(formula, data) {
    suppressWarnings(fixest::fepois(formula, data = data, fixef.rm = "none",
      glm.iter = 1, glm.tol = 1e-12, notes = FALSE, warn = TRUE, data.save = TRUE))
  })
  d <- panel_special_fixture()
  expect_error(eco_panel_run(d, count ~ x + w, "id", "period", outcome_type = "count"),
    "did not converge")
})

test_that("panel options retain valid choices on control rebuilds", {
  skip_if_not_installed("shiny")
  d <- panel_special_fixture(); av <- names(d); nv <- av[vapply(d, is.numeric, logical(1))]
  server <- .ec_app_server(d, av, nv, character(), eco_models(), "iv_y", "continuous", eco_data_structure(d), "cross_section", "")
  shiny::testServer(server, {
    session$setInputs(analysis_mode = "panel", panel_id = "id", panel_time = "period",
      panel_outcome = "continuous", y = "iv_y", x = c("endo", "w"), models = "panel_fe_iv",
      panel_inference = "cluster_id", panel_endogenous = "endo", panel_instruments = "iv1", panel_na = "omit")
    html <- output$model_options$html
    expect_match(html, 'value="cluster_id" selected')
    expect_match(html, 'value="omit" selected')
    expect_match(html, 'value="endo" selected')
    expect_match(html, 'value="iv1" selected')
    session$setInputs(x = c("endo", "w", "x"))
    expect_match(output$model_options$html, 'value="iv1" selected')
    session$setInputs(models = "panel_clogit")
    expect_false(grepl('value="cluster_id"', output$model_options$html))
  })
})
