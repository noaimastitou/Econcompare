read_panel_export <- function(fit, diagnostics = "all") {
  path <- tempfile(fileext = ".html")
  on.exit(unlink(path), add = TRUE)
  eco_view(fit, file = path, open = FALSE, diagnostics = diagnostics)
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

test_that("all panel diagnostics include compatible model comparisons", {
  skip_if_not_installed("plm")
  fit <- eco_panel_run(panel_fixture(), y ~ x + w, "id", "period",
    models = c("panel_fe_individual", "panel_re", "panel_mundlak"))
  html <- read_panel_export(fit)
  expect_match(html, "All added individual-mean coefficients equal zero", fixed = TRUE)
  expect_match(html, "FE/RE coefficient differences", fixed = TRUE)
  iv <- eco_panel_run(panel_special_fixture(), iv_y ~ endo + w, "id", "period",
    models = "panel_fe_iv", endogenous = "endo", instruments = c("iv1", "iv2"))
  expect_match(read_panel_export(iv), "Excluded instruments have zero first-stage coefficients", fixed = TRUE)
})

test_that("HTML retains estimation issues and escapes their messages", {
  skip_if_not_installed("plm")
  fit <- eco_panel_run(panel_fixture(), y ~ x, "id", "period", models = "panel_pooling")
  fit$warnings <- data.frame(model = "panel_pooling", engine = "panel_pooling", message = "audit <warning>")
  fit$failures <- data.frame(model = "panel_re", engine = "panel_re", message = "audit failed estimator")
  html <- read_panel_export(fit, FALSE)
  expect_match(html, "Warnings / failures", fixed = TRUE)
  expect_match(html, "audit &lt;warning&gt;", fixed = TRUE)
  expect_match(html, "audit failed estimator", fixed = TRUE)
  expect_match(html, "<td>estimation</td>", fixed = TRUE)
  expect_false(grepl("audit <warning>", html, fixed = TRUE))
})

test_that("unavailable diagnostics never invite an impossible run", {
  skip_if_not_installed("survival"); skip_if_not_installed("shiny")
  fit <- eco_panel_run(panel_special_fixture(), binary ~ x, "id", "period", outcome_type = "binary")
  expect_match(.ec_panel_diagnostic_prompt(fit), "No supported diagnostic", fixed = TRUE)
  html <- read_panel_export(fit)
  expect_match(html, "Diagnostic limitations", fixed = TRUE)
  expect_match(html, "Error dependence has not been assessed", fixed = TRUE)
  expect_false(grepl("Pass <code>diagnostics", html, fixed = TRUE))
  d <- panel_special_fixture(); av <- names(d); nv <- av[vapply(d, is.numeric, logical(1))]
  server <- .ec_app_server(d, av, nv, character(), eco_models(), "binary", "binary", eco_data_structure(d), "cross_section", "")
  shiny::testServer(server, {
    state$fit <- fit
    expect_match(output$panel_diag_table$html, "No supported diagnostic", fixed = TRUE)
    expect_false(grepl("Select tests and run", output$panel_diag_table$html, fixed = TRUE))
  })
})

test_that("HTML diagnostic zero p-values use a bound and creation time is recorded", {
  skip_if_not_installed("plm")
  fit <- eco_panel_run(panel_fixture(), y ~ x, "id", "period", models = "panel_pooling")
  expect_s3_class(fit$created, "POSIXct")
  expect_length(fit$created, 1L)
  local_mocked_bindings(eco_panel_diagnostics = function(...) data.frame(
    model = "panel_pooling", test = "audit_test", status = "computed", statistic = 100,
    p.value = 0, raw_p.value = 0, note = "test formatting"))
  html <- read_panel_export(fit, "serial")
  expect_match(html, "&lt;0.0001", fixed = TRUE)
  expect_match(html, .ec_escape_html(format(fit$created)), fixed = TRUE)
})
