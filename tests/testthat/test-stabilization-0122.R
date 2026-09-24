fixture_0122 <- function() {
  set.seed(122)
  data.frame(year = 1901:2020, y = 20 + cumsum(rnorm(120)), x = rnorm(120))
}

test_that("temporal response transformations and intercept changes cannot be silently lost", {
  d <- fixture_0122()
  for (f in list(log(y) ~ x, I(y * 2) ~ x)) {
    expect_error(eco_time_run(d, f, time = "year", models = "time_static"), "Precompute")
    expect_error(eco_ecm_run(d, f, time = "year"), "Precompute")
  }
  for (f in list(y ~ x - 1, y ~ x + 0)) {
    expect_error(eco_time_run(d, f, time = "year", models = "time_static"), "intercept")
    expect_error(eco_ecm_run(d, f, time = "year"), "intercept")
  }
  d$log_y <- log(d$y)
  fit <- eco_time_run(d, log_y ~ x, time = "year", models = "time_static")
  expect_equal(unname(coef(fit$models[[1]])), unname(coef(lm(log_y ~ x, d))))
  fit <- eco_ecm_run(d, y ~ x, time = "year", long_run_intercept = FALSE)
  expect_false("(Intercept)" %in% names(coef(fit$long_run_model)))
})

test_that("generated lags cannot overwrite input columns or other generated terms", {
  d <- fixture_0122(); d$y_L1 <- rnorm(nrow(d)); d$x_L1 <- rnorm(nrow(d))
  original <- d
  expect_error(.ec_build_time_model_data(d, "year", y ~ y_L1, p = 1, engine = "dynamic_regression"), "collision")
  expect_error(.ec_build_time_model_data(d, "year", y ~ x + x_L1, q = 1, engine = "distributed_lag"), "collision")
  expect_error(eco_ecm_run(d, y ~ x + x_L1, "year", q = 1), "collision")
  expect_identical(d, original)
  expect_no_error(.ec_build_time_model_data(d, "year", y ~ x, p = 1, q = 1, engine = "ardl"))
})

test_that("aliased OLS and WLS terms remain visible alongside identifiable coefficients", {
  d <- fixture_0122(); d$x2 <- 2*d$x
  fit <- eco_run(d, y ~ x + x2, models = c("ols", "wls"), model_args = list(wls = list(weights = rep(1, nrow(d)))))
  z <- eco_compare(fit)
  for (nm in names(fit$models)) {
    ref <- coef(fit$models[[nm]]); zz <- z[z$model == nm, ]
    expect_setequal(zz$term, names(ref))
    expect_true(all(is.na(zz$estimate[zz$term %in% names(ref)[is.na(ref)]])))
    expect_true(all(zz$term_status[is.na(zz$estimate)] == "not estimable: aliased coefficient"))
  }
  expect_true(nrow(fit$warnings) >= 2)
})

test_that("mixed extraction engines share term status without changing valid estimates", {
  skip_if_not_installed("MASS")
  fit <- eco_run(mtcars, mpg ~ wt + hp, models = c("ols", "robust_m"))
  z <- eco_compare(fit)
  expect_setequal(z$model, names(fit$models))
  expect_true(all(z$term_status == "estimated"))
  expect_equal(z$estimate[z$model == "ols"], unname(coef(lm(mpg ~ wt + hp, mtcars))))
})

test_that("UI formula creation quotes literal column names", {
  d <- fixture_0122(); names(d)[2:3] <- c("GDP index", "education spending")
  f <- .ec_formula("GDP index", "education spending")
  expect_equal(all.vars(f), c("GDP index", "education spending"))
  fit <- eco_time_run(d, f, "year", models = "time_static")
  expect_equal(unname(coef(fit$models[[1]])), unname(coef(lm(f, d))))
})

test_that("small nonzero numbers and p-values are not rendered as exact zero", {
  expect_match(.ec_fmt(1e-12), "e-12", fixed = TRUE)
  expect_identical(.ec_fmt_p(0), "<0.0001")
  expect_identical(.ec_fmt(NA_real_), "NA")
  expect_identical(.ec_version(), "0.12.2")
})

test_that("panel audit distinguishes schedules from key validity and provides original dates", {
  d <- data.frame(id = rep(c("a", "b"), each = 4), date = rep(as.Date(c("2020-01-01", "2020-04-01", "2020-07-01", "2020-10-01")), 2))
  a <- eco_panel_audit(d, "id", "date")
  expect_true(a$summary$index_valid)
  expect_true(all(a$by_individual$first_time == "2020-01-01"))
  a <- eco_panel_audit(rbind(d, d[1, ]), "id", "date")
  expect_false(a$summary$index_valid)
  expect_true(a$summary$balanced)
})

test_that("fixed-effect absorption is informational rather than an estimation warning", {
  skip_if_not_installed("plm")
  d <- panel_fixture()
  fit <- eco_panel_run(d, y ~ x + z, "id", "period", models = "panel_fe_individual")
  expect_true(nrow(fit$information) > 0)
  expect_false(any(grepl("absorbed", fit$warnings$message)))
  expect_true(any(eco_compare(fit)$term_status == "absorbed by fixed effects"))
})

test_that("Shiny preserves a run snapshot and blocks diagnostics after changed specification", {
  skip_if_not_installed("shiny"); skip_if_not_installed("plm")
  d <- panel_fixture(); av <- names(d); nv <- av[vapply(d, is.numeric, logical(1))]
  server <- .ec_app_server(d, av, nv, character(), eco_models(), "y", "continuous", eco_data_structure(d), "cross_section", "")
  shiny::testServer(server, {
    session$setInputs(analysis_mode="panel", panel_id="id", panel_time="period", y="y", x="x",
      models="panel_fe_individual", panel_inference="cluster_id", panel_na="fail")
    session$setInputs(run=1)
    expect_null(state$error); expect_false(results_stale())
    saved <- state$run_data
    session$setInputs(x="w")
    expect_true(results_stale())
    expect_match(as.character(output$run_status), "Settings changed")
    session$setInputs(panel_tests="serial", panel_serial_order=1, panel_run_diagnostics=1)
    expect_null(state$panel_diag)
    expect_identical(state$run_data, saved)
    session$setInputs(run=2)
    expect_false(results_stale())
    expect_equal(all.vars(state$fit$formula), c("y", "w"))
    session$setInputs(panel_run_diagnostics=2)
    expect_true(is.data.frame(state$panel_diag))
    session$setInputs(panel_serial_order=2)
    expect_null(state$panel_diag)
    expect_false(results_stale())
  })
})

test_that("provenance identifies source version and actual fitted formulas", {
  fit <- eco_run(mtcars, mpg ~ wt, models = "ols")
  expect_identical(fit$provenance$econcompare_version, "0.12.2")
  expect_equal(fit$provenance$fitted_formulas$ols, formula(fit$models$ols))
  expect_equal(fit$provenance$sample_info, fit$sample_info)
  expect_true("stats" %in% names(fit$provenance$engine_versions))
})
