test_that("panel audit distinguishes balance, gaps, singletons and invalid keys", {
  d <- panel_fixture(); saved <- d
  a <- eco_panel_audit(d, "id", "period")
  expect_true(a$summary$balanced)
  expect_equal(a$summary$individuals, 32)
  expect_equal(a$summary$periods, 9)
  expect_equal(sum(a$by_individual$internal_missing_periods), 0)
  gap <- eco_panel_audit(d[-5, ], "id", "period")
  expect_false(gap$summary$balanced)
  expect_equal(sum(gap$by_individual$internal_missing_periods), 1)
  d2 <- d[-(2:9), ]
  expect_equal(eco_panel_audit(d2, "id", "period")$summary$singletons, 1)
  expect_equal(eco_panel_audit(rbind(d, d[1, ]), "id", "period")$summary$duplicate_rows, 2)
  d2 <- d; d2$id[1] <- NA; d2$period[2] <- NA
  expect_equal(nrow(eco_panel_audit(d2, "id", "period")$issues), 2)
  expect_identical(d, saved)
  expect_error(eco_panel_audit(d, "id", "id"), "distinct")
  d2 <- d; d2$period <- d2$period + .5
  expect_error(eco_panel_audit(d2, "id", "period"), "integer")
})

test_that("calendar labels work with two periods and do not compress gaps", {
  d <- panel_fixture(); d <- d[d$period <= 2, ]
  d$date <- as.Date(c("2020-01-31", "2020-02-29"))[d$period]
  expect_equal(eco_panel_audit(d, "id", "date")$summary$frequency, "monthly")
  d <- panel_fixture(); d$date <- sprintf("2020-%02d", d$period)
  a <- eco_panel_audit(d[-5, ], "id", "date")
  expect_equal(sum(a$by_individual$internal_missing_periods), 1)
  d$date <- as.Date("2020-01-01") + c(0, 2, 5, 9, 14, 20, 27, 35, 44)[d$period]
  expect_false(eco_panel_audit(d, "id", "date")$summary$calendar_verified)
  expect_identical(eco_data_structure(panel_fixture())$structure, "panel_candidate")
})

test_that("all five estimators reproduce plm coefficients and classical inference", {
  skip_if_not_installed("plm")
  d <- panel_fixture(); reg <- eco_panel_models()[1:5, ]
  fit <- eco_panel_run(d, y ~ x + w, "id", "period", models = reg$engine)
  tab <- eco_compare(fit)
  for (i in seq_len(nrow(reg))) {
    nm <- reg$engine[i]
    ref <- plm::plm(y ~ x + w, d, index = c("id", "period"), model = reg$model[i], effect = reg$effect[i], random.method = "swar")
    z <- tab[tab$model == nm & tab$term_status == "estimated", ]
    z <- z[match(names(coef(ref)), z$term), ]
    expect_equal(z$estimate, unname(coef(ref)), tolerance = 1e-10)
    expect_equal(z$std.error, unname(sqrt(diag(vcov(ref)))), tolerance = 1e-10)
    expect_equal(z$p.value, 2 * pt(abs(z$estimate/z$std.error), df.residual(ref), lower.tail = FALSE), tolerance = 1e-10)
    expect_equal(unique(z$nobs), nrow(d))
    expect_s3_class(fit$models[[nm]], "plm")
  }
  expect_true(fit$sample_comparable)
  expect_true(all(is.na(tab$aic)))
  expect_equal(nrow(eco_sample_audit(fit)), 5)
})

test_that("individual cluster covariance and t G-1 are explicit", {
  skip_if_not_installed("plm")
  fit <- eco_panel_run(panel_fixture(), y ~ x + w, "id", "period", inference = "cluster_id")
  tab <- eco_compare(fit)
  for (nm in names(fit$models)) {
    ref <- plm::vcovHC(fit$models[[nm]], method = "arellano", type = "HC1", cluster = "group")
    expect_equal(fit$covariance[[nm]], ref, ignore_attr = TRUE)
    z <- tab[tab$model == nm & tab$term_status == "estimated", ]
    expect_equal(z$p.value, 2 * pt(abs(z$statistic), 31, lower.tail = FALSE))
    expect_true(all(z$inference_df == 31))
  }
  di <- eco_panel_diagnostics(fit, tests = c("effects_f", "effects_lm"))
  expect_true(all(di$status == "unavailable"))
})

test_that("absorbed terms remain visible and aliases are rejected", {
  skip_if_not_installed("plm")
  d <- panel_fixture()
  fit <- eco_panel_run(d, y ~ x + z, "id", "period", models = "panel_fe_individual")
  tab <- eco_compare(fit)
  expect_true(is.na(tab$estimate[tab$term == "z"]))
  expect_identical(tab$term_status[tab$term == "z"], "absorbed by fixed effects")
  d$copy <- 2 * d$x
  expect_error(eco_panel_run(d, y ~ x + copy, "id", "period"), "Unidentified")
  fit <- eco_panel_run(d, y ~ x + z, "id", "period", models = c("panel_pooling", "panel_fe_individual"))
  expect_true(is.finite(eco_compare(fit)$estimate[eco_compare(fit)$model == "panel_pooling" & eco_compare(fit)$term == "z"]))
  d$y <- 1
  expect_error(eco_panel_run(d, y ~ x, "id", "period"), "variation|perfect fit|variances")
})

test_that("row identities survive sorting, omission, tibble conversion and singletons", {
  skip_if_not_installed("plm")
  d <- panel_fixture(); set.seed(24); d <- d[sample(nrow(d)), ]
  rownames(d) <- paste0("original_", seq_len(nrow(d)))
  d$x[17] <- NA
  expect_error(eco_panel_run(d, y ~ x + w, "id", "period"), "na_action")
  fit <- eco_panel_run(d, y ~ x + w, "id", "period", na_action = "omit")
  expect_equal(fit$excluded_rows$row, 17)
  expect_setequal(fit$sample_rows$panel_pooling, as.character(setdiff(seq_len(nrow(d)), 17)))
  expect_true(fit$sample_comparable)
  d <- panel_fixture()[-(2:9), ]
  fit <- eco_panel_run(d, y ~ x + w, "id", "period")
  expect_false(fit$sample_comparable)
  expect_equal(fit$sample_exclusions$panel_fe_individual$row, 1)
  expect_equal(eco_panel_diagnostics(fit, tests = "effects_f")$status, "unavailable")
  skip_if_not_installed("tibble")
  expect_warning(eco_panel_run(tibble::as_tibble(panel_fixture()), y ~ x, "id", "period"), NA)
})

test_that("diagnostics reproduce direct tests and protect irregular calendars", {
  skip_if_not_installed("plm")
  d <- panel_fixture()
  fit <- eco_panel_run(d, y ~ x + w, "id", "period", models = c("panel_pooling", "panel_fe_individual", "panel_re"))
  ds <- eco_panel_diagnostics(fit, tests = c("effects_f", "effects_lm", "serial", "dependence", "hausman"))
  refs <- list(
    effects_f = plm::pFtest(fit$models$panel_fe_individual, fit$models$panel_pooling),
    effects_lm = plm::plmtest(fit$models$panel_pooling, effect = "individual", type = "bp"),
    hausman = plm::phtest(fit$models$panel_fe_individual, fit$models$panel_re))
  for (nm in names(refs)) {
    z <- ds[ds$test == nm, ]
    expect_equal(z$statistic, unname(refs[[nm]]$statistic))
    expect_equal(z$p.value, unname(refs[[nm]]$p.value))
  }
  for (nm in names(fit$models)) {
    for (test in c("panel_bg", "pesaran_cd")) {
      ref <- if (test == "panel_bg") plm::pbgtest(fit$models[[nm]], order = 1, type = "Chisq") else plm::pcdtest(fit$models[[nm]], test = "cd")
      z <- ds[ds$model == nm & ds$test == test, ]
      expect_equal(z$statistic, unname(ref$statistic))
      expect_equal(z$p.value, unname(ref$p.value))
    }
  }
  gap <- eco_panel_run(d[-5, ], y ~ x, "id", "period")
  expect_true(all(eco_panel_diagnostics(gap, tests = "serial")$status == "unavailable"))
  expect_error(eco_panel_diagnostics(fit, serial_order = 0), "positive")
  expect_error(eco_panel_diagnostics(fit, tests = "unknown"), "Unknown")
  expect_error(eco_diagnostics(fit), "eco_panel_diagnostics")
})

test_that("robust auxiliary Hausman respects covariance and slope restrictions", {
  skip_if_not_installed("plm")
  d <- panel_fixture(); engines <- c("panel_fe_individual", "panel_re")
  fit <- eco_panel_run(d, y ~ x + w, "id", "period", models = engines, inference = "cluster_id")
  ref <- plm::phtest(y ~ x + w, data = d, index = c("id", "period"), method = "aux", model = c("within", "random"), effect = "individual",
    vcov = function(m) plm::vcovHC(m, method = "arellano", type = "HC1", cluster = "group"))
  z <- eco_panel_diagnostics(fit, tests = "hausman")
  expect_equal(z$p.value, unname(ref$p.value), tolerance = 1e-10)
  fit <- eco_panel_run(d, y ~ x + z, "id", "period", models = engines, inference = "cluster_id")
  expect_identical(eco_panel_diagnostics(fit, tests = "hausman")$status, "unavailable")
})

test_that("invalid specifications fail with actionable messages", {
  skip_if_not_installed("plm")
  d <- panel_fixture()
  expect_error(eco_panel_run(rbind(d, d[1, ]), y ~ x, "id", "period"), "duplicate")
  expect_error(eco_panel_run(d, y ~ id, "id", "period"), "indexes")
  expect_error(eco_panel_run(d, y ~ log(x), "id", "period"), "named columns")
  expect_error(eco_panel_run(d, y ~ x - 1, "id", "period"), "intercept")
  expect_error(eco_panel_run(d, y ~ x, "id", "period", models = "ols"), "supported")
  expect_error(eco_panel_run(d[d$id == "unit01", ], y ~ x, "id", "period"), "two individuals")
  d$x[1] <- Inf
  expect_error(eco_panel_run(d, y ~ x, "id", "period"), "non-finite")
})

test_that("Shiny panel workflow estimates, renders, diagnoses and clears failed runs", {
  skip_if_not_installed("plm"); skip_if_not_installed("shiny")
  d <- panel_fixture(); av <- names(d); nv <- av[vapply(d, is.numeric, logical(1))]
  server <- .ec_app_server(d, av, nv, character(), eco_models(), "y", "continuous", eco_data_structure(d), "cross_section", "")
  shiny::testServer(server, {
    session$setInputs(analysis_mode = "panel", panel_id = "id", panel_time = "period", y = "y", x = c("x", "w"),
      models = c("panel_pooling", "panel_fe_individual"), panel_inference = "cluster_id", panel_na = "fail", run = 1)
    expect_null(state$error)
    expect_s3_class(state$fit, "econcompare_panel")
    expect_true(nrow(state$comparison) > 0)
    expect_no_error(output$results_ui)
    expect_no_error(output$run_meta)
    session$setInputs(panel_tests = "serial", panel_serial_order = 1, panel_run_diagnostics = 1)
    expect_true(all(state$panel_diag$status == "computed"))
    session$setInputs(panel_id = "period", run = 2)
    expect_match(state$error, "distinct")
    expect_null(state$fit)
    expect_null(state$comparison)
  })
})

test_that("panel HTML retains absorption and diagnostic explanations", {
  skip_if_not_installed("plm")
  fit <- eco_panel_run(panel_fixture(), y ~ x + z, "id", "period", inference = "cluster_id")
  path <- tempfile(fileext = ".html")
  on.exit(unlink(path), add = TRUE)
  eco_view(fit, file = path, open = FALSE, diagnostics = "effects_f")
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(html, "absorbed by fixed effects", fixed = TRUE)
  expect_match(html, "unavailable", fixed = TRUE)
  expect_match(html, "Term status and inference", fixed = TRUE)
})
