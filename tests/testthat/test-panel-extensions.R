# Independent references: explicit lm transformations and plm estimators.
test_that("FD matches explicit differences and plm without an added drift", {
  skip_if_not_installed("plm")
  d <- panel_fixture()
  fit <- eco_panel_run(d, y ~ x + w + z, "id", "period", models = "panel_fd")
  delta <- do.call(rbind, lapply(split(d, d$id), function(g) {
    data.frame(y = diff(g$y), x = diff(g$x), w = diff(g$w))
  }))
  ref <- lm(y ~ x + w - 1, delta)
  tab <- eco_compare(fit); est <- tab[tab$term_status == "estimated", ]
  expect_equal(est$estimate, unname(coef(ref)), tolerance = 1e-10)
  expect_equal(est$std.error, unname(sqrt(diag(vcov(ref)))), tolerance = 1e-10)
  expect_equal(est$p.value, unname(coef(summary(ref))[, 4]), tolerance = 1e-10)
  pref <- plm::plm(y ~ x + w - 1, d, index = c("id", "period"), model = "fd")
  expect_equal(est$estimate, unname(coef(pref)), tolerance = 1e-10)
  expect_true(all(is.na(tab$estimate[tab$term %in% c("(Intercept)", "z")])))
  expect_equal(unique(tab$r2), summary(ref)$r.squared)
  expect_equal(fit$sample_info$n_effective, 32 * 8)
  expect_equal(fit$sample_info$n_used, nrow(d))
})

test_that("FD records exact pairs and never differences across omissions or gaps", {
  skip_if_not_installed("plm")
  d <- panel_fixture(); d$x[5] <- NA
  set.seed(51); d <- d[sample(nrow(d)), ]
  f <- eco_panel_run(d, y ~ x + w, "id", "period", models = "panel_fd", na_action = "omit")
  pairs <- f$transformations$panel_fd
  expect_equal(nrow(pairs), 32 * 8 - 2)
  expect_equal(d$id[pairs$start_row], d$id[pairs$end_row])
  expect_true(all(d$period[pairs$end_row] - d$period[pairs$start_row] == 1))
  expect_false(any(is.na(d$x[c(pairs$start_row, pairs$end_row)])))
  g <- panel_fixture(); g <- g[g$period %% 2 == 1, ]
  expect_error(eco_panel_run(g, y ~ x, "id", "period", models = "panel_fd"), "consecutive")
  g <- panel_fixture(); g$date <- sprintf("2020-%02d", g$period)
  g$x[g$period == 5] <- NA
  f <- eco_panel_run(g, y ~ x, "id", "date", models = "panel_fd", na_action = "omit")
  expect_true(all(f$transformations$panel_fd$end_period - f$transformations$panel_fd$start_period == 1))
  expect_equal(nobs(f$models$panel_fd), 32 * 6)
})

test_that("FD excludes isolated source observations explicitly", {
  skip_if_not_installed("plm")
  d <- panel_fixture(); d <- d[!(d$id == "unit01" & d$period %in% 2:9), ]
  f <- eco_panel_run(d, y ~ x, "id", "period", models = "panel_fd")
  expect_equal(f$sample_exclusions$panel_fd$row, 1)
  expect_equal(f$meta$panel_fd$groups, 31)
  expect_match(f$sample_exclusions$panel_fd$reason, "consecutive")
})

test_that("between gives equal weight to each mean on an unbalanced sample", {
  skip_if_not_installed("plm")
  d <- panel_fixture()[-c(2:9, 14, 23, 31), ]
  f <- eco_panel_run(d, y ~ x + w, "id", "period", models = "panel_between")
  means <- aggregate(cbind(y, x, w) ~ id, d, mean)
  ref <- lm(y ~ x + w, means)
  z <- eco_compare(f)
  expect_equal(z$estimate, unname(coef(ref)), tolerance = 1e-10)
  expect_equal(z$std.error, unname(sqrt(diag(vcov(ref)))), tolerance = 1e-10)
  expect_equal(z$p.value, unname(coef(summary(ref))[, 4]), tolerance = 1e-10)
  pref <- plm::plm(y ~ x + w, d, index = c("id", "period"), model = "between")
  expect_equal(z$estimate, unname(coef(pref)), tolerance = 1e-10)
  expect_equal(f$sample_info$n_used, nrow(d))
  expect_equal(f$sample_info$n_effective, 32)
  expect_equal(nrow(f$transformations$panel_between), nrow(d))
  expect_equal(f$transformations$panel_between$periods_in_mean[1], 1)
})

test_that("transformed robust covariance matches explicit sandwich references", {
  skip_if_not_installed("plm"); skip_if_not_installed("sandwich")
  d <- panel_fixture()
  f <- eco_panel_run(d, y ~ x + w, "id", "period", models = c("panel_fd", "panel_between"), inference = "cluster_id")
  delta <- do.call(rbind, lapply(split(d, d$id), function(g) data.frame(id = g$id[-1], y = diff(g$y), x = diff(g$x), w = diff(g$w))))
  ref <- lm(y ~ x + w - 1, delta)
  V <- sandwich::vcovCL(ref, cluster = delta$id, type = "HC1", cadjust = TRUE)
  expect_equal(unname(f$covariance$panel_fd), unname(V), tolerance = 1e-10, ignore_attr = TRUE)
  means <- aggregate(cbind(y, x, w) ~ id, d, mean)
  V <- sandwich::vcovHC(lm(y ~ x + w, means), type = "HC1")
  expect_equal(unname(f$covariance$panel_between), unname(V), tolerance = 1e-10, ignore_attr = TRUE)
  z <- eco_compare(f); z <- z[z$term_status == "estimated", ]
  expect_true(all(z$inference_df == 31))
  expect_equal(z$p.value, 2 * pt(abs(z$statistic), 31, lower.tail = FALSE))
})

test_that("Mundlak reproduces explicitly augmented random effects", {
  skip_if_not_installed("plm")
  d <- panel_fixture(); d$x[5] <- NA
  f <- eco_panel_run(d, y ~ x + w + z, "id", "period", models = "panel_mundlak", na_action = "omit", inference = "cluster_id")
  r <- d[complete.cases(d[c("y", "x", "w", "z")]), ]
  r$mean_x <- ave(r$x, r$id, FUN = mean); r$mean_w <- ave(r$w, r$id, FUN = mean)
  ref <- plm::plm(y ~ x + w + z + mean_x + mean_w, r, index = c("id", "period"), model = "random", random.method = "swar")
  expect_equal(unname(coef(f$models$panel_mundlak)), unname(coef(ref)), tolerance = 1e-10)
  V <- plm::vcovHC(ref, method = "arellano", type = "HC1", cluster = "group")
  expect_equal(unname(f$covariance$panel_mundlak), unname(V), tolerance = 1e-10, ignore_attr = TRUE)
  expect_setequal(f$mean_terms$panel_mundlak$variable, c("x", "w"))
  expect_equal(f$transformations$panel_mundlak$periods_in_mean[1], 8)
  expect_false("5" %in% f$sample_rows$panel_mundlak)
})

test_that("Mundlak components and joint Wald use the complete selected covariance", {
  skip_if_not_installed("plm")
  f <- eco_panel_run(panel_fixture(), y ~ x + w, "id", "period", models = "panel_mundlak", inference = "cluster_id")
  m <- f$models$panel_mundlak; b <- coef(m); V <- f$covariance$panel_mundlak
  mapping <- f$mean_terms$panel_mundlak
  C <- f$components$panel_mundlak
  mu <- mapping$term[mapping$variable == "x"]
  z <- C[C$variable == "x" & C$component == "between-type slope (sum)", ]
  expect_equal(z$estimate, unname(b["x"] + b[mu]))
  expect_equal(z$std.error, unname(sqrt(V["x", "x"] + V[mu, mu] + 2 * V["x", mu])))
  u <- mapping$term; W <- as.numeric(crossprod(b[u], solve(V[u, u], b[u]))) / length(u)
  ds <- eco_panel_diagnostics(f, tests = "mundlak")
  expect_identical(ds$status, "computed")
  expect_equal(ds$statistic, W)
  expect_equal(ds$p.value, pf(W, length(u), 31, lower.tail = FALSE))
  f$covariance$panel_mundlak[u, u] <- 0
  expect_identical(eco_panel_diagnostics(f, tests = "mundlak")$status, "unavailable")
})

test_that("sample comparisons distinguish source rows from observational units", {
  skip_if_not_installed("plm")
  f <- eco_panel_run(panel_fixture(), y ~ x + w, "id", "period", models = c("panel_pooling", "panel_fd", "panel_between", "panel_mundlak"))
  expect_false(f$sample_comparable)
  a <- eco_sample_audit(f)
  expect_true(all(a$matches_reference_rows))
  expect_match(a$sample_match[a$model == "panel_fd"], "different observation unit")
  expect_match(a$sample_match[a$model == "panel_between"], "different observation unit")
  expect_match(a$sample_match[a$model == "panel_mundlak"], "same rows")
  expect_length(unique(eco_compare(f)$observation_unit), 3)
  ds <- eco_panel_diagnostics(f, tests = c("serial", "dependence"))
  expect_true(all(ds$status[ds$model %in% c("panel_fd", "panel_between")] == "unavailable"))
})

test_that("extensions reject nonidentified specifications and unsupported factors", {
  skip_if_not_installed("plm")
  d <- panel_fixture(); d$copy <- 2 * d$x; d$f <- factor(d$period %% 2)
  for (nm in c("panel_fd", "panel_between", "panel_mundlak")) {
    expect_error(eco_panel_run(d, y ~ x + copy, "id", "period", models = nm), "[Uu]nidentified")
    expect_error(eco_panel_run(d, y ~ x + f, "id", "period", models = nm), "numeric/logical")
  }
  expect_error(eco_panel_run(d, y ~ z, "id", "period", models = "panel_fd"), "No varying")
  expect_error(eco_panel_run(d, y ~ z, "id", "period", models = "panel_mundlak"), "within-individual")
  d$date <- as.Date("2020-01-01") + c(0, 2, 5, 9, 14, 20, 27, 35, 44)[d$period]
  expect_error(eco_panel_run(d, y ~ x, "id", "date", models = "panel_fd"), "verified calendar")
})

test_that("new estimators support non-syntactic variable names without mutating input", {
  skip_if_not_installed("plm")
  d <- panel_fixture(); d[["odd x"]] <- d$x; saved <- d
  engines <- c("panel_fd", "panel_between", "panel_mundlak")
  a <- eco_panel_run(d, y ~ x + w, "id", "period", models = engines)
  b <- eco_panel_run(d, y ~ `odd x` + w, "id", "period", models = engines)
  for (nm in engines) expect_equal(unname(coef(a$models[[nm]])), unname(coef(b$models[[nm]])), tolerance = 1e-10)
  expect_identical(d, saved)
})

test_that("extension failures can be collected without hiding successful models", {
  skip_if_not_installed("plm")
  d <- panel_fixture(); d$f <- factor(d$period %% 2)
  f <- eco_panel_run(d, y ~ x + f, "id", "period", models = c("panel_pooling", "panel_fd"), error_policy = "collect")
  expect_named(f$models, "panel_pooling")
  expect_identical(f$failures$model, "panel_fd")
})

test_that("panel UI presents transformed units, mean names and diagnostic scope", {
  skip_if_not_installed("plm"); skip_if_not_installed("shiny")
  f <- eco_panel_run(panel_fixture(), y ~ x + w, "id", "period", models = c("panel_fd", "panel_between", "panel_mundlak"))
  html <- as.character(.ec_panel_results_ui(f))
  expect_false(grepl("Mundlak components", html, fixed = TRUE))
  expect_match(html, "n effective", fixed = TRUE)
  expect_match(html, "source membership", fixed = TRUE)
  expect_match(html, "mundlak", fixed = TRUE)
  expect_match(html, "consecutive within-individual change", fixed = TRUE)
  expect_equal(nrow(eco_panel_models()), 11)
})


test_that("omission does not re-infer a coarser calendar for diagnostics", {
  skip_if_not_installed("plm")
  d <- panel_fixture(); d$date <- as.Date(sprintf("2020-%02d-01", d$period))
  d$x[d$period %% 2 == 0] <- NA
  f <- eco_panel_run(d, y ~ x + w, "id", "date", models = "panel_pooling", na_action = "omit")
  expect_equal(f$estimation_audit$summary$frequency, "monthly")
  expect_true(all(f$estimation_audit$by_individual$internal_missing_periods == 4))
  expect_identical(eco_panel_diagnostics(f, tests = "serial")$status, "unavailable")
  expect_error(eco_panel_run(d, y ~ x + w, "id", "date", models = "panel_fd", na_action = "omit"), "consecutive")
})

test_that("Shiny runs the three new engines and the Mundlak diagnostic", {
  skip_if_not_installed("plm"); skip_if_not_installed("shiny"); skip_if_not_installed("sandwich")
  d <- panel_fixture(); av <- names(d); nv <- av[vapply(d, is.numeric, logical(1))]
  server <- .ec_app_server(d, av, nv, character(), eco_models(), "y", "continuous", eco_data_structure(d), "cross_section", "")
  shiny::testServer(server, {
    session$setInputs(analysis_mode = "panel", panel_id = "id", panel_time = "period", y = "y", x = c("x", "w"),
      models = c("panel_fd", "panel_between", "panel_mundlak"), panel_inference = "cluster_id", panel_na = "fail", run = 1)
    expect_null(state$error)
    expect_setequal(names(state$fit$models), c("panel_fd", "panel_between", "panel_mundlak"))
    expect_no_error(output$results_ui)
    session$setInputs(panel_diag_model = "panel_mundlak", panel_tests = "mundlak", panel_serial_order = 1, panel_run_diagnostics = 1)
    expect_identical(state$panel_diag$status, "computed")
  })
})
