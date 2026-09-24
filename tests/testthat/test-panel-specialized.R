test_that("conditional logit matches the exact survival reference", {
  skip_if_not_installed("survival")
  d <- panel_special_fixture(); d$binary[d$id == "unit01"] <- 0
  fit <- eco_panel_run(d, binary ~ x + w + z, "id", "period", outcome_type = "binary")
  strata <- survival::strata
  coxph <- survival::coxph
  Surv <- survival::Surv
  ref <- survival::clogit(binary ~ x + w + strata(id), d[d$id != "unit01", ], method = "exact")
  z <- eco_compare(fit); z <- z[z$term_status == "estimated", ]
  expect_equal(z$estimate, unname(coef(ref)), tolerance = 1e-8)
  expect_equal(z$std.error, unname(sqrt(diag(vcov(ref)))), tolerance = 1e-8)
  expect_equal(z$p.value, 2 * pnorm(abs(z$statistic), lower.tail = FALSE))
  expect_true(all(is.infinite(z$inference_df)))
  expect_equal(unique(z$nobs), nrow(d) - 9)
  expect_equal(fit$sample_info$n_effective, nrow(d) - 9)
  expect_equal(fit$sample_exclusions$panel_clogit$row, 1:9)
  expect_true(all(is.na(z$r2)))
})

test_that("conditional logit refuses unsupported inference and invalid outcomes", {
  skip_if_not_installed("survival")
  d <- panel_special_fixture()
  expect_error(eco_panel_run(d, binary ~ x, "id", "period", outcome_type = "binary", inference = "cluster_id"), "classical")
  d$binary <- d$binary + 1
  expect_error(eco_panel_run(d, binary ~ x, "id", "period", outcome_type = "binary"), "0/1")
  d$binary <- 0
  expect_error(eco_panel_run(d, binary ~ x, "id", "period", outcome_type = "binary"), "informative")
})

test_that("Poisson matches fixest and reports dropped zero groups", {
  skip_if_not_installed("fixest")
  d <- panel_special_fixture(); d$count[d$id == "unit01"] <- 0
  fit <- eco_panel_run(d, count ~ x + w + z, "id", "period", outcome_type = "count")
  ref <- fixest::fepois(count ~ x + w | id, d[d$id != "unit01", ], fixef.rm = "none", glm.iter = 100, glm.tol = 1e-8, fixef.tol = 1e-8)
  ss <- fixest::ssc(K.adj = TRUE, K.fixef = "nonnested", G.adj = TRUE, G.df = "min", t.df = "min")
  z <- eco_compare(fit); z <- z[z$term_status == "estimated", ]
  expect_equal(z$estimate, unname(coef(ref)), tolerance = 1e-7)
  expect_equal(z$std.error, unname(sqrt(diag(vcov(ref, vcov = "iid", ssc = ss)))), tolerance = 1e-7)
  expect_equal(z$p.value, 2 * pnorm(abs(z$statistic), lower.tail = FALSE))
  expect_equal(fit$sample_exclusions$panel_poisson$row, 1:9)
  expect_true(all(is.na(z$r2)))
})

test_that("Poisson cluster inference uses the specified covariance and t G-1", {
  skip_if_not_installed("fixest")
  d <- panel_special_fixture()
  fit <- eco_panel_run(d, count ~ x + w, "id", "period", outcome_type = "count", inference = "cluster_id")
  ref <- fixest::fepois(count ~ x + w | id, d, fixef.rm = "perfect_fit", glm.iter = 100, glm.tol = 1e-8, fixef.tol = 1e-8)
  ss <- fixest::ssc(K.adj = TRUE, K.fixef = "nonnested", G.adj = TRUE, G.df = "min", t.df = "min")
  expect_equal(unname(fit$covariance$panel_poisson), unname(vcov(ref, vcov = ~id, ssc = ss)), tolerance = 1e-7, ignore_attr = TRUE)
  z <- eco_compare(fit); z <- z[z$term_status == "estimated", ]
  expect_equal(z$p.value, 2 * pt(abs(z$statistic), fit$meta$panel_poisson$groups - 1, lower.tail = FALSE))
  d$count[3] <- .5
  expect_error(eco_panel_run(d, count ~ x, "id", "period", outcome_type = "count"), "integer counts")
})

test_that("FE-IV matches direct within 2SLS and retains first stages", {
  skip_if_not_installed("plm")
  d <- panel_special_fixture()
  fit <- eco_panel_run(d, iv_y ~ endo + w + z, "id", "period", models = "panel_fe_iv", endogenous = "endo", instruments = c("iv1", "iv2"))
  ref <- plm::plm(iv_y ~ endo + w | w + iv1 + iv2, d, index = c("id", "period"), model = "within")
  z <- eco_compare(fit); z <- z[z$term_status == "estimated", ]
  expect_equal(z$estimate, unname(coef(ref)), tolerance = 1e-10)
  expect_equal(z$std.error, unname(sqrt(diag(vcov(ref)))), tolerance = 1e-10)
  expect_equal(z$p.value, 2 * pt(abs(z$statistic), df.residual(ref), lower.tail = FALSE))
  expect_named(fit$first_stages$panel_fe_iv, "endo")
  fs <- plm::plm(endo ~ w + iv1 + iv2, d, index = c("id", "period"), model = "within")
  expect_equal(coef(fit$first_stages$panel_fe_iv$endo), coef(fs), tolerance = 1e-10)
  b <- coef(fs)[c("iv1", "iv2")]; V <- vcov(fs)[names(b), names(b)]
  F <- as.numeric(crossprod(b, solve(V, b)))/2
  ds <- eco_panel_diagnostics(fit, tests = "iv_first_stage")
  expect_equal(ds$statistic, F)
  expect_equal(ds$p.value, pf(F, 2, df.residual(fs), lower.tail = FALSE))
})

test_that("FE-IV protects specification, common sample and instrument rank", {
  skip_if_not_installed("plm")
  d <- panel_special_fixture()
  expect_error(eco_panel_run(d, iv_y ~ endo + w, "id", "period", models = "panel_fe_iv"), "endogenous")
  expect_error(eco_panel_run(d, iv_y ~ endo + w, "id", "period", models = "panel_fe_iv", endogenous = "endo", instruments = "endo"), "excluded instruments")
  expect_error(eco_panel_run(d, iv_y ~ endo + w, "id", "period", models = "panel_fe_iv", endogenous = "endo", instruments = "z"), "invariant/collinear")
  d$iv1[4] <- NA
  f <- eco_panel_run(d, iv_y ~ endo + w, "id", "period", models = c("panel_fe_individual", "panel_fe_iv"), endogenous = "endo", instruments = c("iv1", "iv2"), na_action = "omit", inference = "cluster_id")
  expect_equal(f$excluded_rows$row, 4)
  expect_identical(f$sample_rows$panel_fe_iv, f$sample_rows$panel_fe_individual)
  V <- plm::vcovHC(f$models$panel_fe_iv, method = "arellano", type = "HC1", cluster = "group")
  expect_equal(f$covariance$panel_fe_iv, V, ignore_attr = TRUE)
})

test_that("engine families cannot be mixed without a matching outcome contract", {
  d <- panel_special_fixture()
  expect_error(eco_panel_run(d, binary ~ x, "id", "period", models = "panel_clogit"), "outcome_type")
  expect_error(eco_panel_run(d, binary ~ x, "id", "period", outcome_type = "binary", models = c("panel_clogit", "panel_pooling")), "outcome_type")
})

test_that("CD after time effects retains audit values but suspends inference", {
  skip_if_not_installed("plm")
  f <- eco_panel_run(panel_fixture(), y ~ x + w, "id", "period", models = c("panel_fe_individual", "panel_fe_time", "panel_fe_twoways"))
  ds <- eco_panel_diagnostics(f, tests = "dependence")
  for (nm in c("panel_fe_time", "panel_fe_twoways")) {
    ref <- plm::pcdtest(f$models[[nm]], test = "cd"); z <- ds[ds$model == nm, ]
    expect_equal(z$statistic, unname(ref$statistic))
    expect_equal(z$raw_p.value, ref$p.value)
    expect_true(is.na(z$p.value))
    expect_identical(z$status, "computed_uninterpreted")
    expect_identical(z$reason_code, "calibration_unverified")
    expect_false(grepl("Reject H0", z$interpretation, fixed = TRUE))
  }
  expect_identical(ds$status[ds$model == "panel_fe_individual"], "computed")
})

test_that("unavailable diagnostic reasons distinguish scope and missing models", {
  skip_if_not_installed("plm")
  f <- eco_panel_run(panel_fixture(), y ~ x + w, "id", "period", models = c("panel_fd", "panel_between"))
  ds <- eco_panel_diagnostics(f, tests = c("serial", "dependence", "hausman", "mundlak", "iv_first_stage"))
  expect_true(all(ds$reason_code[ds$model == "panel_fd"] == "not_implemented"))
  expect_true(all(ds$reason_code[ds$model == "panel_between"] == "not_applicable"))
  expect_true(all(ds$reason_code[ds$test %in% c("hausman", "mundlak", "iv_first_stage")] == "missing_models"))
})

test_that("Mundlak labels and inference are explicit and source rows are not conflated", {
  skip_if_not_installed("plm")
  f <- eco_panel_run(panel_fixture(), y ~ x + w, "id", "period", models = c("panel_fd", "panel_between", "panel_mundlak"), inference = "cluster_id")
  z <- eco_compare(f)
  expect_equal(z$term_label[z$term == ".ec_mundlak_mean_1"], "Individual mean of x")
  expect_match(eco_panel_diagnostics(f, tests = "mundlak")$inference, "cluster_id")
  expect_match(eco_panel_diagnostics(f, tests = "mundlak")$parameters, "df2 31")
  a <- eco_sample_audit(f)
  expect_match(a$sample_warning[a$model == "panel_between"], "Same source rows")
  skip_if_not_installed("shiny")
  html <- as.character(.ec_panel_results_ui(f))
  expect_match(html, "Individual mean of x", fixed = TRUE)
  ds <- eco_panel_diagnostics(f, tests = "hausman")
  expect_match(as.character(.ec_panel_table_ui(ds)), "Required models absent", fixed = TRUE)
})

test_that("new Shiny families run and instrument changes invalidate diagnostics", {
  skip_if_not_installed("shiny"); skip_if_not_installed("survival"); skip_if_not_installed("fixest"); skip_if_not_installed("plm")
  d <- panel_special_fixture(); av <- names(d); nv <- av[vapply(d, is.numeric, logical(1))]
  server <- .ec_app_server(d, av, nv, character(), eco_models(), "binary", "binary", eco_data_structure(d), "cross_section", "")
  shiny::testServer(server, {
    session$setInputs(analysis_mode = "panel", panel_id = "id", panel_time = "period", panel_outcome = "binary", y = "binary", x = c("x", "w"), models = "panel_clogit", panel_inference = "classical", panel_na = "fail", run = 1)
    expect_null(state$error); expect_named(state$fit$models, "panel_clogit"); expect_no_error(output$results_ui)
    session$setInputs(panel_outcome = "count", y = "count", models = "panel_poisson", panel_inference = "cluster_id", run = 2)
    expect_null(state$error); expect_named(state$fit$models, "panel_poisson"); expect_no_error(output$results_ui)
    session$setInputs(panel_outcome = "continuous", y = "iv_y", x = c("endo", "w"), models = "panel_fe_iv", panel_endogenous = "endo", panel_instruments = c("iv1", "iv2"), run = 3)
    expect_null(state$error); expect_named(state$fit$models, "panel_fe_iv")
    session$setInputs(panel_tests = "iv_first_stage", panel_serial_order = 1, panel_run_diagnostics = 1)
    expect_identical(state$panel_diag$status, "computed")
    session$setInputs(panel_instruments = "iv1")
    expect_true(results_stale())
  })
})

test_that("nonlinear row maps survive sorting and explicit missingness", {
  skip_if_not_installed("survival"); skip_if_not_installed("fixest")
  d <- panel_special_fixture(); set.seed(18); d <- d[sample(nrow(d)), ]; d$x[13] <- NA
  saved <- d
  for (kind in c("binary", "count")) {
    f <- eco_panel_run(d, reformulate(c("x", "w"), kind), "id", "period", outcome_type = kind, na_action = "omit")
    expect_equal(f$excluded_rows$row, 13)
    expect_false("13" %in% f$sample_rows[[1]])
    map <- f$transformations[[1]]
    expect_equal(as.character(d$id[map$row]), as.character(map$id))
    expect_equal(as.numeric(d$period[map$row]), as.numeric(map$period))
  }
  expect_identical(d, saved)
})

test_that("new engines retain public terms with non-syntactic names", {
  skip_if_not_installed("survival"); skip_if_not_installed("fixest"); skip_if_not_installed("plm")
  d <- panel_special_fixture(); d[["odd x"]] <- d$x
  for (kind in c("binary", "count")) {
    f <- eco_panel_run(d, reformulate(c("`odd x`", "w"), kind), "id", "period", outcome_type = kind)
    z <- eco_compare(f)
    expect_true("`odd x`" %in% z$term)
    expect_true(all(is.finite(z$estimate[z$term_status == "estimated"])))
  }
})

test_that("FE-IV can retain two explicit endogenous first stages", {
  skip_if_not_installed("plm")
  d <- panel_special_fixture(); set.seed(78)
  d$endo2 <- d$iv2 + rnorm(nrow(d)); d$iv_y <- d$iv_y + .4 * d$endo2
  f <- eco_panel_run(d, iv_y ~ endo + endo2 + w, "id", "period", models = "panel_fe_iv",
    endogenous = c("endo", "endo2"), instruments = c("iv1", "iv2"), inference = "cluster_id")
  expect_named(f$first_stages$panel_fe_iv, c("endo", "endo2"))
  expect_equal(nrow(eco_panel_diagnostics(f, tests = "iv_first_stage")), 2)
  expect_error(eco_panel_run(d, iv_y ~ endo + endo2 + w, "id", "period", models = "panel_fe_iv",
    endogenous = c("endo", "endo2"), instruments = "iv1"), "Underidentified")
})


test_that("conditional logit works with survival namespace loaded but not attached", {
  skip_if_not_installed("survival")
  skip_if("package:survival" %in% search(), "Run in a fresh R session to exercise namespace-only loading")
  before <- search()
  d <- panel_special_fixture()
  f <- eco_panel_run(d, binary ~ x + w, "id", "period", outcome_type = "binary")
  expect_s3_class(f$models$panel_clogit, "clogit")
  expect_true(all(is.finite(coef(f$models$panel_clogit))))
  expect_identical(search(), before)
})
