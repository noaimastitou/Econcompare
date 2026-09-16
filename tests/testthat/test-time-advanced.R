test_that("ECM keeps long-run and short-run relationships distinct", {
  set.seed(110)
  n <- 80
  x <- cumsum(rnorm(n))
  y <- 1.5 * x + arima.sim(model = list(ar = .4), n = n)
  d <- data.frame(year = 1940 + seq_len(n), y = as.numeric(y), x = as.numeric(x))
  fit <- eco_ecm_run(d, y ~ x, time = "year", p = 1, q = 1)
  expect_s3_class(fit, "econcompare")
  expect_identical(fit$temporal_family, "ecm")
  expect_true("ECT_L1" %in% names(stats::coef(fit$models$ecm)))
  lr <- eco_ecm_long_run(fit)
  expect_true(any(lr$term == "x"))
  cmp <- eco_compare(fit)
  expect_true(any(cmp$term == "ECT_L1"))
  expect_match(fit$time_metadata$cointegration_note, "does not infer cointegration")
})

test_that("ECM supports variable-specific short-run difference lags", {
  set.seed(111)
  n <- 100
  x1 <- cumsum(rnorm(n)); x2 <- cumsum(rnorm(n))
  y <- x1 - .5*x2 + rnorm(n)
  d <- data.frame(year=1901:2000, y=y, x1=x1, x2=x2)
  fit <- eco_ecm_run(d, y ~ x1 + x2, time="year", p=0, q=0, q_by_var=c(x1=2, x2=1))
  co <- names(stats::coef(fit$models$ecm))
  expect_true(all(c("D_x1", "D_x1_L1", "D_x1_L2", "D_x2", "D_x2_L1") %in% co))
  expect_identical(unname(fit$q_by_var[c("x1","x2")]), c(2L,1L))
})

test_that("ECM refuses calendar gaps and internal missing values", {
  d <- data.frame(year=c(2000:2005, 2007:2020), y=rnorm(20), x=rnorm(20))
  expect_error(eco_ecm_run(d, y ~ x, time="year"), "complete regular time grid")
  d2 <- data.frame(year=2000:2020, y=rnorm(21), x=rnorm(21)); d2$x[10] <- NA_real_
  expect_error(eco_ecm_run(d2, y ~ x, time="year"), "complete numeric sample")
})

test_that("VAR is represented as a multivariate system with equation-level coefficients", {
  skip_if_not_installed("vars")
  set.seed(112)
  n <- 90
  x <- matrix(rnorm(n*2), ncol=2)
  for (t in 2:n) x[t,] <- c(.5*x[t-1,1] + .1*x[t-1,2], -.2*x[t-1,1] + .4*x[t-1,2]) + x[t,]
  d <- data.frame(year=1931:2020, a=x[,1], b=x[,2])
  fit <- eco_system_run(d, c("a","b"), time="year", model="var", p=2)
  expect_s3_class(fit, "econcompare_system")
  expect_identical(fit$analysis_type, "time_series_system")
  expect_identical(fit$temporal_family, "var")
  expect_length(fit$models, 2L)
  cmp <- eco_compare(fit)
  expect_true(nrow(cmp) > 0L)
  expect_true(all(cmp$engine == "var"))
  expect_true(all(cmp$outcome_type == "system"))
})

test_that("VAR lag selection reports criteria without choosing for the researcher", {
  skip_if_not_installed("vars")
  set.seed(113)
  d <- data.frame(year=1951:2020, a=rnorm(70), b=rnorm(70))
  z <- eco_var_lag_selection(d, c("a","b"), time="year", lag_max=3)
  expect_true(all(c("criteria","selection","note") %in% names(z)))
  expect_true(all(z$criteria$p %in% 1:3))
  expect_match(z$note, "does not select")
})

test_that("Johansen diagnostics report critical values without automatic rank", {
  skip_if_not_installed("urca")
  set.seed(114)
  n <- 100
  x <- cumsum(rnorm(n)); y <- x + rnorm(n, sd=.4)
  d <- data.frame(year=1921:2020, x=x, y=y)
  z <- eco_johansen_test(d, c("x","y"), time="year", K=2, type="trace", ecdet="const")
  expect_true(all(c("null_rank_hypothesis","statistic","critical_5pct","note") %in% names(z)))
  expect_true(all(grepl("does not choose", z$note)))
})

test_that("VECM requires explicit valid rank and retains Johansen evidence", {
  skip_if_not_installed("vars")
  skip_if_not_installed("urca")
  set.seed(115)
  n <- 120
  x <- cumsum(rnorm(n)); y <- 1.2*x + rnorm(n, sd=.5)
  d <- data.frame(year=1901:2020, x=x, y=y)
  expect_error(eco_system_run(d, c("x","y"), time="year", model="vecm", p=2), "explicit integer `rank`")
  fit <- eco_system_run(d, c("x","y"), time="year", model="vecm", p=2, rank=1, johansen_type="trace", ecdet="const")
  expect_s3_class(fit, "econcompare_system")
  expect_identical(fit$temporal_family, "vecm")
  expect_equal(fit$rank, 1L)
  expect_true(is.data.frame(eco_vecm_rank_test(fit)))
  beta <- eco_vecm_cointegration(fit)
  expect_true(is.matrix(beta) || is.data.frame(beta))
  expect_match(attr(beta, "note"), "normalization-dependent")
})

test_that("advanced temporal controls are mounted in Shiny UI", {
  skip_if_not_installed("shiny")
  ui <- .ec_app_ui(mtcars, names(mtcars), names(mtcars), "mpg", "continuous",
                   eco_data_structure(mtcars), "cross_section", "")
  html <- as.character(ui)
  expect_match(html, "time_family", fixed=TRUE)
  expect_match(html, "Multivariate dynamic system", fixed=TRUE)
})

test_that("system model catalogue separates VAR and VECM availability", {
  z <- eco_system_models()
  expect_equal(z$engine, c("var", "vecm"))
  expect_true(all(c("estimator", "package", "description", "available") %in% names(z)))
  expect_type(z$available, "logical")
})

test_that("system Portmanteau horizon must exceed fitted VAR lag order", {
  x <- list(analysis_type = "time_series_system", temporal_family = "var", p = 2L)
  class(x) <- c("econcompare_system", "econcompare")
  expect_error(eco_system_diagnostics(x, serial_lags = 2L), "greater than the fitted VAR lag order")
})
