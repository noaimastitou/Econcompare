test_that("continuous OLS extraction remains numerically identical to stats::lm", {
  x <- eco_run(mtcars, mpg ~ wt + hp, models = "ols")
  ref <- stats::lm(mpg ~ wt + hp, data = mtcars)
  z <- eco_compare(x)
  expect_equal(z$estimate, unname(stats::coef(ref)), tolerance = 1e-10)
  expect_equal(z$std.error, unname(summary(ref)$coefficients[, 2]), tolerance = 1e-10)
  expect_equal(z$p.value, unname(summary(ref)$coefficients[, 4]), tolerance = 1e-10)
})

test_that("length-zero fit statistics remain safe", {
  expect_true(is.na(.ec_scalar_numeric(numeric(0), missing = "na", stat = "test")))
  expect_error(.ec_scalar_numeric(numeric(0), missing = "error", stat = "test"), "length-0")
})

test_that("OLS diagnostic subset stays available", {
  x <- eco_run(mtcars, mpg ~ wt + hp, models = "ols")
  z <- eco_diagnostics(x, tests = c("breusch_pagan", "reset", "vif"))
  expect_setequal(z$id, c("breusch_pagan", "reset", "vif"))
})

test_that("Koenker-Breusch-Pagan benchmark matches lmtest when available", {
  skip_if_not_installed("lmtest")
  x <- eco_run(mtcars, mpg ~ wt + hp, models = "ols")
  ours <- eco_diagnostics(x, tests = "breusch_pagan")
  ref <- lmtest::bptest(stats::lm(mpg ~ wt + hp, data = mtcars), studentize = TRUE)
  expect_equal(ours$statistic[1], unname(as.numeric(ref$statistic)), tolerance = 1e-8)
  expect_equal(ours$p.value[1], ref$p.value, tolerance = 1e-8)
})

test_that("Ramsey RESET benchmark matches lmtest when available", {
  skip_if_not_installed("lmtest")
  x <- eco_run(mtcars, mpg ~ wt + hp, models = "ols")
  ours <- eco_diagnostics(x, tests = "reset")
  ref <- lmtest::resettest(stats::lm(mpg ~ wt + hp, data = mtcars), power = 2:3, type = "fitted")
  expect_equal(ours$statistic[1], unname(as.numeric(ref$statistic)), tolerance = 1e-8)
  expect_equal(ours$p.value[1], ref$p.value, tolerance = 1e-8)
})

test_that("VIF/GVIF handles factor terms", {
  d <- transform(mtcars, gear_f = factor(gear))
  x <- eco_run(d, mpg ~ wt + gear_f, models = "ols")
  vf <- eco_diagnostics(x, tests = "vif")
  expect_equal(vf$result[1], "informational")
  expect_match(vf$details[1], "GVIF")
})

test_that("WLS weights remain aligned after formula missingness", {
  d <- mtcars
  d$hp[3] <- NA_real_
  w <- seq_len(nrow(d))
  x <- eco_run(d, mpg ~ wt + hp, models = "wls", model_args = list(wls = list(weights = w)))
  ref <- stats::lm(mpg ~ wt + hp, data = d, weights = w)
  expect_equal(stats::coef(x$models$wls), stats::coef(ref), tolerance = 1e-10)
  expect_equal(stats::nobs(x$models$wls), stats::nobs(ref))
})

test_that("generic extraction does not invent p-values", {
  co <- cbind(Estimate = c(1, 2), `Std. Error` = c(.5, .5), `z value` = c(2, 4))
  rownames(co) <- c("a", "b")
  z <- .ec_matrix_to_df(co, "demo")
  expect_true(all(is.na(z$p.value)))
})

test_that("comparison collection isolates extraction failures by model", {
  good <- stats::lm(mpg ~ wt, data = mtcars)
  bad <- good
  bad$qr <- NULL
  x <- list(
    models = list(good = good, bad = bad),
    meta = list(
      good = list(engine = "ols", outcome_type = "continuous", family = "Linear", comparison_note = "good"),
      bad = list(engine = "ols", outcome_type = "continuous", family = "Linear", comparison_note = "bad")
    ),
    outcome_type = "continuous"
  )
  class(x) <- "econcompare"

  z <- eco_compare(x, error_policy = "collect")
  expect_true(nrow(z) > 0L)
  expect_true(all(z$model == "good"))
  failures <- attr(z, "extraction_failures")
  expect_equal(failures$model, "bad")
  expect_match(failures$message, "qr|QR|proper")
  expect_error(eco_compare(x, error_policy = "stop"))
})

test_that("data columns must be uniquely named", {
  d <- mtcars[, 1:2]
  names(d) <- c("dup", "dup")
  expect_error(eco_run(d, dup ~ 1, models = "ols"), "unique column names")
})

