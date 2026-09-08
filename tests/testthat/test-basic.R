test_that("OLS comparison works", {
  x <- eco_run(mtcars, mpg ~ wt + hp, models = "ols")
  expect_s3_class(x, "econcompare")
  z <- eco_compare(x)
  expect_true(all(c("model", "term", "estimate", "std.error", "p.value", "nobs", "unavailable_stats") %in% names(z)))
  expect_true(nrow(z) >= 3)
})

test_that("OLS reference is automatically added", {
  x <- eco_run(mtcars, mpg ~ wt + hp, models = "robust_m")
  expect_true("ols" %in% names(x$models))
  expect_true("robust_m" %in% names(x$models))
})

test_that("model catalogue is restricted to OLS-comparable cross-section engines", {
  z <- eco_models()
  expect_true(all(c("engine", "family", "estimator", "package", "comparison_to_ols", "available") %in% names(z)))
  expect_true(all(c("ols", "ivreg", "quantile", "tobit", "heckman") %in% z$engine))
  expect_false(any(c("logit", "probit", "poisson", "beta", "multinomial", "nls") %in% z$engine))
})

test_that("length-zero statistics normalize to NA", {
  expect_true(is.na(.ec_scalar_numeric(numeric(0), missing = "na", stat = "test")))
  expect_error(.ec_scalar_numeric(numeric(0), missing = "error", stat = "test"), "length-0")
})

test_that("app helpers parse numeric values and bounds", {
  expect_equal(.ec_parse_numeric_list("0.25, 0.5, 0.75"), c(.25, .5, .75))
  expect_equal(.ec_parse_bound("-Inf", "left"), -Inf)
  expect_equal(.ec_parse_bound("Inf", "right"), Inf)
  expect_equal(.ec_parse_bound("25", "right"), 25)
})
