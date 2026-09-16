test_that("time structure detection is conservative and robust", {
  d <- data.frame(year = 2000:2020, y = rnorm(21), x = rnorm(21))
  z <- eco_data_structure(d)
  expect_identical(z$structure, "time_series")
  expect_identical(z$time_variable, "year")

  cs <- data.frame(id = 1:20, birth_date = as.Date("1980-01-01") + cumsum(sample(20:500, 20, replace=TRUE)), y = rnorm(20))
  z2 <- eco_data_structure(cs)
  expect_false(identical(z2$structure, "time_series"))

  amb <- data.frame(date = c("01/02/2020", "02/03/2020", "03/04/2020", "04/05/2020"), y=1:4)
  expect_identical(eco_data_structure(amb)$structure, "cross_section")
})

test_that("time audit catches duplicates and unsorted rows", {
  d <- data.frame(year = c(2003, 2000, 2001, 2001, 2002), y=1:5, x=6:10)
  a <- eco_time_audit(d, "year")
  expect_false(a$sorted_ascending)
  expect_equal(a$duplicate_time, 1L)
  expect_error(eco_time_run(d, y ~ x, time="year"), "duplicated time")
})

test_that("time models create expected lagged regressions", {
  set.seed(1)
  d <- data.frame(year=2000:2025)
  d$x <- rnorm(nrow(d))
  d$y <- 0.5*d$x + rnorm(nrow(d))
  fit <- eco_time_run(d, y ~ x, time="year",
                      models=c("time_static","distributed_lag","dynamic_regression","ardl"), p=1, q=2)
  expect_s3_class(fit, "econcompare")
  expect_identical(fit$analysis_type, "time_series")
  expect_length(fit$models, 4L)
  cmp <- eco_compare(fit, error_policy="collect")
  expect_true(any(cmp$term == "x_L1"))
  expect_true(any(cmp$term == "y_L1"))
  expect_true(nrow(eco_sample_audit(fit)) == 4L)
})

test_that("time models reject time variable as a regressor", {
  d <- data.frame(year=2000:2015, y=rnorm(16), x=rnorm(16))
  expect_error(eco_time_run(d, y ~ x + year, time="year"), "time-index variable")
})
