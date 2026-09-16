test_that("calendar frequency detection handles month length and leap years", {
  d_month <- data.frame(date = as.Date(c("2024-01-01","2024-02-01","2024-03-01","2024-04-01")), y=1:4)
  a <- eco_time_audit(d_month, "date")
  expect_equal(a$frequency, "monthly")
  expect_true(a$regular_spacing)
  expect_equal(a$missing_periods, 0L)

  d_q <- data.frame(date=as.Date(c("2023-01-01","2023-04-01","2023-07-01","2023-10-01","2024-01-01")),y=1:5)
  aq <- eco_time_audit(d_q,"date")
  expect_equal(aq$frequency,"quarterly")
  expect_true(aq$regular_spacing)
})

test_that("calendar gaps are detected and lagged models refuse observation-lag substitution", {
  d <- data.frame(
    date=as.Date(c("2020-01-01","2020-04-01","2020-10-01","2021-01-01","2021-04-01","2021-07-01","2021-10-01","2022-01-01")),
    y=1:8, x=seq(2,16,2)
  )
  a <- eco_time_audit(d,"date")
  expect_equal(a$frequency,"quarterly")
  expect_false(a$regular_spacing)
  expect_equal(a$missing_periods,1L)
  expect_error(eco_time_run(d,y~x,time="date",models="distributed_lag",q=1),"complete regular time grid")
  expect_s3_class(eco_time_run(d,y~x,time="date",models="time_static"),"econcompare")
})

test_that("manual type overrides are explicit and validated", {
  d <- data.frame(year=c("2020","2021","2022","2023"), x=c("1.0","2.0","3.0","4.0"), group=c("a","b","a","b"), stringsAsFactors=FALSE)
  z <- eco_apply_types(d,c(year="time_year",x="continuous",group="nominal"))
  expect_true(is.integer(z$year))
  expect_true(is.numeric(z$x))
  expect_true(is.factor(z$group))
  expect_error(eco_apply_types(d,c(x="binary")),"exactly two observed states")
})

test_that("stationarity tests do not collapse internal missing observations", {
  skip_if_not_installed("tseries")
  d <- data.frame(year=2000:2015, y=rnorm(16))
  d$y[8] <- NA_real_
  expect_error(eco_stationarity_tests(d,"y","year"),"does not collapse the time grid")
})

test_that("overparameterized lag specifications are rejected before estimation", {
  d <- data.frame(year=2000:2015, y=rnorm(16), x1=rnorm(16), x2=rnorm(16), x3=rnorm(16))
  expect_error(eco_time_run(d,y~x1+x2+x3,time="year",models="ardl",p=3,q=3),"over-parameterized|too large")
})

test_that("HAC inference is opt-in and recorded", {
  skip_if_not_installed("sandwich")
  skip_if_not_installed("lmtest")
  set.seed(1)
  d <- data.frame(year=2000:2020, y=rnorm(21), x=rnorm(21))
  fit <- eco_time_run(d,y~x,time="year",models="time_static",inference="HAC",hac_lag=2)
  cmp <- eco_compare(fit)
  expect_true("inference" %in% names(cmp))
  expect_true(all(cmp$inference=="HAC"))
})

test_that("calendar detection accepts month-end schedules", {
  d <- data.frame(date=as.Date(c("2024-01-31","2024-02-29","2024-03-31","2024-04-30")), y=1:4)
  a <- eco_time_audit(d,"date")
  expect_equal(a$frequency,"monthly")
  expect_true(a$regular_spacing)
})

test_that("type overrides are retained in fitted objects for reproducibility", {
  d <- data.frame(year=as.character(2000:2012), y=rnorm(13), x=as.character(seq_len(13)))
  d2 <- eco_apply_types(d, c(year="time_year", x="continuous"))
  fit <- eco_time_run(d2, y ~ x, time="year", models="time_static")
  expect_identical(unname(fit$type_overrides[c("year","x")]), c("time_year","continuous"))
})

test_that("manual ordinal type overrides require an explicit substantive order", {
  d <- data.frame(rating = c("high", "low", "medium", "high"), stringsAsFactors = FALSE)
  expect_error(
    eco_apply_types(d, c(rating = "ordinal")),
    "requires an explicit category order"
  )
  z <- eco_apply_types(
    d,
    c(rating = "ordinal"),
    ordinal_levels = list(rating = c("low", "medium", "high"))
  )
  expect_true(is.ordered(z$rating))
  expect_identical(levels(z$rating), c("low", "medium", "high"))
  expect_identical(attr(z, "econcompare_ordinal_levels")$rating, c("low", "medium", "high"))
  expect_error(
    eco_apply_types(d, c(rating = "ordinal"), ordinal_levels = list(rating = c("low", "high"))),
    "every observed category exactly once"
  )
})

test_that("ADF deterministic component is explicit and recorded", {
  skip_if_not_installed("urca")
  set.seed(42)
  d <- data.frame(year = 1980:2025, y = cumsum(rnorm(46)))
  z <- eco_stationarity_tests(d, "y", "year", adf_k = 1, adf_deterministic = "trend", kpss_null = "Trend")
  adf <- z[z$test == "ADF", , drop = FALSE]
  expect_equal(nrow(adf), 1L)
  expect_identical(adf$deterministic, "trend")
  expect_true(is.finite(adf$statistic))
  expect_true(is.finite(adf$critical_5pct))
  expect_true(is.na(adf$p.value))
})

test_that("HAC and Breusch-Godfrey refuse irregular or incomplete calendars", {
  d <- data.frame(
    year = c(2000, 2001, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012),
    y = rnorm(12), x = rnorm(12)
  )
  expect_error(
    eco_time_run(d, y ~ x, time = "year", models = "time_static", inference = "HAC"),
    "complete regular time grid"
  )
  fit <- eco_time_run(d, y ~ x, time = "year", models = "time_static", inference = "classical")
  expect_error(
    eco_time_diagnostics(fit, bg_order = 1),
    "complete regular time grid"
  )
})

test_that("stationarity diagnostics only test explicitly requested series", {
  skip_if_not_installed("urca")
  set.seed(3)
  d <- data.frame(year = 1990:2025, y = rnorm(36), x = rnorm(36), z = rnorm(36))
  ans <- eco_stationarity_tests(d, c("y", "z"), "year", adf_k = 1, adf_deterministic = "drift")
  expect_setequal(unique(ans$variable), c("y", "z"))
  expect_false("x" %in% ans$variable)
})

test_that("variable-specific explanatory lag API creates the requested lag structure", {
  set.seed(4)
  d <- data.frame(year = 1980:2025, y = rnorm(46), x1 = rnorm(46), x2 = rnorm(46))
  fit <- eco_time_run(
    d, y ~ x1 + x2, time = "year", models = "ardl", p = 1, q = 1,
    q_by_var = c(x1 = 3, x2 = 1)
  )
  expect_identical(unname(fit$q_by_var[c("x1", "x2")]), c(3L, 1L))
  m <- fit$models[[1L]]
  cn <- names(stats::coef(m))
  expect_true(all(c("x1_L1", "x1_L2", "x1_L3", "x2_L1") %in% cn))
  expect_false("x2_L2" %in% cn)
  expect_true(is.na(fit$q))
})

test_that("time-series fits expose structured temporal metadata", {
  set.seed(5)
  d <- data.frame(year = 2000:2025, y = rnorm(26), x = rnorm(26))
  fit <- eco_time_run(d, y ~ x, time = "year", models = c("time_static", "dynamic_regression"), p = 1)
  md <- fit$time_metadata
  expect_true(is.list(md))
  expect_identical(md$time_variable, "year")
  expect_identical(md$frequency, "annual")
  expect_true(md$complete_regular_grid)
  expect_true(is.data.frame(md$index))
  expect_true(all(c("original_row", "time_display", "time_numeric") %in% names(md$index)))
  expect_true(length(md$model_specs) == length(fit$models))
})

test_that("comparison wording describes an identical estimation sample, not merely equal n", {
  set.seed(6)
  d <- data.frame(year = 2000:2025, y = rnorm(26), x = rnorm(26))
  fit <- eco_time_run(d, y ~ x, time = "year", models = "time_static")
  cmp <- eco_compare(fit)
  expect_true(all(cmp$sample_comparable == "same estimation sample across compared models"))
})

test_that("HAC and Breusch-Godfrey also reject gaps created by model missing values", {
  set.seed(13)
  d <- data.frame(year=2000:2020, y=rnorm(21), x=rnorm(21))
  d$x[10] <- NA_real_
  expect_error(
    eco_time_run(d, y ~ x, time="year", models="time_static", inference="HAC"),
    "missing model values create gaps"
  )
  fit <- eco_time_run(d, y ~ x, time="year", models="time_static", inference="classical")
  expect_error(
    eco_time_diagnostics(fit, bg_order=1),
    "estimation sample contains internal calendar gaps"
  )
})
