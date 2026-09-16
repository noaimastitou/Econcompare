test_that("model catalogue is grouped by outcome objective", {
  z <- eco_models()
  expect_true(all(c("engine", "outcome_type", "family", "estimator", "package", "comparison_note", "available") %in% names(z)))
  expect_true(all(c("continuous", "binary", "nominal", "ordinal") %in% unique(z$outcome_type)))
  expect_true(all(c("lpm", "logit", "probit", "multinomial_logit", "ordered_logit", "ordered_probit") %in% z$engine))
  expect_true(all(eco_models("binary")$outcome_type == "binary"))
})

test_that("OLS is no longer inserted automatically", {
  d <- mtcars
  d$high <- as.integer(d$mpg > median(d$mpg))
  x <- eco_run(d, high ~ wt + hp, models = "logit")
  expect_identical(names(x$models), "logit")
  expect_identical(x$outcome_type, "binary")
})

test_that("models from different outcome groups cannot be mixed", {
  d <- mtcars
  d$high <- as.integer(d$mpg > median(d$mpg))
  expect_error(
    eco_run(d, high ~ wt, models = c("ols", "logit")),
    "different outcome groups"
  )
})

test_that("continuous OLS still matches stats lm", {
  x <- eco_run(mtcars, mpg ~ wt + hp, models = "ols")
  ref <- stats::lm(mpg ~ wt + hp, data = mtcars)
  expect_equal(stats::coef(x$models$ols), stats::coef(ref), tolerance = 1e-12)
  cmp <- eco_compare(x)
  expect_true(all(c("outcome_type", "comparison_note") %in% names(cmp)))
  expect_true(all(cmp$outcome_type == "continuous"))
})

test_that("binary LPM logit and probit use the same explicit 0/1 event coding", {
  d <- mtcars
  d$high <- factor(ifelse(d$mpg > median(d$mpg), "high", "low"), levels = c("low", "high"))
  x <- eco_run(d, high ~ wt + hp, models = c("lpm", "logit", "probit"))
  expect_setequal(names(x$models), c("lpm", "logit", "probit"))
  expect_identical(x$outcome_type, "binary")
  expect_equal(stats::model.response(stats::model.frame(x$models$lpm)),
               stats::model.response(stats::model.frame(x$models$logit)))
  expect_match(x$meta$logit$response_coding, "low = 0; high = 1", fixed = TRUE)
  expect_identical(x$models$logit$family$link, "logit")
  expect_identical(x$models$probit$family$link, "probit")
})

test_that("binary models reject non-binary outcomes", {
  expect_error(
    eco_run(mtcars, mpg ~ wt, models = "logit"),
    "Binary models require"
  )
})

test_that("binary link cannot be silently overridden", {
  d <- mtcars
  d$high <- as.integer(d$mpg > median(d$mpg))
  expect_error(
    eco_run(d, high ~ wt, models = "logit", model_args = list(logit = list(family = stats::binomial("probit")))),
    "cannot override"
  )
})

test_that("multinomial logit fits a nominal factor when nnet is available", {
  skip_if_not_installed("nnet")
  d <- mtcars
  d$gear_f <- factor(d$gear)
  x <- eco_run(d, gear_f ~ wt + hp, models = "multinomial_logit")
  expect_s3_class(x$models$multinomial_logit, "multinom")
  expect_identical(x$outcome_type, "nominal")
  expect_identical(x$meta$multinomial_logit$reference_category, levels(d$gear_f)[1L])
  cmp <- eco_compare(x)
  expect_true(any(grepl(":", cmp$term, fixed = TRUE)))
  expect_true(all(is.na(cmp$p.value)))
})

test_that("multinomial logit requires explicit categorical data", {
  d <- mtcars
  d$high <- factor(ifelse(d$mpg > median(d$mpg), "high", "low"))
  expect_error(eco_run(d, high ~ wt, models = "multinomial_logit"), "at least three")
  expect_error(eco_run(mtcars, gear ~ wt, models = "multinomial_logit"), "does not convert numeric outcomes")
})

test_that("ordered logit and probit require an ordered factor", {
  skip_if_not_installed("MASS")
  d <- mtcars
  d$gear_ord <- ordered(d$gear, levels = sort(unique(d$gear)))
  x <- eco_run(d, gear_ord ~ wt + hp, models = c("ordered_logit", "ordered_probit"))
  expect_s3_class(x$models$ordered_logit, "polr")
  expect_s3_class(x$models$ordered_probit, "polr")
  expect_identical(x$outcome_type, "ordinal")
  expect_identical(x$meta$ordered_logit$link, "logistic")
  expect_identical(x$meta$ordered_probit$link, "probit")
  cmp <- eco_compare(x)
  expect_true(any(grepl("^threshold:", cmp$term)))
  expect_true(all(is.na(cmp$p.value)))
  parts <- .ec_split_ordinal_compare(cmp)
  expect_false(any(grepl("^threshold:", parts$slopes$term)))
  expect_true(nrow(parts$thresholds) > 0L)
  expect_true(all(grepl("^threshold:", parts$thresholds$term)))
})

test_that("ordered models do not guess category ordering", {
  d <- mtcars
  d$gear_f <- factor(d$gear)
  expect_error(
    eco_run(d, gear_f ~ wt, models = "ordered_logit"),
    "ordered factor"
  )
})

test_that("outcome-type detector is conservative and useful", {
  expect_identical(.ec_outcome_type(mtcars$mpg), "continuous")
  expect_identical(.ec_outcome_type(as.integer(mtcars$am)), "binary")
  expect_identical(.ec_outcome_type(c("yes", "no", "yes")), "binary")
  expect_identical(.ec_outcome_type(factor(mtcars$gear)), "nominal")
  expect_identical(.ec_outcome_type(ordered(mtcars$gear)), "ordinal")
})

test_that("data explorer exposes a suggested model group", {
  d <- mtcars
  d$gear_f <- factor(d$gear)
  z <- .ec_all_variable_summary(d)
  expect_true("model group" %in% names(z))
  expect_identical(z$`model group`[z$variable == "mpg"], "continuous")
  expect_identical(z$`model group`[z$variable == "gear_f"], "nominal")
})

test_that("sample audit uses a technical anchor rather than OLS", {
  d <- mtcars
  d$high <- as.integer(d$mpg > median(d$mpg))
  x <- eco_run(d, high ~ wt + hp, models = c("logit", "probit"))
  z <- eco_sample_audit(x, reference = "probit")
  expect_true(all(z$reference_model == "probit"))
  expect_true(all(c("matches_reference_n", "matches_reference_rows") %in% names(z)))
  expect_false(any(c("matches_ols_n", "matches_ols_rows") %in% names(z)))
})

test_that("OLS diagnostics remain available without being a reference", {
  x <- eco_run(mtcars, mpg ~ wt + hp, models = "ols")
  z <- eco_diagnostics(x, tests = c("breusch_pagan", "reset"))
  expect_setequal(z$id, c("breusch_pagan", "reset"))
})

test_that("binary interactive diagnostics stay deliberately simple", {
  d <- mtcars
  d$high <- as.integer(d$mpg > median(d$mpg))
  x <- eco_run(d, high ~ wt + hp, models = c("lpm", "logit", "probit"))
  for (m in names(x$models)) {
    z <- .ec_diag_choices_for_model(x, m)
    expect_true(all(z$id %in% "coefficient_significance"))
  }
})

test_that("residual OLS diagnostics are not offered to binary GLM engines", {
  z <- eco_diagnostic_tests()
  bp <- z[z$id == "breusch_pagan", , drop = FALSE]
  expect_false(grepl("logit|probit|lpm", bp$applies_to))
})

test_that("OLS weights remain rejected and robust OLS cannot become weighted", {
  expect_error(
    eco_run(mtcars, mpg ~ wt, models = "ols", model_args = list(ols = list(weights = mtcars$wt))),
    "OLS engine cannot use"
  )
  skip_if_not_installed("estimatr")
  expect_error(
    eco_run(mtcars, mpg ~ wt, models = "ols_robust", model_args = list(ols_robust = list(weights = mtcars$wt))),
    "reserved for the same unweighted OLS coefficients"
  )
})

test_that("quantile summary_args cannot override validated se", {
  skip_if_not_installed("quantreg")
  expect_error(
    eco_run(mtcars, mpg ~ wt, models = "quantile",
            model_args = list(quantile = list(se = "iid", summary_args = list(se = "rank")))),
    "cannot override"
  )
})

test_that("Tobit observed values are checked against censoring bounds", {
  skip_if_not_installed("censReg")
  expect_error(
    eco_run(mtcars, mpg ~ wt, models = "tobit", model_args = list(tobit = list(right = 20))),
    "exceed the declared right-censoring bound"
  )
})
