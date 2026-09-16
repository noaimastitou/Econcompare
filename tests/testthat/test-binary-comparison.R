test_that("binary models are kept unweighted in the simple comparison set", {
  d <- mtcars
  d$high <- as.integer(d$mpg > median(d$mpg))
  for (eng in c("lpm", "logit", "probit")) {
    expect_error(
      eco_run(d, high ~ wt, models = eng,
              model_args = stats::setNames(list(list(weights = rep(1, nrow(d)))), eng)),
      "weighted|unweighted"
    )
  }
})

test_that("binary comparison exposes common descriptive prediction summaries", {
  d <- mtcars
  d$high <- as.integer(d$mpg > median(d$mpg))
  x <- eco_run(d, high ~ wt + hp, models = c("lpm", "logit", "probit"))
  z <- eco_binary_compare(x)
  expect_setequal(z$model, c("lpm", "logit", "probit"))
  expect_true(all(c("accuracy_0_5", "rmse", "brier_score", "mean_prediction", "predictions_in_0_1") %in% names(z)))
  expect_true(all(z$accuracy_0_5 >= 0 & z$accuracy_0_5 <= 1))
  expect_true(all(z$event_rate >= 0 & z$event_rate <= 1))
})

test_that("binary model-fit display does not rank LPM with binomial likelihood metrics", {
  d <- mtcars
  d$high <- as.integer(d$mpg > median(d$mpg))
  x <- eco_run(d, high ~ wt + hp, models = c("lpm", "logit", "probit"))
  z <- .ec_model_fit_table(eco_compare(x), "binary")
  lpm <- z[z$engine == "lpm", , drop = FALSE]
  glm <- z[z$engine %in% c("logit", "probit"), , drop = FALSE]
  if ("aic" %in% names(z)) expect_true(is.na(lpm$aic))
  if ("r2" %in% names(z)) expect_true(all(is.na(glm$r2)))
  expect_true(all(c("Gaussian linear fit", "Binomial likelihood") %in% z$fit_basis))
})
