test_that("numeric outcomes are never silently converted to nominal categories", {
  expect_error(
    eco_run(mtcars, gear ~ wt, models = "multinomial_logit"),
    "does not convert numeric outcomes"
  )
})

test_that("character binary outcomes are recognized but require an explicit event", {
  d <- mtcars
  d$high <- ifelse(d$mpg > median(d$mpg), "high", "low")
  expect_identical(.ec_outcome_type(d$high), "binary")
  expect_error(eco_run(d, high ~ wt, models = "logit"), "explicit `binary_event`")
  x <- eco_run(d, high ~ wt, models = c("lpm", "logit", "probit"), binary_event = "high")
  expect_match(x$meta$logit$response_coding, "high = 1", fixed = TRUE)
  expect_equal(
    stats::model.response(stats::model.frame(x$models$lpm)),
    stats::model.response(stats::model.frame(x$models$logit))
  )
})

test_that("ordered model data drops unused levels while preserving order", {
  d <- mtcars
  d$ord <- ordered(ifelse(d$gear == 3, "low", ifelse(d$gear == 4, "mid", "high")),
                   levels = c("unused", "low", "mid", "high"))
  z <- .ec_ordinal_model_data(d, ord ~ wt)
  expect_true(is.ordered(z$data$ord))
  expect_identical(levels(z$data$ord), c("low", "mid", "high"))
})

test_that("user-defined ordinal order is explicit and complete", {
  x <- c("medium", "low", "high", "medium")
  z <- .ec_user_ordered(x, "low, medium, high")
  expect_true(is.ordered(z))
  expect_identical(levels(z), c("low", "medium", "high"))
  expect_error(.ec_user_ordered(x, "low, high"), "every observed")
})
