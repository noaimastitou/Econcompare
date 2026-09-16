test_that("data explorer reports binary and categorical balance", {
  d <- data.frame(
    y = c(0, 1, 1, 0, 1),
    group = factor(c("A", "A", "B", "B", "B")),
    stringsAsFactors = FALSE
  )
  bp <- .ec_binary_profile(d$y)
  expect_equal(sum(bp$count), 5)
  expect_equal(sum(bp$share), 1)
  cp <- .ec_category_profile(d$group)
  expect_equal(sum(cp$count), 5)
  expect_equal(sum(cp$share), 1)
  prof <- .ec_variable_profile(d, "y")
  expect_true("Category balance" %in% prof$measure)
})

test_that("two-valued numeric data are not mislabeled as binary unless coded 0/1", {
  expect_identical(.ec_simple_type(c(0, 1, 1, 0)), "Binary / numeric (0/1)")
  expect_identical(.ec_simple_type(c(1, 2, 1, 2)), "Numeric (two observed values)")
  expect_identical(.ec_outcome_type(c(1, 2, 1, 2)), "continuous")
})

test_that("relationship status handles constant variables without calling cor", {
  z <- .ec_relationship_status(rep(1, 10), seq_len(10))
  expect_false(z$ok)
  expect_match(z$message, "zero variance")
  z2 <- .ec_relationship_status(seq_len(10), seq_len(10))
  expect_true(z2$ok)
  expect_equal(z2$n, 10)
})

test_that("data explorer propagates temporal semantics", {
  d <- data.frame(
    date = as.Date(c("2024-01-01","2024-04-01","2024-07-01","2024-10-01")),
    year = rep(2024, 4),
    quarter = 1:4,
    y = rnorm(4)
  )
  z <- .ec_all_variable_summary(d)
  expect_identical(z$`model group`[z$variable == "date"], "time index candidate")
  expect_identical(z$`model group`[z$variable == "quarter"], "seasonal component")
})
