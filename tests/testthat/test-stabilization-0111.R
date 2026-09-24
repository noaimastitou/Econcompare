# Regression cases reproduced during the 0.11.0 runtime audit.
.stab_data <- function() {
  set.seed(11101)
  n <- 160L
  trend <- cumsum(rnorm(n))
  data.frame(year = 1800L + seq_len(n), a = trend + rnorm(n),
             b = 0.7 * trend + rnorm(n), c = -0.4 * trend + rnorm(n))
}

test_that("time preparation preserves row identity and accepts tibbles", {
  skip_if_not_installed("tibble")
  d <- .stab_data()[160:1, ]
  rownames(d) <- NULL
  td <- tibble::as_tibble(d)
  expect_warning(p <- .ec_prepare_time_data(td, "year"), NA)
  ref <- .ec_prepare_time_data(d, "year")
  expect_identical(p$data, ref$data)
  expect_identical(rownames(p$data), as.character(160:1))
  expect_true(p$reordered)
  expect_identical(names(p$data), names(d))
  expect_identical(p$data$year, sort(d$year))
})

test_that("VAR output and diagnostics agree with a direct vars fit", {
  skip_if_not_installed("vars")
  d <- .stab_data()
  vs <- c("a", "b", "c")
  for (v in vs) d[[v]] <- c(NA_real_, diff(d[[v]]))
  d <- d[-1, ]; rownames(d) <- NULL
  for (det in c("const", "none", "trend", "both")) {
    fit <- eco_system_run(d, vs, "year", p = 4, deterministic = det)
    ref <- vars::VAR(d[vs], p = 4, type = det)
    tab <- eco_compare(fit)
    for (eq in vs) {
      co <- coef(summary(ref$varresult[[eq]]))
      z <- tab[tab$model == paste0("VAR: ", eq), ]
      expect_setequal(z$term, rownames(co))
      expect_equal(nrow(z), nrow(co))
      z <- z[match(rownames(co), z$term), ]
      expect_equal(unname(as.matrix(z[c("estimate", "std.error", "statistic", "p.value")])),
                   unname(co), tolerance = 1e-10)
    }
    expect_true(all(fit$sample_info$n_used == nrow(d) - 4L))
    expect_identical(fit$sample_rows[[1]], as.character(5:nrow(d)))
    dg <- eco_system_diagnostics(fit, 8)
    expect_equal(nrow(dg$roots), 12L)
    expect_equal(sort(dg$roots$modulus), sort(vars::roots(ref)), tolerance = 1e-10)
    pt <- vars::serial.test(ref, lags.pt = 8, type = "PT.asymptotic")$serial
    expect_equal(dg$serial_correlation$statistic, unname(pt$statistic))
    expect_equal(dg$serial_correlation$p.value, pt$p.value)
    expect_equal(dg$serial_correlation$df, unname(pt$parameter))
  }
})

test_that("system estimation is invariant to tibble input and reordered rows", {
  skip_if_not_installed("vars")
  skip_if_not_installed("urca")
  skip_if_not_installed("tibble")
  d <- .stab_data()[160:1, ]; rownames(d) <- NULL
  for (engine in c("var", "vecm")) {
    ref <- eco_system_run(d, c("a", "b", "c"), "year", model = engine, p = 4, rank = 2)
    expect_warning(fit <- eco_system_run(tibble::as_tibble(d), c("a", "b", "c"),
                                         "year", model = engine, p = 4, rank = 2), NA)
    expect_equal(eco_compare(fit), eco_compare(ref))
    expect_identical(fit$sample_rows, ref$sample_rows)
    expect_identical(fit$sample_rows[[1]], as.character(156:1))
    expect_equal(eco_system_diagnostics(fit, 8), eco_system_diagnostics(ref, 8))
  }
})

test_that("VECM K=4 retains all three difference lags and n-K observations", {
  skip_if_not_installed("vars")
  skip_if_not_installed("urca")
  d <- .stab_data()
  vs <- c("a", "b", "c")
  for (rank in 1:2) {
    fit <- eco_system_run(d, vs, "year", model = "vecm", p = 4, rank = rank,
                          johansen_type = "eigen", ecdet = "const", spec = "transitory")
    jo <- urca::ca.jo(d[vs], K = 4, type = "eigen", ecdet = "const", spec = "transitory")
    ref <- urca::cajorls(jo, r = rank)
    co <- coef(ref$rlm)
    tab <- eco_compare(fit)
    expected <- c(paste0("ect", seq_len(rank)), unlist(lapply(1:3, function(l) paste0(vs, ".dl", l))))
    for (j in seq_len(ncol(co))) {
      z <- tab[tab$model == paste0("VECM: ", colnames(co)[j]), ]
      expect_setequal(z$term, expected)
      expect_equal(nrow(z), 9L + rank)
      expect_equal(z$estimate[match(rownames(co), z$term)], unname(co[, j]), tolerance = 1e-10)
    }
    expect_true(all(tab$nobs == 156L))
    expect_identical(fit$sample_rows[[1]], as.character(5:160))
    dg <- eco_system_diagnostics(fit, 8)
    expect_equal(nrow(dg$roots), 12L)
    expect_true(all(is.finite(dg$roots$modulus)))
    pt <- vars::serial.test(vars::vec2var(jo, r = rank), lags.pt = 8, type = "PT.asymptotic")$serial
    expect_equal(dg$serial_correlation$statistic, unname(pt$statistic))
    expect_equal(dg$serial_correlation$p.value, pt$p.value)
    expect_equal(dg$serial_correlation$df, unname(pt$parameter))
    expect_false(any(grepl("|", eco_vecm_rank_test(fit)$null_rank_hypothesis, fixed = TRUE)))
    expect_equal(eco_vecm_rank_test(fit)$statistic, as.numeric(jo@teststat))
  }
})

test_that("constant and collinear systems fail before coefficient extraction", {
  skip_if_not_installed("vars")
  skip_if_not_installed("urca")
  for (kind in c("constant", "collinear")) {
    d <- .stab_data()
    d$c <- if (kind == "constant") rep(1, nrow(d)) else 2 * d$a
    for (engine in c("var", "vecm")) {
      expect_error(eco_system_run(d, c("a", "b", "c"), "year", model = engine, p = 4, rank = 2), kind)
    }
    expect_error(eco_var_lag_selection(d, c("a", "b", "c"), "year"), kind)
    expect_error(eco_johansen_test(d, c("a", "b", "c"), "year"), kind)
  }
})

test_that("lag-induced aliasing is rejected even for independent contemporaneous series", {
  skip_if_not_installed("vars")
  set.seed(11102)
  a <- rnorm(161)
  d <- data.frame(year = 1901:2060, a = a[-161], b = a[-1])
  expect_error(eco_system_run(d, c("a", "b"), "year", p = 2), "rank-deficient")
})

test_that("old singular objects produce unavailable diagnostics instead of eigen errors", {
  skip_if_not_installed("vars")
  d <- .stab_data()
  fit <- eco_system_run(d, c("a", "b", "c"), "year", p = 4)
  # Emulate a 0.11.0 object whose VAR retained aliased coefficients.
  fit$system_model$varresult$a$coefficients[1] <- NA_real_
  expect_error(dg <- eco_system_diagnostics(fit, 8), NA)
  expect_equal(nrow(dg$roots), 0L)
  expect_match(dg$root_note, "Unavailable.*non-finite")
})

test_that("rank, lag and horizon boundaries fail cleanly", {
  skip_if_not_installed("vars")
  skip_if_not_installed("urca")
  d <- .stab_data()
  for (r in list(0, 3, 1.5, NA_real_, Inf, "2", TRUE, 1e30)) {
    expect_error(eco_system_run(d, c("a", "b", "c"), "year", model = "vecm", p = 4, rank = r), "rank")
  }
  for (p in list(0, 2.5, NA_real_, Inf, "2", 1e30)) {
    expect_error(eco_system_run(d, c("a", "b", "c"), "year", p = p), "p")
  }
  expect_error(eco_johansen_test(d[1:12, ], c("a", "b", "c"), "year", K = 2), "parameterized|too large")
  fit <- eco_system_run(d, c("a", "b", "c"), "year", p = 4)
  for (h in list(1, 4, NA_real_, Inf, "8", 1e30, 156)) {
    expect_error(eco_system_diagnostics(fit, h), "serial_lags")
  }
})

test_that("ECM no-intercept ECT aligns and HAC agrees with sandwich", {
  skip_if_not_installed("sandwich")
  skip_if_not_installed("lmtest")
  d <- .stab_data()
  fit <- eco_ecm_run(d, a ~ b + c, "year", p = 1, q = 2,
                     long_run_intercept = FALSE, inference = "HAC", hac_lag = 3)
  expect_false("(Intercept)" %in% names(coef(fit$long_run_model)))
  ref <- lm(a ~ b + c - 1, data = d)
  expect_equal(unname(fit$error_correction_term), unname(residuals(ref)))
  m <- fit$models$ecm
  mf <- model.frame(m)
  rows <- as.integer(rownames(mf))
  expect_equal(mf$ECT_L1, unname(residuals(ref)[rows - 1L]))
  co <- lmtest::coeftest(m, vcov. = sandwich::NeweyWest(m, lag = 3, prewhite = FALSE, adjust = TRUE))
  tab <- eco_compare(fit)
  expect_equal(tab$std.error[match(rownames(co), tab$term)], unname(co[, 2]), tolerance = 1e-10)
  expect_true(all(tab$inference == "HAC"))
  expect_error(eco_ecm_run(d[1:30, ], a ~ b + c, "year", p = 6, q = 6), "over-parameterized|too large")
})

test_that("missing values, calendar gaps and duplicate dates remain explicit errors", {
  skip_if_not_installed("vars")
  d <- .stab_data()
  miss <- d; miss$a[50] <- NA_real_
  dup <- d; dup$year[50] <- dup$year[49]
  expect_error(eco_system_run(miss, c("a", "b"), "year"), "complete numeric sample")
  expect_error(eco_system_run(d[-50, ], c("a", "b"), "year"), "complete regular time grid")
  expect_error(eco_system_run(dup, c("a", "b"), "year"), "duplicated")
})


test_that("cross-sectional comparison accepts new and legacy objects", {
  for (spec in list(list(formula = mpg ~ wt + hp, models = "ols"),
                    list(formula = am ~ wt + hp, models = "lpm"))) {
    fit <- eco_run(mtcars, spec$formula, models = spec$models)
    expect_identical(fit$analysis_type, "cross_section")
    expect_error(tab <- eco_compare(fit), NA)
    expect_true(nrow(tab) > 0L)
    fit$analysis_type <- NULL
    expect_equal(eco_compare(fit), tab)
  }
})
