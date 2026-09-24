# Self-contained synthetic demonstration, not empirical evidence or a causal design.
library(econcompare)
needed <- c("survival", "fixest", "plm")
if (!all(vapply(needed, requireNamespace, logical(1), quietly = TRUE))) stop("Install survival, fixest and plm to run this example.")
set.seed(140014)
d <- expand.grid(period = 1:8, id = sprintf("person%02d", 1:50), stringsAsFactors = FALSE)
a <- rep(rnorm(50), each = 8)
d$x <- rnorm(nrow(d)); d$w <- rnorm(nrow(d))
d$binary <- rbinom(nrow(d), 1, plogis(.3 * d$x - .2 * d$w + a))
d$count <- rpois(nrow(d), exp(.3 + .25 * d$x - .15 * d$w + .2 * a))
d$z1 <- rnorm(nrow(d)); d$z2 <- rnorm(nrow(d)); u <- rnorm(nrow(d))
d$endogenous_x <- d$z1 + .6 * d$z2 + .5 * d$w + u
d$y <- 1.2 * d$endogenous_x + .3 * d$w + a + .7 * u + rnorm(nrow(d))

binary_fit <- eco_panel_run(d, binary ~ x + w, "id", "period", outcome_type = "binary", inference = "classical")
count_fit <- eco_panel_run(d, count ~ x + w, "id", "period", outcome_type = "count", inference = "cluster_id")
iv_fit <- eco_panel_run(d, y ~ endogenous_x + w, "id", "period",
  models = c("panel_fe_individual", "panel_fe_iv"), endogenous = "endogenous_x",
  instruments = c("z1", "z2"), inference = "cluster_id")
for (fit in list(binary_fit, count_fit, iv_fit)) {
  print(eco_compare(fit))
  print(eco_sample_audit(fit))
  print(fit$sample_exclusions)
}
print(eco_panel_diagnostics(iv_fit, tests = "iv_first_stage"))
# The instrument exclusions hold by construction here; real research needs its own justification.
# Exact logit classical inference is not a substitute for a robust covariance.
# For UI testing: eco_app(d), choose Panel mode, indexes and the appropriate outcome family.
