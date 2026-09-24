# Static panel tutorial: an association analysis, not a causal design.
library(econcompare)
if (!requireNamespace("plm", quietly = TRUE) || !requireNamespace("sandwich", quietly = TRUE)) stop("Install plm and sandwich to run this tutorial.")
data("Grunfeld", package = "plm")
print(eco_panel_audit(Grunfeld, "firm", "year"))
fit <- eco_panel_run(Grunfeld, inv ~ value + capital, "firm", "year",
  models = eco_panel_models()$engine[1:8], inference = "cluster_id")
print(eco_compare(fit))
print(eco_sample_audit(fit))
print(eco_panel_diagnostics(fit, tests = c("serial", "dependence", "hausman", "mundlak")))
# With ten firms, cluster-based asymptotic inference is particularly uncertain.
# Classically calibrated diagnostics are shown separately, with different assumptions.
classical <- eco_panel_run(Grunfeld, inv ~ value + capital, "firm", "year",
  models = eco_panel_models()$engine[1:8], inference = "classical")
print(eco_panel_diagnostics(classical, tests = c("effects_f", "effects_lm")))
# Raw plm/lm objects, exact covariance and excluded rows remain inspectable:
str(fit$panel_metadata)
print(fit$term_status)
print(fit$sample_exclusions)
# Interactive path: eco_app(Grunfeld), then explicitly choose Panel mode.

# Source-row counts differ from transformed observation counts.
print(fit$mean_terms)
print(fit$components)
print(utils::head(fit$transformations$panel_fd))
