#' Cross-section diagnostics for collected models
#'
#' Computes a compatibility-aware diagnostic layer driven by the diagnostic
#' registry. Generic coefficient inference is reported only when the underlying
#' estimator exposes the required quantities; OLS residual/specification tests
#' are limited to the OLS engine where currently validated, and IV diagnostics remain IV-specific.
#'
#' @param x An `econcompare` object.
#' @param alpha Significance level used to describe hypothesis-test outcomes.
#' @param models Optional character vector of fitted model names to diagnose. `NULL` uses all fitted models.
#' @param tests Optional character vector of diagnostic ids to execute. `NULL` keeps backward-compatible behaviour and runs all compatible diagnostics. The Shiny app uses this argument to run only user-selected diagnostics.
#' @return A data.frame with diagnostic statistics, p-values, interpretation and details.
#' @export
eco_diagnostics <- function(x, alpha = 0.05, models = NULL, tests = NULL) {
  if (!inherits(x, "econcompare")) .ec_stop("`x` must be an econcompare object.")
  if (length(alpha) != 1L || !is.finite(alpha) || alpha <= 0 || alpha >= 1) .ec_stop("`alpha` must be a single number strictly between 0 and 1.")

  selected_models <- if (is.null(models)) names(x$models) else as.character(models)
  bad_models <- setdiff(selected_models, names(x$models))
  if (length(bad_models)) .ec_stop("Unknown fitted model(s): ", paste(bad_models, collapse = ", "), ".")

  granular_iv <- c("iv_weak_instruments", "iv_wu_hausman", "iv_overidentification")
  valid_test_ids <- unique(c(.ec_diag_registry()$id, granular_iv))
  if (!is.null(tests)) {
    tests <- unique(as.character(tests))
    bad_tests <- setdiff(tests, valid_test_ids)
    if (length(bad_tests)) .ec_stop("Unknown diagnostic test id(s): ", paste(bad_tests, collapse = ", "), ". Run eco_diagnostic_tests() to inspect available diagnostic ids.")
  }

  reg <- .ec_diag_registry()
  runners <- list(
    coefficient_significance = function(m,n,e) .ec_diag_coef_significance(m,n,e,alpha),
    global_significance = function(m,n,e) .ec_diag_global(m,n,e,alpha),
    confidence_intervals = function(m,n,e) .ec_diag_ci(m,n,e,alpha),
    breusch_pagan = function(m,n,e) .ec_diag_bp(m,n,e,alpha),
    white = function(m,n,e) .ec_diag_white(m,n,e,alpha),
    reset = function(m,n,e) .ec_diag_reset(m,n,e,alpha),
    vif = function(m,n,e) .ec_diag_vif(m,n,e),
    influence = function(m,n,e) .ec_diag_influence(m,n,e),
    jarque_bera = function(m,n,e) .ec_diag_jb(m,n,e,alpha),
    iv_diagnostics = function(m,n,e) .ec_diag_iv(m,n,e,alpha)
  )

  out <- list(); k <- 0L
  for (name in selected_models) {
    model <- x$models[[name]]
    engine <- .ec_model_engine(x, name)
    family_ids <- reg$id[vapply(reg$applies_to, .ec_rule_applies, logical(1), engine = engine)]

    if (!is.null(tests)) {
      requested <- tests
      requested_iv <- intersect(requested, granular_iv)
      family_ids <- intersect(family_ids, requested)
      if (length(requested_iv) && identical(engine, "ivreg") && "iv_diagnostics" %in% reg$id) {
        family_ids <- unique(c(family_ids, "iv_diagnostics"))
      }
    } else {
      requested_iv <- character()
    }

    for (id in family_ids) {
      ans <- runners[[id]](model, name, engine)
      if (identical(id, "iv_diagnostics") && !is.null(tests) && length(requested_iv)) {
        if (nrow(ans) && all(ans$id == "iv_diagnostics")) {
          iv_labels <- c(
            iv_weak_instruments = "Weak-instrument diagnostic",
            iv_wu_hausman = "Wu–Hausman endogeneity diagnostic",
            iv_overidentification = "Sargan overidentification diagnostic"
          )
          reason <- ans$interpretation[1L]
          ans <- do.call(rbind, lapply(requested_iv, function(iv_id) {
            .ec_diag_unavailable(name, engine, iv_id, unname(iv_labels[iv_id]), "Endogeneity / instruments", reason)
          }))
        } else if (nrow(ans)) {
          ans <- ans[ans$id %in% requested_iv, , drop = FALSE]
          missing_iv <- setdiff(requested_iv, ans$id)
          if (length(missing_iv)) {
            iv_labels <- c(
              iv_weak_instruments = "Weak-instrument diagnostic",
              iv_wu_hausman = "Wu–Hausman endogeneity diagnostic",
              iv_overidentification = "Sargan overidentification diagnostic"
            )
            miss_rows <- lapply(missing_iv, function(iv_id) {
              .ec_diag_unavailable(
                name, engine, iv_id, unname(iv_labels[iv_id]), "Endogeneity / instruments",
                "This IV diagnostic was selected but was not returned by ivreg for the fitted specification; for example, an overidentification test is not available for an exactly identified model."
              )
            })
            ans <- rbind(ans, do.call(rbind, miss_rows))
          }
        }
        if (!nrow(ans)) next
      }
      k <- k + 1L
      out[[k]] <- ans
    }
  }
  if (!length(out)) return(data.frame())
  ans <- do.call(rbind, out); rownames(ans) <- NULL; ans
}

