.ec_args <- function(model_args, engine) {
  z <- model_args[[engine]]
  if (is.null(z)) list() else z
}

.ec_do <- function(fun, base_args, extra_args = list()) {
  do.call(fun, c(base_args, extra_args))
}

.ec_name_quantile <- function(tau) {
  paste0("quantile_q", gsub("\\.", "_", format(tau, trim = TRUE, scientific = FALSE)))
}

#' Estimate and collect cross-sectional econometric models
#'
#' @param data A data.frame.
#' @param formula Main model formula.
#' @param models Character vector of engines. See [eco_models()].
#' @param model_args Named list of engine-specific argument lists. For example
#'   `list(quantile = list(tau = c(.25,.5,.75)), tobit = list(left = 0),
#'   wls = list(weights = "w"))`.
#' @param iv_formula Optional IV formula, e.g. `y ~ x1 + x2 | z1 + x2`.
#' @param fixest_formula Optional formula passed to `fixest::feols()`.
#' @param vcov Optional `vcov` argument passed to `fixest::feols()`.
#' @param selection_formula Selection equation for the Heckman model.
#' @param outcome_formula Outcome equation for the Heckman model. Defaults to `formula`.
#' @return An object of class `econcompare`.
#' @export
eco_run <- function(data, formula, models = "ols", model_args = list(),
                    iv_formula = NULL, fixest_formula = NULL, vcov = NULL,
                    selection_formula = NULL, outcome_formula = NULL) {
  if (!is.data.frame(data)) .ec_stop("`data` must be a data.frame.")
  if (!inherits(formula, "formula")) .ec_stop("`formula` must be a formula.")
  if (!is.list(model_args)) .ec_stop("`model_args` must be a named list.")

  reg <- .ec_registry()
  if (!"ols" %in% models) {
    message("econcompare: adding `ols` as the reference model.")
    models <- c("ols", models)
  }
  models <- unique(models)
  bad <- setdiff(models, reg$engine)
  if (length(bad)) .ec_stop("Unsupported model(s): ", paste(bad, collapse = ", "),
                            ". Run eco_models() to see available engines.")

  fits <- list()
  meta <- list()
  add_fit <- function(name, fit, engine) {
    fits[[name]] <<- fit
    meta[[name]] <<- list(engine = engine,
                          family = reg$family[match(engine, reg$engine)],
                          estimator = reg$estimator[match(engine, reg$engine)])
  }

  if ("ols" %in% models) {
    add_fit("ols", .ec_do(stats::lm, list(formula = formula, data = data), .ec_args(model_args, "ols")), "ols")
  }
  if ("wls" %in% models) {
    a <- .ec_args(model_args, "wls")
    w <- a$weights
    if (is.character(w) && length(w) == 1L) w <- data[[w]]
    if (is.null(w)) .ec_stop("`wls` requires model_args$wls$weights (a vector or column name).")
    a$weights <- NULL
    add_fit("wls", .ec_do(stats::lm, list(formula = formula, data = data, weights = w), a), "wls")
  }
  if ("ols_robust" %in% models) {
    .ec_require("estimatr", "ols_robust")
    a <- .ec_args(model_args, "ols_robust")
    if (is.null(a$se_type)) a$se_type <- "HC3"
    add_fit("ols_robust", .ec_do(estimatr::lm_robust, list(formula = formula, data = data), a), "ols_robust")
  }
  if ("robust_m" %in% models) {
    .ec_require("MASS", "robust_m")
    add_fit("robust_m", .ec_do(MASS::rlm, list(formula = formula, data = data), .ec_args(model_args, "robust_m")), "robust_m")
  }
  if ("fixest" %in% models) {
    .ec_require("fixest", "fixest")
    fml <- if (is.null(fixest_formula)) formula else fixest_formula
    a <- .ec_args(model_args, "fixest")
    if (!is.null(vcov) && is.null(a$vcov)) a$vcov <- vcov
    add_fit("fixest", .ec_do(fixest::feols, list(fml = fml, data = data), a), "fixest")
  }
  if ("ivreg" %in% models) {
    .ec_require("ivreg", "ivreg")
    fml <- if (is.null(iv_formula)) model_args$ivreg$formula else iv_formula
    if (is.null(fml)) .ec_stop("`ivreg` requires `iv_formula` or model_args$ivreg$formula.")
    a <- .ec_args(model_args, "ivreg"); a$formula <- NULL
    add_fit("ivreg", .ec_do(ivreg::ivreg, list(formula = fml, data = data), a), "ivreg")
  }
  if ("quantile" %in% models) {
    .ec_require("quantreg", "quantile")
    a <- .ec_args(model_args, "quantile")
    taus <- if (is.null(a$tau)) 0.5 else a$tau
    a$tau <- NULL
    for (tau in taus) {
      nm <- .ec_name_quantile(tau)
      add_fit(nm, .ec_do(quantreg::rq, list(formula = formula, data = data, tau = tau), a), "quantile")
    }
  }
  if ("tobit" %in% models) {
    .ec_require("censReg", "tobit")
    add_fit("tobit", .ec_do(censReg::censReg, list(formula = formula, data = data), .ec_args(model_args, "tobit")), "tobit")
  }
  if ("heckman" %in% models) {
    .ec_require("sampleSelection", "heckman")
    sel <- if (is.null(selection_formula)) model_args$heckman$selection else selection_formula
    out_formula <- if (is.null(outcome_formula)) formula else outcome_formula
    if (is.null(sel)) .ec_stop("`heckman` requires `selection_formula` or model_args$heckman$selection.")
    a <- .ec_args(model_args, "heckman"); a$selection <- NULL; a$outcome <- NULL
    add_fit("heckman", .ec_do(sampleSelection::selection, list(selection = sel, outcome = out_formula, data = data), a), "heckman")
  }

  out <- list(call = match.call(), formula = formula, models = fits, meta = meta,
              data_n = nrow(data), created = Sys.time(), version = "0.6.0")
  class(out) <- "econcompare"
  out
}
