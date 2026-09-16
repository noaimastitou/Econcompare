.ec_fit_binary_models <- function(models, binary_info, formula, model_args, run_fit) {
  if (!any(c("lpm", "logit", "probit") %in% models)) return(invisible(NULL))
  bdata <- binary_info$data
  bcoding <- binary_info$coding

  if ("lpm" %in% models) {
    a <- .ec_args(model_args, "lpm")
    if ("weights" %in% names(a)) {
      .ec_stop("`lpm` in econcompare is the unweighted linear probability model; weighted binary models are outside the current comparison set.")
    }
    run_fit(
      "lpm", "lpm",
      function() .ec_do(stats::lm, list(formula = formula, data = bdata), a),
      meta_extra = list(response_coding = bcoding), sample_data = bdata
    )
  }

  for (eng in intersect(c("logit", "probit"), models)) {
    a <- .ec_args(model_args, eng)
    if ("weights" %in% names(a)) {
      .ec_stop("`", eng, "` in econcompare is currently unweighted so LPM, logit and probit remain a simple comparison set. Weighted binary models are outside the current scope.")
    }
    if ("family" %in% names(a)) {
      .ec_stop("`model_args$", eng, "$family` cannot override the model link. Select the desired engine instead.")
    }
    link <- if (eng == "logit") "logit" else "probit"
    run_fit(
      eng, eng,
      function() .ec_do(stats::glm, list(formula = formula, data = bdata, family = stats::binomial(link = link)), a),
      meta_extra = list(response_coding = bcoding, link = link), sample_data = bdata
    )
  }
  invisible(NULL)
}

.ec_fit_nominal_model <- function(models, nominal_info, formula, model_args, run_fit) {
  if (!"multinomial_logit" %in% models) return(invisible(NULL))
  mdata <- nominal_info$data
  a <- .ec_args(model_args, "multinomial_logit")
  if (is.null(a$trace)) a$trace <- FALSE
  if (is.null(a$Hess)) a$Hess <- TRUE
  if (is.null(a$model)) a$model <- TRUE
  run_fit(
    "multinomial_logit", "multinomial_logit",
    function() {
      .ec_require("nnet", "multinomial_logit")
      .ec_do(nnet::multinom, list(formula = formula, data = mdata), a)
    },
    meta_extra = list(reference_category = nominal_info$levels[1L], categories = nominal_info$levels),
    sample_data = mdata
  )
  invisible(NULL)
}

.ec_fit_ordinal_models <- function(models, ordinal_info, formula, model_args, run_fit) {
  if (!any(c("ordered_logit", "ordered_probit") %in% models)) return(invisible(NULL))
  odata <- ordinal_info$data
  for (eng in intersect(c("ordered_logit", "ordered_probit"), models)) {
    a <- .ec_args(model_args, eng)
    if ("method" %in% names(a)) {
      .ec_stop("`model_args$", eng, "$method` cannot override the ordered-model link. Select the desired engine instead.")
    }
    if ("weights" %in% names(a)) .ec_stop("Weighted ordered models are outside the current econcompare comparison scope.")
    if (is.null(a$Hess)) a$Hess <- TRUE
    if (is.null(a$model)) a$model <- TRUE
    method <- if (eng == "ordered_logit") "logistic" else "probit"
    run_fit(
      eng, eng,
      function() {
        .ec_require("MASS", eng)
        .ec_do(MASS::polr, list(formula = formula, data = odata, method = method), a)
      },
      meta_extra = list(link = method, ordered_levels = ordinal_info$levels),
      sample_data = odata
    )
  }
  invisible(NULL)
}
