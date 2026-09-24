# New panel families: explicit outcome and instrument contracts.
.ec_panel_iv_spec <- function(data, formula, id, time, endogenous, instruments) {
  x <- all.vars(formula)[-1L]
  valid <- function(z) is.character(z) && length(z) > 0L && !anyNA(z) && !anyDuplicated(z)
  if (!valid(endogenous) || any(!endogenous %in% x)) .ec_stop("FE-IV requires `endogenous` to name selected regressors explicitly.")
  if (!valid(instruments) || any(!instruments %in% names(data)) || any(instruments %in% c(all.vars(formula), id, time))) .ec_stop("Choose distinct excluded instruments, present in data and outside the response, regressors and panel indexes.")
  if (length(instruments) < length(endogenous)) .ec_stop("Underidentified FE-IV: fewer excluded instruments than endogenous regressors.")
  if (any(!vapply(data[unique(c(all.vars(formula), instruments))], is.numeric, logical(1)))) .ec_stop("FE-IV currently requires numeric response, regressors and instruments; encode contrasts explicitly.")
  list(endogenous = endogenous, instruments = instruments, exogenous = setdiff(x, endogenous))
}

.ec_panel_special_fit <- function(nm, dat, formula, full_mm, original, inference, iv) {
  vars <- all.vars(formula); yname <- vars[1L]
  if (any(vapply(dat[vars], is.factor, logical(1)))) .ec_stop("These panel engines currently require numeric/logical regressors; encode contrasts explicitly.")
  ids <- dat$.ec_panel_id; y <- dat[[yname]]
  size <- stats::ave(rep(1, nrow(dat)), ids, FUN = length)
  if (nm == "panel_clogit") {
    .ec_require("survival", "conditional panel logit")
    if (any(!y %in% c(0, 1))) .ec_stop("Conditional logit requires an explicitly coded numeric 0/1 outcome; 1 is the event.")
    if (inference != "classical") .ec_stop("Exact conditional logit currently supports classical conditional-likelihood covariance only. Cluster covariance is not implemented for this exact likelihood; select classical explicitly or deselect this engine.")
    total <- stats::ave(y, ids, FUN = sum)
    keep <- total > 0 & total < size
    reason <- "individual has no within-individual variation in the binary outcome"
  } else if (nm == "panel_poisson") {
    .ec_require("fixest", "fixed-effects panel Poisson")
    if (any(y < 0 | abs(y - round(y)) > 1e-8)) .ec_stop("Panel Poisson currently requires non-negative integer counts. Rates and non-integer PPML outcomes are outside this release's scope.")
    keep <- stats::ave(y, ids, FUN = sum) > 0 & size > 1
    reason <- "individual has an all-zero count outcome or is a singleton"
  } else {
    keep <- size > 1
    reason <- "singleton individual has no within information for FE-IV"
  }
  pos <- which(keep); d <- dat[pos, , drop = FALSE]; rownames(d) <- as.character(pos)
  g <- length(unique(d$.ec_panel_id))
  if (length(pos) < 4L || g < 2L) .ec_stop("At least two informative individuals and four observations are required after estimator-specific exclusions.")
  mm <- full_mm[pos, , drop = FALSE]
  centered <- apply(mm, 2L, function(v) v - stats::ave(v, d$.ec_panel_id, FUN = mean))
  varying <- vapply(seq_len(ncol(mm)), function(j) max(abs(centered[, j])) > 1e-10 * max(1, max(abs(mm[, j]))), logical(1))
  if (!any(varying)) .ec_stop("No within-varying regressors remain.")
  if (qr(centered[, varying, drop = FALSE])$rank < sum(varying)) .ec_stop("Unidentified regressors after within transformation; remove collinear terms explicitly.")
  status <- data.frame(term = colnames(full_mm), status = ifelse(varying, "estimated", "absorbed by individual fixed effects"))
  # No factor expansion here: one design column per numeric/logical regressor.
  slopes <- vars[-1L][attr(full_mm, "assign")[varying]]
  f <- .ec_formula(yname, slopes)
  first_stages <- list(); first_stage_tests <- data.frame()
  if (nm == "panel_clogit") {
    # clogit constructs an unqualified coxph call evaluated in its caller.
    # Bind the functions locally, without attaching survival to the search path.
    coxph <- survival::coxph
    Surv <- survival::Surv
    strata <- survival::strata
    env <- new.env(parent = environment(formula))
    env$coxph <- coxph; env$Surv <- Surv; env$strata <- strata
    cf <- stats::as.formula(paste(.ec_quote_name(yname), "~", paste(vapply(slopes, .ec_quote_name, character(1)), collapse = " + "), "+ strata(.ec_panel_id)"), env = env)
    fit <- withCallingHandlers(survival::clogit(cf, data = d, method = "exact", model = TRUE, x = TRUE,
      na.action = stats::na.fail, control = survival::coxph.control(iter.max = 50)), warning = function(w) {
        if (grepl("converg|infinite|overflow", conditionMessage(w), ignore.case = TRUE)) .ec_stop("Conditional logit could not establish a finite converged estimate: ", conditionMessage(w))
      })
    if (any(fit$iter >= 50)) .ec_stop("Conditional logit iteration limit reached; inspect separation or specification.")
    if (!identical(rownames(fit$model), rownames(d))) .ec_stop("Could not verify conditional-logit source rows.")
    V <- stats::vcov(fit); df <- Inf
    covnote <- "Exact conditional-likelihood covariance; asymptotic normal z, no finite t degrees of freedom"
    note <- "Log-odds coefficients conditional on individual outcome totals; exp(coefficient) is a conditional odds ratio. Individuals without outcome changes are excluded. Individual intercepts and unconditional probabilities are not estimated. Exact likelihood only; classical covariance assumes the conditional model is correctly specified."
    scale <- "log odds"
  } else if (nm == "panel_poisson") {
    pf <- stats::as.formula(paste(paste(deparse(f), collapse = " "), "| .ec_panel_id"), env = environment(formula))
    fit <- .ec_panel_poisson_engine(pf, d)
    if (!isTRUE(fit$convStatus)) .ec_stop("Poisson did not converge; inspect separation, scaling or model specification.")
    used <- fixest::obs(fit)
    if (!identical(as.integer(used), seq_len(nrow(d)))) .ec_stop("Poisson changed the sample unexpectedly; inspect separation and fixed-effect removals.")
    ss <- fixest::ssc(K.adj = TRUE, K.fixef = "nonnested", G.adj = TRUE, G.df = "min", t.df = "min")
    V <- if (inference == "classical") stats::vcov(fit, vcov = "iid", ssc = ss) else stats::vcov(fit, vcov = ~ .ec_panel_id, ssc = ss)
    df <- if (inference == "classical") Inf else g - 1L
    covnote <- if (inference == "classical") "Poisson model-based covariance; asymptotic normal z" else "fixest individual cluster sandwich; K.adj=TRUE, K.fixef=nonnested, G.adj=TRUE; t(G-1)"
    note <- "Log conditional-mean coefficients; exp(coefficient) is a multiplicative mean ratio. All-zero individuals and singletons are excluded. Classical Poisson inference assumes its variance specification; cluster covariance still assumes independent individuals. No exposure offset or automatic dispersion correction is supplied."
    scale <- "log conditional mean"
  } else {
    if (any(!iv$endogenous %in% slopes)) .ec_stop("An endogenous regressor is invariant within individuals and cannot be instrumented in FE-IV.")
    ex <- intersect(iv$exogenous, slopes); zvars <- c(ex, iv$instruments)
    Z <- as.matrix(d[zvars]); storage.mode(Z) <- "double"
    Z <- apply(Z, 2L, function(v) v - stats::ave(v, d$.ec_panel_id, FUN = mean))
    if (qr(Z)$rank < ncol(Z)) .ec_stop("Excluded instruments or exogenous controls are invariant/collinear after individual demeaning.")
    X <- centered[, varying, drop = FALSE]
    if (qr(crossprod(Z, X))$rank < ncol(X)) .ec_stop("FE-IV rank condition fails: the instruments do not identify all structural slopes.")
    if (nrow(d) - g - ncol(Z) <= 0) .ec_stop("Insufficient residual degrees of freedom for the FE-IV first stages.")
    ivf <- stats::as.formula(paste(paste(deparse(f), collapse = " "), "|", paste(vapply(zvars, .ec_quote_name, character(1)), collapse = " + ")), env = environment(formula))
    fit <- plm::plm(ivf, data = d, index = c(".ec_panel_id", ".ec_panel_time"), model = "within", effect = "individual", na.action = stats::na.fail)
    fi <- plm::index(fit)
    if (!identical(as.character(fi[[1L]]), as.character(d$.ec_panel_id)) || !identical(as.numeric(as.character(fi[[2L]])), as.numeric(d$.ec_panel_time))) .ec_stop("Could not verify FE-IV source rows.")
    V <- if (inference == "classical") stats::vcov(fit) else plm::vcovHC(fit, method = "arellano", type = "HC1", cluster = "group")
    df <- if (inference == "classical") stats::df.residual(fit) else g - 1L
    for (en in iv$endogenous) {
      fs <- plm::plm(.ec_formula(en, zvars), data = d, index = c(".ec_panel_id", ".ec_panel_time"), model = "within", effect = "individual", na.action = stats::na.fail)
      first_stages[[en]] <- fs
      VV <- if (inference == "classical") stats::vcov(fs) else plm::vcovHC(fs, method = "arellano", type = "HC1", cluster = "group")
      # Numeric formula terms may be backtick-quoted by model.matrix.
      zz <- colnames(stats::model.matrix(.ec_formula(en, zvars), d))[-1L]
      restricted <- zz[match(iv$instruments, zvars)]
      b1 <- stats::coef(fs)[restricted]; V1 <- VV[restricted, restricted, drop = FALSE]
      denom <- if (inference == "classical") stats::df.residual(fs) else g - 1L
      good <- all(is.finite(b1)) && all(is.finite(V1)) && !inherits(try(chol(V1), silent = TRUE), "try-error")
      W <- if (good) as.numeric(crossprod(b1, solve(V1, b1))) / length(b1) else NA_real_
      first_stage_tests <- rbind(first_stage_tests, data.frame(endogenous = en,
        test = "Excluded-instrument joint first-stage Wald F", statistic = W,
        p.value = if (good) stats::pf(W, length(b1), denom, lower.tail = FALSE) else NA_real_,
        df1 = length(b1), df2 = denom, inference = inference,
        status = if (good) "computed" else "unavailable",
        note = "Relevance diagnostic only. Not a general weak-instrument-robust test or a test of instrument validity; with several endogenous variables, marginal first stages do not establish joint strength."))
    }
    covnote <- if (inference == "classical") "plm within-2SLS covariance; residual t degrees of freedom" else "plm within-2SLS Arellano HC1, individual clusters; t(G-1)"
    note <- "Individual fixed-effects 2SLS with explicitly selected endogenous regressors and excluded instruments. Instrument exogeneity and exclusion are researcher assumptions. First-stage relevance does not establish validity or protect against weak instruments."
    scale <- "response units"
  }
  b <- stats::coef(fit)
  expected <- status$term[status$status == "estimated"]
  raw_names <- names(b)
  # Engine conventions differ for backtick-quoted numeric names. Resolve each
  # coefficient to exactly one original design term; never rely on column order.
  public <- vapply(raw_names, function(term) {
    hit <- which(vapply(seq_along(slopes), function(j) {
      alternatives <- c(expected[j], slopes[j], .ec_quote_name(slopes[j]))
      if (is.logical(d[[slopes[j]]])) alternatives <- c(expected[j], paste0(slopes[j], "TRUE"), paste0(.ec_quote_name(slopes[j]), "TRUE"))
      term %in% alternatives
    }, logical(1)))
    if (length(hit) == 1L) expected[hit] else NA_character_
  }, character(1))
  if (anyNA(public) || anyDuplicated(public)) .ec_stop("Could not map fitted terms to the original design unambiguously.")
  V <- V[raw_names, raw_names, drop = FALSE]
  names(b) <- public; dimnames(V) <- list(public, public)
  if (!setequal(names(b), expected) || any(!is.finite(b))) .ec_stop("Some structural coefficients are unidentified or were removed by the engine; inspect collinearity or separation.")
  if (is.finite(df) && df <= 0) .ec_stop("Insufficient inference degrees of freedom.")
  V <- V[names(b), names(b), drop = FALSE]
  if (any(!is.finite(V)) || any(diag(V) <= 0)) .ec_stop("Non-finite or non-positive coefficient variance; inference is unavailable.")
  se <- sqrt(diag(V)); stat <- b/se
  p <- if (is.infinite(df)) 2 * stats::pnorm(abs(stat), lower.tail = FALSE) else 2 * stats::pt(abs(stat), df, lower.tail = FALSE)
  attr(fit, "econcompare_panel_table") <- cbind(Estimate = b, `Std. Error` = se, statistic = stat, p.value = p)
  attr(fit, "econcompare_panel_df") <- df
  attr(fit, "econcompare_panel_status") <- status
  attr(fit, "econcompare_panel_no_r2") <- TRUE
  attr(fit, "econcompare_panel_nobs") <- length(pos)
  list(fit = fit, V = V, status = status, pos = pos, df = df, groups = g,
    unit = "individual-period", note = note, covariance_note = covnote,
    exclusion_reason = reason, scale = scale,
    first_stages = first_stages, first_stage_tests = first_stage_tests,
    mapping = data.frame(row = original[pos], id = d$.ec_panel_id, period = d$.ec_panel_time))
}

.ec_panel_diag_stop <- function(reason_code, message) {
  stop(structure(list(message = message, call = NULL, reason_code = reason_code),
    class = c("econcompare_panel_diagnostic_unavailable", "error", "condition")))
}

# Kept separate so convergence guards can be exercised with a limited real optimizer.
.ec_panel_poisson_engine <- function(formula, data) {
  fixest::fepois(formula, data = data, fixef.rm = "none", glm.iter = 100,
    glm.tol = 1e-8, fixef.tol = 1e-8, notes = FALSE, warn = TRUE, data.save = TRUE)
}
