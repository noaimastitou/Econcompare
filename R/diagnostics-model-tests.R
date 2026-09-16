.ec_lm_parts <- function(model) {
  if (!inherits(model, "lm")) return(NULL)
  X <- tryCatch(stats::model.matrix(model), error = function(e) NULL)
  e <- tryCatch(stats::residuals(model), error = function(e) NULL)
  fv <- tryCatch(stats::fitted(model), error = function(e) NULL)
  if (is.null(X) || is.null(e) || nrow(X) != length(e)) return(NULL)
  list(X = X, e = as.numeric(e), fitted = as.numeric(fv), n = length(e))
}

.ec_aux_lm_r2 <- function(y, Z) {
  ok <- is.finite(y) & apply(Z, 1L, function(r) all(is.finite(r)))
  y <- y[ok]; Z <- Z[ok, , drop = FALSE]
  if (length(y) <= ncol(Z) + 1L || stats::var(y) <= .Machine$double.eps) return(NULL)
  fit <- stats::lm.fit(cbind(`(Intercept)` = 1, Z), y)
  rss <- sum(fit$residuals^2)
  tss <- sum((y - mean(y))^2)
  if (!is.finite(tss) || tss <= 0) return(NULL)
  list(r2 = max(0, min(1, 1 - rss/tss)), rank = fit$rank, n = length(y))
}

.ec_diag_bp <- function(model, name, engine, alpha) {
  p <- .ec_lm_parts(model)
  if (is.null(p)) return(.ec_diag_unavailable(name, engine, "breusch_pagan", "Koenker–Breusch–Pagan", "Error variance", "The studentized Koenker–Breusch–Pagan test is currently implemented for the OLS engine."))
  Z <- p$X[, colnames(p$X) != "(Intercept)", drop = FALSE]
  if (!ncol(Z)) return(.ec_diag_unavailable(name, engine, "breusch_pagan", "Koenker–Breusch–Pagan", "Error variance", "No slope regressors are available for the auxiliary variance regression."))
  aux <- .ec_aux_lm_r2(p$e^2, Z)
  if (is.null(aux)) return(.ec_diag_unavailable(name, engine, "breusch_pagan", "Koenker–Breusch–Pagan", "Error variance", "The auxiliary regression is rank-deficient or too small."))
  df <- max(1, aux$rank - 1L); stat <- aux$n * aux$r2; pv <- stats::pchisq(stat, df, lower.tail = FALSE)
  .ec_diag_row(name, engine, "breusch_pagan", "Koenker–Breusch–Pagan", "Error variance", stat, df, pv,
               if (pv < alpha) "evidence_against_h0" else "no_evidence_against_h0",
               if (pv < alpha) "Evidence against the null of homoskedastic errors." else "The test does not reject homoskedastic errors.",
               "Studentized LM form based on n × R² from the auxiliary regression of squared OLS residuals on the original regressors (Koenker form).")
}

.ec_white_terms <- function(X) {
  X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  if (!ncol(X)) return(X)
  out <- X
  for (j in seq_len(ncol(X))) out <- cbind(out, X[, j]^2)
  if (ncol(X) > 1L) {
    cmb <- utils::combn(seq_len(ncol(X)), 2L)
    for (k in seq_len(ncol(cmb))) out <- cbind(out, X[, cmb[1L, k]] * X[, cmb[2L, k]])
  }
  out <- as.matrix(out)
  keep <- apply(out, 2L, function(z) is.finite(stats::var(z)) && stats::var(z) > .Machine$double.eps)
  out <- out[, keep, drop = FALSE]
  if (ncol(out)) out <- out[, !duplicated(as.data.frame(t(out))), drop = FALSE]
  out
}

.ec_diag_white <- function(model, name, engine, alpha) {
  p <- .ec_lm_parts(model)
  if (is.null(p)) return(.ec_diag_unavailable(name, engine, "white", "White", "Error variance", "White's test is currently implemented for the OLS engine."))
  q <- sum(colnames(p$X) != "(Intercept)")
  expected_aux <- as.integer(q * (q + 3L) / 2L)
  if (expected_aux >= p$n - 2L) {
    return(.ec_diag_unavailable(name, engine, "white", "White", "Error variance",
      sprintf("White test unavailable: the unrestricted quadratic auxiliary specification would contain about %d terms for %d observations, which is too large relative to the sample.", expected_aux, p$n)))
  }
  Z <- .ec_white_terms(p$X)
  if (!ncol(Z)) return(.ec_diag_unavailable(name, engine, "white", "White", "Error variance", "The quadratic auxiliary design could not be constructed."))
  aux <- .ec_aux_lm_r2(p$e^2, Z)
  if (is.null(aux)) return(.ec_diag_unavailable(name, engine, "white", "White", "Error variance", "The White auxiliary regression is rank-deficient or too large for the sample."))
  df <- max(1, aux$rank - 1L); stat <- aux$n * aux$r2; pv <- stats::pchisq(stat, df, lower.tail = FALSE)
  .ec_diag_row(name, engine, "white", "White", "Error variance", stat, df, pv,
               if (pv < alpha) "evidence_against_h0" else "no_evidence_against_h0",
               if (pv < alpha) "Evidence against homoskedasticity under a general quadratic variance specification." else "The test does not reject homoskedasticity under the fitted quadratic variance specification.",
               "Auxiliary regression includes original regressors, squares and pairwise cross-products; rank determines the reported degrees of freedom.")
}

.ec_diag_reset <- function(model, name, engine, alpha) {
  p <- .ec_lm_parts(model)
  if (is.null(p)) return(.ec_diag_unavailable(name, engine, "reset", "Ramsey RESET", "Specification", "RESET is currently implemented for the OLS engine."))
  X <- p$X; y <- stats::model.response(stats::model.frame(model))
  add <- cbind(fitted2 = p$fitted^2, fitted3 = p$fitted^3)
  ur <- stats::lm.fit(cbind(X, add), y)
  rr <- sum(p$e^2); ru <- sum(ur$residuals^2)
  q <- ur$rank - qr(X)$rank; df2 <- length(y) - ur$rank
  if (q <= 0 || df2 <= 0 || !is.finite(ru) || ru <= 0) return(.ec_diag_unavailable(name, engine, "reset", "Ramsey RESET", "Specification", "The augmented RESET regression is not estimable."))
  f <- max(0, ((rr - ru) / q) / (ru / df2)); pv <- stats::pf(f, q, df2, lower.tail = FALSE)
  .ec_diag_row(name, engine, "reset", "Ramsey RESET", "Specification", f, paste0(q, ", ", df2), pv,
               if (pv < alpha) "evidence_against_h0" else "no_evidence_against_h0",
               if (pv < alpha) "Evidence of functional-form misspecification or neglected nonlinear structure." else "RESET does not detect neglected fitted-value nonlinearities at the selected alpha.",
               "Augments the OLS equation with squared and cubed fitted values.")
}

.ec_vif_table <- function(model) {
  if (!inherits(model, "lm")) return(NULL)
  X0 <- tryCatch(stats::model.matrix(model), error = function(e) NULL)
  V0 <- tryCatch(stats::vcov(model), error = function(e) NULL)
  if (is.null(X0) || is.null(V0)) return(NULL)
  keep <- colnames(X0) != "(Intercept)"
  if (!any(keep)) return(NULL)
  assign <- attr(X0, "assign")[keep]
  term_labels <- attr(stats::terms(model), "term.labels")
  coef_names <- colnames(X0)[keep]
  V <- as.matrix(V0)[coef_names, coef_names, drop = FALSE]
  R <- suppressWarnings(stats::cov2cor(V))
  if (any(!is.finite(R)) || qr(R)$rank < ncol(R)) return(NULL)
  ids <- sort(unique(assign[assign > 0]))
  vals <- lapply(ids, function(id) {
    idx <- which(assign == id); other <- setdiff(seq_len(ncol(R)), idx); df <- length(idx)
    det_all <- det(R)
    det_a <- det(R[idx, idx, drop = FALSE])
    det_b <- if (length(other)) det(R[other, other, drop = FALSE]) else 1
    raw <- det_a * det_b / det_all
    adj <- if (df == 1L) raw else raw^(1/(2*df))
    data.frame(term = if (id <= length(term_labels)) term_labels[id] else paste0("term", id),
               df=df, gvif=raw, adjusted=adj, stringsAsFactors=FALSE)
  })
  do.call(rbind, vals)
}

.ec_diag_vif <- function(model, name, engine) {
  tab <- .ec_vif_table(model)
  if (is.null(tab) || !nrow(tab)) {
    return(.ec_diag_unavailable(name, engine, "vif", "VIF / GVIF", "Multicollinearity",
      "VIF/GVIF is unavailable because the OLS design matrix is empty, singular, or otherwise non-estimable."))
  }
  mx <- max(tab$adjusted, na.rm = TRUE)
  details <- paste(sprintf("%s: df=%d, %s=%.4g, adjusted=%.4g", tab$term, tab$df, ifelse(tab$df==1,"VIF","GVIF"), tab$gvif, tab$adjusted), collapse = "; ")
  .ec_diag_row(name, engine, "vif", "VIF / GVIF", "Multicollinearity", mx,
               result = "informational",
               interpretation = sprintf("Maximum adjusted VIF/GVIF measure: %s.", format(mx, digits = 4)),
               details = paste0(details, ". For multi-parameter terms, adjusted GVIF = GVIF^(1/(2*df))."), kind = "screening")
}

.ec_diag_influence <- function(model, name, engine) {
  if (!inherits(model, "lm")) return(.ec_diag_unavailable(name, engine, "influence", "Influence (Cook/leverage)", "Influence", "Cook distance and leverage are currently implemented for the OLS engine."))
  cd <- tryCatch(stats::cooks.distance(model), error = function(e) numeric())
  hv <- tryCatch(stats::hatvalues(model), error = function(e) numeric())
  if (!length(cd) || !length(hv)) return(.ec_diag_unavailable(name, engine, "influence", "Influence (Cook/leverage)", "Influence", "Influence measures are unavailable."))
  n <- length(cd); p <- model$rank
  c_thr <- 4/n; h_thr <- 2*p/n
  nc <- sum(cd > c_thr, na.rm = TRUE); nh <- sum(hv > h_thr, na.rm = TRUE)
  .ec_diag_row(name, engine, "influence", "Influence (Cook/leverage)", "Influence",
               statistic = max(cd, na.rm = TRUE), result = "informational",
               interpretation = sprintf("%d observation(s) exceed Cook > 4/n; %d exceed leverage > 2p/n.", nc, nh),
               details = sprintf("Cook threshold=%.4g; leverage threshold=%.4g; max leverage=%.4g. Thresholds are screening rules, not hypothesis tests.", c_thr, h_thr, max(hv, na.rm = TRUE)),
               kind = "screening")
}

.ec_diag_jb <- function(model, name, engine, alpha) {
  p <- .ec_lm_parts(model)
  if (is.null(p)) return(.ec_diag_unavailable(name, engine, "jarque_bera", "Jarque-Bera", "Residual distribution", "Jarque-Bera is currently reported for OLS residuals."))
  e <- p$e[is.finite(p$e)]; n <- length(e)
  if (n < 8L || stats::sd(e) <= 0) return(.ec_diag_unavailable(name, engine, "jarque_bera", "Jarque-Bera", "Residual distribution", "Too few non-degenerate residuals for a useful Jarque-Bera diagnostic."))
  z <- (e - mean(e))/sqrt(mean((e-mean(e))^2))
  skew <- mean(z^3); kurt <- mean(z^4)
  jb <- n/6 * (skew^2 + (kurt - 3)^2/4); pv <- stats::pchisq(jb, 2, lower.tail = FALSE)
  .ec_diag_row(name, engine, "jarque_bera", "Jarque-Bera", "Residual distribution", jb, 2, pv,
               if (pv < alpha) "evidence_against_h0" else "no_evidence_against_h0",
               if (pv < alpha) "Residual skewness/kurtosis depart from the Gaussian benchmark." else "The test does not reject the Gaussian skewness/kurtosis benchmark.",
               "Uses the standard asymptotic Jarque-Bera chi-squared(2) reference without a small-sample correction. Normal residuals are not required for OLS unbiasedness or consistency; this diagnostic is most relevant to exact small-sample inference assumptions.")
}

.ec_iv_diagnostics_table <- function(model) {
  sm <- tryCatch(summary(model, diagnostics = TRUE), error = function(e) NULL)
  dg <- if (!is.null(sm)) sm$diagnostics else NULL
  if (is.null(dg) || !length(dg)) return(NULL)

  dg <- as.data.frame(dg, stringsAsFactors = FALSE)
  rn <- rownames(dg)
  if (is.null(rn) || !nrow(dg)) return(NULL)

  # ivreg documents a diagnostics table with df1, df2, statistic and p-value.
  # Match this documented schema first, with narrowly defined aliases only.
  nm <- names(dg)
  norm <- function(x) gsub("[^a-z0-9]", "", tolower(x))
  nn <- norm(nm)
  pick <- function(aliases) {
    hit <- which(nn %in% aliases)
    if (length(hit)) hit[1L] else NA_integer_
  }
  i_df1 <- pick(c("df1"))
  i_df2 <- pick(c("df2"))
  i_stat <- pick(c("statistic", "stat"))
  i_p <- pick(c("pvalue", "pval"))
  if (is.na(i_stat) || is.na(i_p)) return(NULL)

  canonical_id <- function(label) {
    z <- tolower(trimws(label))
    if (startsWith(z, "weak instruments")) return("iv_weak_instruments")
    if (z %in% c("wu-hausman", "wu hausman", "wu–hausman")) return("iv_wu_hausman")
    if (z == "sargan") return("iv_overidentification")
    NA_character_
  }

  out <- lapply(seq_len(nrow(dg)), function(i) {
    id <- canonical_id(rn[i])
    if (is.na(id)) return(NULL)
    data.frame(
      id = id,
      label = rn[i],
      df1 = if (!is.na(i_df1)) suppressWarnings(as.numeric(dg[[i_df1]][i])) else NA_real_,
      df2 = if (!is.na(i_df2)) suppressWarnings(as.numeric(dg[[i_df2]][i])) else NA_real_,
      statistic = suppressWarnings(as.numeric(dg[[i_stat]][i])),
      p.value = suppressWarnings(as.numeric(dg[[i_p]][i])),
      stringsAsFactors = FALSE
    )
  })
  out <- Filter(Negate(is.null), out)
  if (!length(out)) return(NULL)
  do.call(rbind, out)
}

.ec_diag_iv <- function(model, name, engine, alpha) {
  dg <- .ec_iv_diagnostics_table(model)
  if (is.null(dg) || !nrow(dg)) {
    return(.ec_diag_unavailable(
      name, engine, "iv_diagnostics", "IV diagnostics", "Endogeneity / instruments",
      "ivreg did not return the documented diagnostics schema (df1, df2, statistic, p-value) for a recognized IV diagnostic."
    ))
  }

  rows <- lapply(seq_len(nrow(dg)), function(i) {
    id <- dg$id[i]
    pv <- dg$p.value[i]
    dft <- paste(
      c(
        if (is.finite(dg$df1[i])) paste0("df1=", dg$df1[i]) else NULL,
        if (is.finite(dg$df2[i])) paste0("df2=", dg$df2[i]) else NULL
      ),
      collapse = ", "
    )
    if (!nzchar(dft)) dft <- NA_character_

    interp <- switch(
      id,
      iv_weak_instruments = "Weak-instrument diagnostic: strength is associated with a large first-stage test statistic; the p-value tests relevance restrictions but is not by itself a universal weak-instrument threshold.",
      iv_wu_hausman = "Wu-Hausman diagnostic: a small p-value provides evidence that OLS and IV differ systematically under the test assumptions.",
      iv_overidentification = "Sargan overidentification diagnostic: a small p-value is evidence against the joint overidentifying restrictions; the test is unavailable in exactly identified specifications.",
      "Estimator-specific IV diagnostic returned by ivreg."
    )

    .ec_diag_row(
      name, engine, id, paste0("IV: ", dg$label[i]), "Endogeneity / instruments",
      dg$statistic[i], dft, pv,
      if (is.finite(pv) && pv < alpha) "evidence_against_h0" else if (is.finite(pv)) "no_evidence_against_h0" else "informational",
      interp,
      "Parsed from the documented ivreg summary diagnostics table using its df1/df2/statistic/p-value schema; each diagnostic has a distinct null hypothesis.",
      inference_basis = "ivreg documented diagnostic table"
    )
  })
  do.call(rbind, rows)
}



