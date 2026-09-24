# Explicit source-row maps for transformations with different observational units.
.ec_panel_extension_fit <- function(nm, dat, formula, full_mm, original, calendar, inference) {
  vars <- all.vars(formula)
  if (any(vapply(dat[vars], is.factor, logical(1)))) {
    .ec_stop("First differences, between and Mundlak currently require numeric/logical regressors. Explicitly encode substantive contrasts before fitting; automatic factor averaging is not supported.")
  }
  ids <- dat$.ec_panel_id
  groups <- split(seq_len(nrow(dat)), ids)
  means <- function(v) vapply(groups, function(i) mean(as.numeric(v[i])), numeric(1))
  mm <- full_mm
  mean_terms <- data.frame(term = character(), variable = character(), slope_term = character())
  status <- data.frame(term = colnames(mm), status = "estimated", stringsAsFactors = FALSE)
  pos <- seq_len(nrow(dat))
  if (nm == "panel_fd") {
    if (!isTRUE(calendar)) .ec_stop("First differences require a verified calendar. Use explicit integer, year-month or year-quarter period labels.")
    end <- which(c(FALSE, ids[-1L] == ids[-length(ids)] & diff(dat$.ec_panel_time) == 1))
    start <- end - 1L
    if (length(end) < 3L) .ec_stop("Too few consecutive within-individual pairs for first differences; gaps are never compressed.")
    X <- mm[end, , drop = FALSE] - mm[start, , drop = FALSE]
    y <- as.numeric(dat[[vars[1L]]][end] - dat[[vars[1L]]][start])
    zero <- vapply(seq_len(ncol(X)), function(j) max(abs(X[, j])) <= 1e-12 * max(1, max(abs(mm[, j]))), logical(1))
    status$status[zero] <- "absorbed by first differences"
    X <- X[, !zero, drop = FALSE]
    if (!ncol(X)) .ec_stop("No varying regressors remain after first differences.")
    cluster <- ids[end]
    pos <- sort(unique(c(start, end)))
    mapping <- data.frame(observation = seq_along(end), id = cluster,
      start_row = original[start], end_row = original[end],
      start_period = dat$.ec_panel_time[start], end_period = dat$.ec_panel_time[end])
    unit <- "consecutive within-individual change"
    note <- "No drift/intercept is added to the differenced equation. Its R-squared is uncentered. Classical inference concerns differenced errors; independent level errors generally become serially correlated after differencing."
  } else if (nm == "panel_between") {
    X <- vapply(seq_len(ncol(mm)), function(j) means(mm[, j]), numeric(length(groups)))
    colnames(X) <- colnames(mm)
    y <- means(dat[[vars[1L]]])
    cluster <- names(groups)
    mapping <- data.frame(row = original, id = ids, observation = match(ids, names(groups)),
      periods_in_mean = as.integer(table(ids)[ids]))
    unit <- "individual temporal mean"
    note <- "One equally weighted mean per individual, including singletons. Means use the common complete-case sample; unbalanced panels may average different periods. Cluster inference here is heteroskedasticity-robust inference across independent individual means."
  } else {
    # Build means of the numeric design columns, retaining exact term mappings.
    varying <- vapply(seq_len(ncol(mm)), function(j) {
      centered <- mm[, j] - stats::ave(mm[, j], ids, FUN = mean)
      max(abs(centered)) > 1e-12 * max(1, max(abs(mm[, j])))
    }, logical(1))
    if (!any(varying)) .ec_stop("Mundlak requires at least one regressor with within-individual variation.")
    cols <- colnames(mm)[varying]
    new_names <- paste0(".ec_mundlak_mean_", seq_along(cols))
    if (any(new_names %in% names(dat))) .ec_stop("Reserved Mundlak mean column names are present; rename them.")
    for (j in seq_along(cols)) dat[[new_names[j]]] <- stats::ave(mm[, cols[j]], ids, FUN = mean)
    mean_terms <- data.frame(term = new_names,
      variable = vars[-1L][match(attr(full_mm, "assign")[varying], seq_along(vars[-1L]))],
      slope_term = cols, stringsAsFactors = FALSE)
    expanded <- stats::reformulate(c(attr(stats::terms(formula), "term.labels"), new_names),
      response = .ec_quote_name(vars[1L]), env = environment(formula))
    fit <- plm::plm(expanded, dat, index = c(".ec_panel_id", ".ec_panel_time"),
      model = "random", effect = "individual", random.method = "swar", na.action = stats::na.fail)
    fi <- plm::index(fit)
    key <- paste(match(as.character(fi[[1L]]), unique(ids)), as.numeric(as.character(fi[[2L]])), sep = ":")
    pos <- match(key, paste(match(ids, unique(ids)), dat$.ec_panel_time, sep = ":"))
    if (anyNA(pos) || anyDuplicated(pos) || length(pos) != nrow(dat)) .ec_stop("Could not verify the Mundlak estimation sample.")
    status <- rbind(status, data.frame(term = new_names, status = "estimated"))
    if (!setequal(status$term, names(stats::coef(fit)))) .ec_stop("Unidentified Mundlak regressors or individual means. Remove redundant terms explicitly.")
    cluster <- ids[pos]
    y <- plm::pmodel.response(fit)
    mapping <- data.frame(row = original[pos], id = ids[pos],
      periods_in_mean = as.integer(table(ids)[ids[pos]]))
    unit <- "individual-period"
    note <- "Mundlak adds individual means of time-varying regressors to random effects. Original slopes are within-type conditional associations; mean coefficients are between-minus-within contrasts. This does not remove time-varying endogeneity or informative missingness."
  }
  if (nm != "panel_mundlak") {
    # A private formula with original matrix term names gives a conventional lm
    # object while avoiding evaluation of user column names as expressions.
    frame <- as.data.frame(X, check.names = FALSE)
    private_y <- ".ec_transformed_response"
    if (private_y %in% names(frame)) .ec_stop("Reserved transformed-response column name is present; rename it.")
    frame[[private_y]] <- as.numeric(y)
    slopes <- setdiff(colnames(X), "(Intercept)")
    tf <- stats::reformulate(vapply(slopes, .ec_quote_name, character(1)), response = private_y,
      intercept = nm == "panel_between")
    fit <- stats::lm(tf, data = frame, na.action = stats::na.fail)
    # Quoting an already-quoted design name can alter coefficient labels. Map
    # the fitted design back in its verified column order for public extraction.
    expected <- if (nm == "panel_between") c("(Intercept)", slopes) else slopes
    if (length(stats::coef(fit)) != length(expected)) .ec_stop("Unexpected transformed design dimensions.")
    attr(fit, "econcompare_term_names") <- expected
  }
  b <- stats::coef(fit)
  if (!length(b) || any(!is.finite(b)) || stats::df.residual(fit) <= 0) .ec_stop("Unidentified regressors or insufficient residual degrees of freedom after transformation.")
  scale <- max(1, max(abs(y)))
  if (max(abs(y - mean(y))) <= 1e-12 * scale) .ec_stop("No usable response variation after the model transformation.")
  if (max(abs(stats::residuals(fit))) <= 1e-12 * scale) .ec_stop("Essentially perfect fit: coefficient inference is unreliable.")
  ng <- length(unique(cluster))
  if (ng < 2L) .ec_stop("At least two contributing individuals are required.")
  if (inference == "classical") {
    V <- stats::vcov(fit)
    covariance_note <- "Model-based covariance; residual t degrees of freedom"
  } else if (nm == "panel_mundlak") {
    V <- plm::vcovHC(fit, method = "arellano", type = "HC1", cluster = "group")
    covariance_note <- "plm Arellano HC1, individual clusters; t(G-1)"
  } else {
    .ec_require("sandwich", "robust first-difference/between inference")
    V <- if (nm == "panel_fd") sandwich::vcovCL(fit, cluster = cluster, type = "HC1", cadjust = TRUE) else sandwich::vcovHC(fit, type = "HC1")
    covariance_note <- if (nm == "panel_fd") "sandwich HC1, individual clusters, G/(G-1) adjustment; t(G-1)" else "sandwich HC1 across individual means; t(G-1)"
  }
  V <- V[names(b), names(b), drop = FALSE]
  if (any(!is.finite(V)) || any(diag(V) <= 0)) .ec_stop("Coefficient inference is unavailable: non-finite or non-positive variances.")
  public_names <- attr(fit, "econcompare_term_names")
  if (!is.null(public_names)) { names(b) <- public_names; dimnames(V) <- list(public_names, public_names) }
  df <- if (inference == "classical") stats::df.residual(fit) else ng - 1L
  se <- sqrt(diag(V)); stat <- b/se
  co <- cbind(Estimate = b, `Std. Error` = se, `t value` = stat, `Pr(>|t|)` = 2 * stats::pt(abs(stat), df, lower.tail = FALSE))
  attr(fit, "econcompare_panel_table") <- co
  attr(fit, "econcompare_panel_df") <- df
  attr(fit, "econcompare_panel_status") <- status
  components <- data.frame()
  if (nm == "panel_mundlak") {
    components <- do.call(rbind, lapply(seq_len(nrow(mean_terms)), function(j) {
      slope <- mean_terms$slope_term[j]; mu <- mean_terms$term[j]
      C <- matrix(0, 3L, length(b), dimnames = list(NULL, names(b)))
      C[1L, slope] <- 1; C[2L, mu] <- 1; C[3L, c(slope, mu)] <- 1
      value <- as.numeric(C %*% b); variance <- diag(C %*% V %*% t(C))
      if (any(!is.finite(variance)) || any(variance < 0)) .ec_stop("Invalid covariance for Mundlak component contrasts.")
      error <- sqrt(variance)
      data.frame(variable = mean_terms$variable[j],
        component = c("within-type conditional slope", "between-minus-within contrast", "between-type slope (sum)"),
        estimate = value, std.error = error,
        p.value = ifelse(error > 0, 2 * stats::pt(abs(value/error), df, lower.tail = FALSE), NA_real_),
        inference_df = df)
    }))
  }
  list(fit = fit, V = V, status = status, pos = pos, df = df, groups = ng,
    unit = unit, note = note, covariance_note = covariance_note, mapping = mapping,
    mean_terms = mean_terms, components = components,
    exclusion_reason = "not part of any usable consecutive within-individual pair")
}

# Re-auditing a reduced sample must not infer a different calendar cadence.
.ec_panel_audit_on_grid <- function(dat, id, time, source_audit) {
  a <- eco_panel_audit(dat, id, time)
  a$summary$frequency <- source_audit$summary$frequency
  a$summary$calendar_verified <- source_audit$summary$calendar_verified
  a$index$period <- dat$.ec_panel_time
  for (j in seq_len(nrow(a$by_individual))) {
    tt <- sort(unique(dat$.ec_panel_time[dat$.ec_panel_id == a$by_individual$id[j]]))
    a$by_individual$first_period[j] <- min(tt)
    a$by_individual$last_period[j] <- max(tt)
    a$by_individual$internal_missing_periods[j] <- if (isTRUE(source_audit$summary$calendar_verified)) sum(pmax(diff(tt) - 1, 0)) else NA_real_
  }
  a
}
