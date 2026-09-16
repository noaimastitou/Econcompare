.ec_parse_numeric_list <- function(x, default = NULL) {
  if (is.null(x) || !nzchar(trimws(x))) return(default)
  vals <- suppressWarnings(as.numeric(trimws(strsplit(x, ",", fixed = TRUE)[[1]])))
  vals <- vals[is.finite(vals)]
  if (!length(vals)) default else vals
}

.ec_parse_bound <- function(x, side = c("left", "right")) {
  side <- match.arg(side)
  if (is.null(x) || !nzchar(trimws(x))) return(if (side == "left") -Inf else Inf)
  z <- tolower(trimws(x))
  if (z %in% c("-inf", "-infinity")) return(-Inf)
  if (z %in% c("inf", "+inf", "infinity", "+infinity")) return(Inf)
  out <- suppressWarnings(as.numeric(z))
  if (is.na(out)) .ec_stop("Invalid ", side, " Tobit bound: `", x, "`.")
  out
}

.ec_formula <- function(y, x) {
  if (!length(x)) .ec_stop("Select at least one explanatory variable.")
  stats::reformulate(x, response = y)
}

.ec_help_icon <- function(text) {
  shiny::tags$span(
    class = "ec-q",
    `aria-label` = text,
    `data-tip` = text,
    tabindex = "0",
    "?"
  )
}

.ec_labeled <- function(label, tip = NULL, caption = NULL) {
  shiny::tagList(
    shiny::div(class = "ec-label-row",
      shiny::tags$span(class = "ec-label-text", label),
      if (!is.null(tip)) .ec_help_icon(tip)
    ),
    if (!is.null(caption)) shiny::div(class = "ec-field-caption", caption)
  )
}

.ec_choice_badges <- function(models) {
  if (!length(models)) {
    return(shiny::div(class = "ec-chip-wrap",
      shiny::tags$span(class = "ec-chip ec-chip-soft", "Choose at least one model")
    ))
  }
  shiny::div(class = "ec-chip-wrap",
    lapply(models, function(m) shiny::tags$span(class = "ec-chip ec-chip-soft", m))
  )
}

.ec_preview_table <- function(data, n = 8L) {
  z <- utils::head(data, n)
  num <- vapply(z, is.numeric, logical(1))
  z[num] <- lapply(z[num], .ec_fmt)
  .ec_html_table(z)
}


.ec_coef_inference_table <- function(fit, model_name, alpha = 0.05, include_ci = TRUE) {
  if (is.null(fit$models[[model_name]])) return(data.frame())
  engine <- .ec_model_engine(fit, model_name)
  z <- .ec_clean_coef(fit$models[[model_name]], model_name, engine = engine, purpose = "slope")
  keep <- intersect(c("term", "term_role", "estimate", "std.error", "statistic", "p.value"), names(z))
  z <- z[, keep, drop = FALSE]
  if (!nrow(z)) return(z)

  if (!isTRUE(include_ci) || !all(c("estimate", "std.error") %in% names(z))) return(z)

  spec <- .ec_inference_spec(fit$models[[model_name]], engine, alpha)
  native <- .ec_match_native_ci(spec$native, z$term)
  if (!is.null(native)) {
    z$conf.low <- native$conf.low
    z$conf.high <- native$conf.high
    z$inference <- spec$basis
    return(z)
  }
  if (!is.finite(spec$critical)) {
    z$conf.low <- NA_real_
    z$conf.high <- NA_real_
    z$inference <- spec$basis
    return(z)
  }
  z$conf.low <- z$estimate - spec$critical * z$std.error
  z$conf.high <- z$estimate + spec$critical * z$std.error
  z$inference <- spec$basis
  z
}

.ec_diag_choices_for_model <- function(fit, model_name) {
  if (is.null(fit$models[[model_name]])) return(data.frame())
  engine <- .ec_model_engine(fit, model_name)
  available <- .ec_diag_choice_table(engine)

  # The interactive workspace intentionally exposes a short, reliable checklist.
  # More technical diagnostics remain available through eco_diagnostics().
  wanted <- switch(engine,
    ols = c("coefficient_significance", "breusch_pagan", "reset", "vif", "influence"),
    wls = c("coefficient_significance"),
    ols_robust = c("coefficient_significance"),
    robust_m = character(),
    fixest = c("coefficient_significance"),
    ivreg = c("coefficient_significance", "iv_weak_instruments", "iv_wu_hausman", "iv_overidentification"),
    quantile = c("coefficient_significance"),
    tobit = c("coefficient_significance"),
    heckman = c("coefficient_significance"),
    lpm = c("coefficient_significance"),
    logit = c("coefficient_significance"),
    probit = c("coefficient_significance"),
    multinomial_logit = character(),
    ordered_logit = character(),
    ordered_probit = character(),
    character()
  )
  out <- available[match(wanted, available$id, nomatch = 0L), , drop = FALSE]
  if (!nrow(out)) return(out)

  out$simple_label <- vapply(out$id, function(id) switch(id,
    coefficient_significance = "Coefficient precision",
    breusch_pagan = "Error variance",
    reset = "Functional form",
    vif = "Collinearity",
    influence = "Influential observations",
    iv_weak_instruments = "Instrument strength",
    iv_wu_hausman = "Endogeneity check",
    iv_overidentification = "Overidentifying restrictions",
    out$diagnostic[out$id == id][1L]
  ), character(1))
  out$simple_question <- vapply(out$id, function(id) switch(id,
    coefficient_significance = "Are the main coefficients estimated precisely enough to be informative?",
    breusch_pagan = "Is there evidence that the error variance is not constant?",
    reset = "Is there evidence that the linear functional form may be too restrictive?",
    vif = "Are regressors strongly related enough to make coefficients imprecise?",
    influence = "Are a few observations unusually influential for the OLS fit?",
    iv_weak_instruments = "Do the instruments appear strong enough in the first stage?",
    iv_wu_hausman = "Is there evidence that OLS and IV differ because of endogeneity?",
    iv_overidentification = "When available, are the overidentifying restrictions rejected?",
    ""
  ), character(1))

  # Keep the button useful without pretending that every possible diagnostic is essential.
  out$recommended <- TRUE
  out
}

.ec_diag_choice_labels <- function(tab) {
  if (!nrow(tab)) return(character())
  label <- paste0(tab$simple_label, " — ", tab$simple_question)
  stats::setNames(tab$id, label)
}

.ec_diag_result_label <- function(x) {
  switch(as.character(x),
    evidence_against_h0 = "Evidence against H0",
    no_evidence_against_h0 = "No evidence against H0",
    unavailable = "Unavailable",
    informational = "Informational",
    as.character(x)
  )
}

.ec_diag_synthesis <- function(z, engine) {
  if (!nrow(z)) return("No compatible diagnostics are available for this model.")
  grab <- function(id) z[z$id == id, , drop = FALSE]
  phrases <- character()
  if (identical(engine, "ols")) {
    bp <- grab("breusch_pagan"); wh <- grab("white"); rs <- grab("reset")
    if (nrow(bp) && nrow(wh) && identical(bp$result[1], "no_evidence_against_h0") && identical(wh$result[1], "evidence_against_h0")) {
      phrases <- c(phrases, "Koenker–Breusch–Pagan does not reject homoskedasticity, while the more general White test does; the variance pattern may therefore be nonlinear or interaction-dependent.")
    } else if (nrow(bp) && identical(bp$result[1], "evidence_against_h0")) {
      phrases <- c(phrases, "Koenker–Breusch–Pagan provides evidence against homoskedasticity; heteroskedasticity-robust inference is worth considering.")
    } else if (nrow(wh) && identical(wh$result[1], "evidence_against_h0")) {
      phrases <- c(phrases, "White provides evidence against homoskedasticity under a general quadratic variance specification.")
    }
    if (nrow(rs) && identical(rs$result[1], "evidence_against_h0")) {
      phrases <- c(phrases, "RESET also indicates possible functional-form misspecification or neglected nonlinear structure, so changing the covariance estimator alone may not address the specification issue.")
    }
    vf <- grab("vif")
    if (nrow(vf) && is.finite(vf$statistic[1]) && vf$statistic[1] >= 5) {
      phrases <- c(phrases, sprintf("The maximum VIF is %.2f; treat this as a collinearity screening signal rather than a pass/fail test.", vf$statistic[1]))
    }
  }
  if (identical(engine, "robust_m")) {
    phrases <- c(phrases, "Coefficient inference for robust M-estimation is displayed as an asymptotic approximation and should not be read as classical OLS inference.")
  }
  if (identical(engine, "ivreg")) {
    phrases <- c(phrases, "IV diagnostics must be interpreted test by test: instrument strength, endogeneity and overidentifying restrictions answer different questions and do not jointly prove instrument validity.")
  }
  if (!length(phrases)) {
    phrases <- "Review the compatible diagnostics alongside the estimator assumptions; absence of rejection is not evidence that the specification is correct."
  }
  paste(phrases, collapse = " ")
}

.ec_diag_cards_ui <- function(z) {
  if (!nrow(z)) return(shiny::div(class = "ec-empty ec-empty-small", "No compatible diagnostics are available for this model."))
  cats <- unique(z$category)
  shiny::tagList(lapply(cats, function(cat) {
    zz <- z[z$category == cat, , drop = FALSE]
    shiny::div(class = "ec-diag-section",
      shiny::div(class = "ec-diag-section-title", cat),
      shiny::div(class = "ec-diag-grid",
        lapply(seq_len(nrow(zz)), function(i) {
          r <- zz[i, , drop = FALSE]
          tech <- character()
          if (is.finite(r$statistic)) tech <- c(tech, paste0("Statistic: ", .ec_fmt(r$statistic)))
          if (!is.na(r$df) && nzchar(r$df)) tech <- c(tech, paste0("df: ", r$df))
          if (is.finite(r$p.value)) tech <- c(tech, paste0("p-value: ", .ec_fmt(r$p.value)))
          if (nzchar(r$inference_basis)) tech <- c(tech, paste0("Inference: ", r$inference_basis))
          if (nzchar(r$details)) tech <- c(tech, r$details)
          shiny::div(class = "ec-diag-card",
            shiny::div(class = "ec-diag-card-head",
              shiny::div(
                shiny::div(class = "ec-diag-name", r$diagnostic),
                shiny::div(class = "ec-diag-kind", tools::toTitleCase(r$kind))
              ),
              shiny::tags$span(class = paste0("ec-result ec-result-", r$result), .ec_diag_result_label(r$result))
            ),
            if (is.finite(r$p.value)) shiny::div(class = "ec-simple-p", paste0("p = ", .ec_fmt(r$p.value))),
            shiny::p(class = "ec-diag-interpretation", r$interpretation),
            if (length(tech)) shiny::tags$details(class = "ec-more",
              shiny::tags$summary("Technical details"),
              shiny::tags$ul(lapply(tech, shiny::tags$li))
            )
          )
        })
      )
    )
  }))
}

.ec_app_css <- function() {
  paste0(
    ":root{",
      "--ec-bg:#f3f6fb;--ec-card:#ffffff;--ec-text:#132238;--ec-muted:#5f6f86;",
      "--ec-border:#dfe7f1;--ec-dark:#132238;--ec-soft:#eef4fb;--ec-accent:#1f6feb;",
      "--ec-accent-soft:#eaf2ff;--ec-success:#e8f7ee;--ec-shadow:0 10px 30px rgba(19,34,56,.08)",
    "}",
    "body{background:linear-gradient(180deg,#f7f9fd 0%,#f2f5fa 100%);color:var(--ec-text);font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif}",
    ".container-fluid{padding:0!important}",
    ".ec-shell{padding:22px;max-width:1680px;margin:auto}",
    ".ec-top{display:flex;justify-content:space-between;gap:18px;align-items:stretch;margin-bottom:18px}",
    ".ec-hero{flex:1;background:linear-gradient(135deg,#132238 0%,#1e3557 100%);color:#fff;border-radius:24px;padding:26px 28px;box-shadow:var(--ec-shadow)}",
    ".ec-title{font-size:34px;font-weight:820;letter-spacing:-.03em;line-height:1.05;margin:0}",
    ".ec-sub{opacity:.86;margin-top:8px;font-size:16px;max-width:760px}",
    ".ec-hero-pills{display:flex;flex-wrap:wrap;gap:8px;margin-top:18px}",
    ".ec-pill{display:inline-flex;align-items:center;gap:8px;padding:8px 12px;border-radius:999px;background:rgba(255,255,255,.12);font-size:12px;font-weight:700;backdrop-filter:blur(6px)}",
    ".ec-metric-stack{width:360px;display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:12px}",
    ".ec-metric{background:var(--ec-card);border:1px solid var(--ec-border);border-radius:20px;padding:18px 16px;box-shadow:var(--ec-shadow)}",
    ".ec-metric-k{font-size:24px;font-weight:820;letter-spacing:-.02em}",
    ".ec-metric-l{font-size:12px;color:var(--ec-muted);font-weight:700;text-transform:uppercase;letter-spacing:.04em;margin-top:4px}",
    ".ec-grid{display:grid;grid-template-columns:minmax(320px,390px) minmax(0,1fr);gap:18px;align-items:start}",
    ".ec-sidebar,.ec-main{background:var(--ec-card);border:1px solid var(--ec-border);border-radius:24px;box-shadow:var(--ec-shadow);min-width:0;max-width:100%}",
    ".ec-sidebar{padding:18px;position:sticky;top:12px}",
    ".ec-main{padding:18px;min-height:640px}",
    ".ec-section{padding:0 0 16px;margin-bottom:16px;border-bottom:1px solid var(--ec-border)}.ec-section:last-child{border-bottom:0;margin-bottom:0;padding-bottom:0}",
    ".ec-step{display:flex;align-items:center;gap:10px;margin-bottom:10px}",
    ".ec-step-n{display:inline-flex;align-items:center;justify-content:center;width:28px;height:28px;border-radius:50%;background:var(--ec-accent-soft);color:var(--ec-accent);font-weight:800;font-size:13px}",
    ".ec-h{font-weight:820;font-size:18px;letter-spacing:-.02em}",
    ".ec-help{font-size:13px;color:var(--ec-muted);margin:0 0 10px;line-height:1.5}",
    ".ec-mini-card{background:#f8fbff;border:1px solid var(--ec-border);border-radius:16px;padding:12px 14px;margin-top:12px}",
    ".ec-chip-wrap{display:flex;flex-wrap:wrap;gap:8px;margin-top:10px}",
    ".ec-chip{display:inline-flex;align-items:center;padding:6px 10px;border-radius:999px;background:var(--ec-dark);color:#fff;font-size:12px;font-weight:700}",
    ".ec-chip-soft{background:var(--ec-soft);color:var(--ec-text)}",
    ".ec-label-row{display:flex;align-items:center;gap:8px;margin-bottom:6px}",
    ".ec-label-text{font-weight:740;font-size:14px;color:var(--ec-text)}",
    ".ec-field-caption{font-size:12px;line-height:1.45;color:var(--ec-muted);margin:-2px 0 8px}",
    ".ec-q{display:inline-flex;align-items:center;justify-content:center;width:18px;height:18px;border-radius:50%;background:var(--ec-accent-soft);color:var(--ec-accent);font-size:11px;font-weight:900;cursor:help;position:relative;border:1px solid #cfe0ff}",
    ".ec-q:hover::after,.ec-q:focus::after{content:attr(data-tip);position:absolute;left:24px;top:-6px;min-width:260px;max-width:360px;background:#11243c;color:#fff;padding:10px 12px;border-radius:12px;font-size:12px;line-height:1.45;font-weight:500;box-shadow:0 12px 30px rgba(0,0,0,.18);z-index:30;white-space:normal}",
    ".ec-q:hover::before,.ec-q:focus::before{content:'';position:absolute;left:18px;top:5px;border-top:7px solid transparent;border-bottom:7px solid transparent;border-right:7px solid #11243c;z-index:31}",
    ".btn-primary{background:linear-gradient(135deg,#132238 0%,#27456f 100%)!important;border:0!important;border-radius:14px!important;font-weight:760!important;padding:11px 14px!important;box-shadow:0 10px 20px rgba(19,34,56,.14)!important}",
    ".btn-default{border-radius:14px!important;border:1px solid var(--ec-border)!important;background:#fff!important}",
    ".form-group,.selectize-control{max-width:100%;min-width:0}.form-control,.selectize-input{border-radius:14px!important;border:1px solid var(--ec-border)!important;box-shadow:none!important;padding-top:10px!important;padding-bottom:10px!important;max-width:100%;min-width:0;box-sizing:border-box}.selectize-input{overflow-wrap:anywhere}.selectize-input>div{display:inline-block;max-width:calc(100% - 8px);overflow:hidden;text-overflow:ellipsis;white-space:nowrap;vertical-align:top}.selectize-input>input{max-width:100%!important}",
    ".selectize-dropdown,.dropdown-menu{border-radius:14px!important;border-color:var(--ec-border)!important;box-shadow:var(--ec-shadow)!important;max-width:100%}",
    ".checkbox{margin-top:0!important;margin-bottom:8px!important}.checkbox label{line-height:1.45;color:var(--ec-text)}",
    ".nav-tabs{border-bottom:1px solid var(--ec-border);display:flex;gap:8px;flex-wrap:wrap}",
    ".nav-tabs>li>a{border:1px solid transparent!important;color:var(--ec-muted);font-weight:760;padding:10px 14px;border-radius:12px!important;background:transparent!important}",
    ".nav-tabs>li.active>a{color:var(--ec-text)!important;border-color:var(--ec-border)!important;background:#f9fbff!important;box-shadow:inset 0 -2px 0 var(--ec-accent)!important}",
    ".ec-empty{padding:84px 30px;text-align:center;color:var(--ec-muted)}.ec-empty b{display:block;color:var(--ec-text);font-size:22px;margin-bottom:10px}",
    ".ec-empty ol{display:inline-block;text-align:left;margin:10px auto 0;padding-left:22px;line-height:1.8}",
    ".ec-status{padding:12px 14px;border-radius:14px;background:var(--ec-soft);font-size:13px;margin-top:12px;white-space:pre-wrap;line-height:1.5;border:1px solid transparent}",
    ".ec-error{background:#fff1f2;color:#9f1239;border-color:#fecdd3}.ec-warn{background:#fffbeb;color:#92400e;border-color:#fde68a}.ec-ok{background:var(--ec-success);color:#116149;border-color:#bbf7d0}",
    ".ec-main-head{display:flex;justify-content:space-between;gap:12px;align-items:flex-start;margin-bottom:8px}",
    ".ec-main-title{font-size:20px;font-weight:800;letter-spacing:-.02em}",
    ".ec-main-sub{font-size:13px;color:var(--ec-muted);line-height:1.5;max-width:900px}",
    ".ec-note{font-size:12px;color:var(--ec-muted);margin:0 0 12px;line-height:1.5}",
    ".ec-table-wrap{overflow-x:auto;overflow-y:hidden;max-width:100%;min-width:0;-webkit-overflow-scrolling:touch}",
    ".ec-table-wrap table{border-collapse:separate;border-spacing:0;width:100%;font-size:13px;min-width:720px}.ec-table-compact table{min-width:0;table-layout:auto}",
    ".ec-table-wrap table th,.ec-table-wrap table td{padding:10px 10px;border-bottom:1px solid var(--ec-border);white-space:nowrap}",
    ".ec-table-wrap table th{background:#f8fbff;font-weight:780;position:sticky;top:0;z-index:1}",
    ".ec-table-wrap table td.num{text-align:right;font-variant-numeric:tabular-nums}",
    ".coef-table .model-group{text-align:center;border-left:2px solid #d7dce3;background:#eef4fb}",
    ".coef-table .model-start{border-left:2px solid #d7dce3}",
    ".coef-table .term-head,.coef-table .term{position:sticky;left:0;background:white;font-weight:720;z-index:2}",
    ".coef-table .term-head{background:#f8fbff;z-index:4}",
    ".coef-table tbody tr:hover td{background:#f7fbff}.coef-table tbody tr:hover .term{background:#f7fbff}",
    ".ec-kicker{font-size:11px;text-transform:uppercase;letter-spacing:.08em;color:var(--ec-muted);font-weight:800;margin-bottom:6px}",
    ".ec-tip-box{background:#f8fbff;border:1px dashed #c8d8ef;border-radius:16px;padding:12px 14px;font-size:12px;color:var(--ec-muted);line-height:1.55;margin-top:8px}",
    ".ec-diag-toolbar{display:flex;gap:14px;align-items:end;flex-wrap:wrap;padding:14px;background:#f8fbff;border:1px solid var(--ec-border);border-radius:16px;margin-bottom:14px}.ec-diag-toolbar .form-group{margin:0;min-width:260px}",
    ".ec-diag-picker-head{display:flex;justify-content:space-between;gap:12px;align-items:flex-start;margin:2px 0 8px}.ec-diag-picker-head h4{font-size:15px;font-weight:820;margin:0 0 4px}",
    ".ec-diag-actions{display:flex;gap:9px;flex-wrap:wrap;margin:12px 0 9px}.ec-diag-actions .btn{padding:8px 12px!important}",
    ".ec-synthesis{background:#f5f8ff;border:1px solid #d6e3fb;border-left:4px solid var(--ec-accent);border-radius:16px;padding:14px 16px;margin:12px 0 18px}.ec-synthesis-title{font-size:13px;font-weight:820;margin-bottom:5px}.ec-synthesis p{font-size:13px;line-height:1.55;color:#42536b;margin:0}",
    ".ec-diag-section{margin:18px 0 22px}.ec-diag-section-title{font-size:13px;text-transform:uppercase;letter-spacing:.06em;font-weight:820;color:var(--ec-muted);margin-bottom:9px}",
    ".ec-diag-grid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:12px}.ec-diag-card{border:1px solid var(--ec-border);border-radius:18px;padding:15px;background:#fff;box-shadow:0 5px 16px rgba(19,34,56,.045)}",
    ".ec-diag-card-head{display:flex;justify-content:space-between;gap:10px;align-items:flex-start}.ec-diag-name{font-weight:820;font-size:15px}.ec-diag-kind{font-size:11px;color:var(--ec-muted);margin-top:3px}.ec-result{font-size:10px;font-weight:800;padding:5px 8px;border-radius:999px;white-space:nowrap;background:#eef4fb;color:#40526b}.ec-result-evidence_against_h0{background:#fff4e5;color:#8a4b08}.ec-result-no_evidence_against_h0{background:#eef7f2;color:#286044}.ec-result-unavailable{background:#f3f4f6;color:#6b7280}",
    ".ec-stat-row{display:flex;gap:9px;flex-wrap:wrap;margin:12px 0}.ec-stat{background:#f8fbff;border:1px solid var(--ec-border);border-radius:10px;padding:7px 9px;min-width:88px}.ec-stat span{display:block;font-size:10px;color:var(--ec-muted);text-transform:uppercase;letter-spacing:.04em}.ec-stat b{font-size:13px;font-variant-numeric:tabular-nums}.ec-simple-p{display:inline-block;background:#f8fbff;border:1px solid var(--ec-border);border-radius:999px;padding:5px 8px;font-size:11px;font-weight:760;margin:10px 0 2px}.ec-diag-interpretation{font-size:12.5px;line-height:1.5;margin:7px 0;color:#34465e}.ec-more summary{font-size:11px;color:var(--ec-accent);cursor:pointer;font-weight:700}.ec-more p{font-size:11.5px;color:var(--ec-muted);line-height:1.5;margin:7px 0 0}.ec-empty-small{padding:35px 20px}",
    ".ec-inference-box{margin:14px 0 20px}.ec-inference-box h4{font-size:14px;font-weight:820;margin:0 0 8px}.ec-raw{margin-top:20px}.ec-raw summary{cursor:pointer;font-size:12px;font-weight:760;color:var(--ec-muted);margin-bottom:10px}",
    ".ec-explore-intro{background:#f8fbff;border:1px solid var(--ec-border);border-radius:18px;padding:18px;margin:8px 0 16px}.ec-explore-intro h3{margin:0 0 6px;font-size:18px}.ec-explore-intro p{margin:0;color:var(--ec-muted);font-size:13px;line-height:1.5}",
    ".ec-explore-metrics{display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:10px;margin-top:14px}.ec-mini-metric{background:#fff;border:1px solid var(--ec-border);border-radius:14px;padding:12px}.ec-mini-metric b{display:block;font-size:20px}.ec-mini-metric span{font-size:11px;color:var(--ec-muted);text-transform:uppercase;font-weight:760}",
    ".ec-explore-grid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:14px}.ec-explore-card{border:1px solid var(--ec-border);border-radius:18px;padding:16px;background:#fff;margin-bottom:14px;min-width:0;max-width:100%}.ec-explore-card h4{font-size:15px;font-weight:820;margin:0 0 5px}.ec-explore-wide{margin-top:0}.shiny-plot-output{width:100%!important;max-width:100%!important;min-width:0!important;overflow:hidden}",
    ".ec-profile-list{border-top:1px solid var(--ec-border)}.ec-profile-row{display:flex;justify-content:space-between;gap:16px;padding:8px 2px;border-bottom:1px solid var(--ec-border);font-size:12px}.ec-profile-row span{color:var(--ec-muted)}.ec-profile-row span,.ec-profile-row b{min-width:0;overflow-wrap:anywhere}.ec-profile-row b{text-align:right;font-variant-numeric:tabular-nums}",
    ".ec-rel-controls{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:12px;min-width:0}.ec-corr-note{background:#f8fbff;border:1px solid var(--ec-border);border-radius:12px;padding:10px 12px;font-size:12px;margin-bottom:8px;overflow-wrap:anywhere}.ec-corr-note span{color:var(--ec-muted)}.ec-check-list{margin:8px 0 0;padding-left:20px;color:#3a4b61;font-size:12.5px;line-height:1.55}.ec-check-list li{margin-bottom:7px}.ec-ordinal-settings{min-width:0;max-width:100%}.tab-content,.tab-pane,.ec-section{min-width:0;max-width:100%}",
    "@media(max-width:1080px){.ec-top{flex-direction:column}.ec-metric-stack{width:auto;grid-template-columns:repeat(2,minmax(0,1fr))}.ec-grid{grid-template-columns:1fr}.ec-sidebar{position:static}.ec-diag-grid{grid-template-columns:1fr}.ec-explore-grid{grid-template-columns:1fr}.ec-explore-metrics{grid-template-columns:repeat(2,minmax(0,1fr))}}",
    "@media(max-width:640px){.ec-shell{padding:12px}.ec-hero{padding:20px}.ec-title{font-size:28px}.ec-metric-stack{grid-template-columns:1fr 1fr}.ec-rel-controls{grid-template-columns:1fr}.ec-main-head{flex-direction:column}.ec-table-wrap table{min-width:620px}.ec-table-compact table{min-width:0}}"
  )
}

.ec_split_ordinal_compare <- function(df) {
  if (!is.data.frame(df)) return(list(slopes = data.frame(), thresholds = data.frame()))
  if (!nrow(df) || !"term" %in% names(df)) {
    return(list(slopes = df, thresholds = df[0, , drop = FALSE]))
  }
  is_threshold <- grepl("^threshold: ", as.character(df$term))
  list(
    slopes = df[!is_threshold, , drop = FALSE],
    thresholds = df[is_threshold, , drop = FALSE]
  )
}

.ec_coef_shiny_table <- function(df) {
  shiny::HTML(.ec_coef_html_table(df))
}
