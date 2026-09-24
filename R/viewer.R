.ec_html_table <- function(df) {
  headers <- paste0("<th>", .ec_escape_html(names(df)), "</th>", collapse = "")
  rows <- apply(df, 1, function(r) {
    paste0("<tr>", paste0("<td>", .ec_escape_html(r), "</td>", collapse = ""), "</tr>")
  })
  paste0("<table><thead><tr>", headers, "</tr></thead><tbody>", paste(rows, collapse = "\n"), "</tbody></table>")
}

.ec_coef_html_table <- function(df) {
  models <- unique(df$model)
  terms <- unique(df$term)
  measures <- c("estimate", "std.error", "statistic", "p.value")
  labels <- c("Estimate", "Std. Error", "Statistic", "p-value")

  model_headers <- paste0(
    '<th class="model-group" colspan="', length(measures), '">',
    .ec_escape_html(models),
    '</th>',
    collapse = ""
  )

  sub_headers <- paste(vapply(seq_along(models), function(i) {
    paste0(
      '<th class="model-start">', labels[1], '</th>',
      paste0('<th>', labels[-1], '</th>', collapse = "")
    )
  }, character(1)), collapse = "")

  rows <- vapply(terms, function(term) {
    cells <- vapply(models, function(model_name) {
      z <- df[df$model == model_name & df$term == term, measures, drop = FALSE]
      if (!nrow(z)) {
        vals <- rep("—", length(measures))
      } else {
        vals <- vapply(names(z), function(k) if (k == "p.value") .ec_fmt_p(as.numeric(z[[k]][1L])) else .ec_fmt(as.numeric(z[[k]][1L])), character(1))
      }
      paste0(
        '<td class="num model-start">', .ec_escape_html(vals[1]), '</td>',
        paste0('<td class="num">', .ec_escape_html(vals[-1]), '</td>', collapse = "")
      )
    }, character(1))

    paste0(
      '<tr data-term="', .ec_escape_html(term), '">',
      '<td class="term">', .ec_escape_html(term), '</td>',
      paste(cells, collapse = ""),
      '</tr>'
    )
  }, character(1))

  paste0(
    '<div class="table-scroll"><table class="coef-table"><thead>',
    '<tr><th class="term-head" rowspan="2">Term</th>', model_headers, '</tr>',
    '<tr>', sub_headers, '</tr>',
    '</thead><tbody>', paste(rows, collapse = "\n"), '</tbody></table></div>'
  )
}

#' Open an interactive HTML comparison viewer
#'
#' Result extraction is isolated by fitted model so a post-estimation summary
#' failure in one alternative does not discard successful results from others.
#'
#' @param x An `econcompare` object.
#' @param file Optional path for the generated HTML file.
#' @param open Logical; open the viewer after generating it.
#' @param empty_stats How to handle length-0 fit statistics: `\"na\"` shows them as NA and lists them in Model fit; `\"error\"` stops.
#' @param diagnostics Diagnostics to include in the HTML viewer. `FALSE` (default) runs none; `TRUE` or `\"all\"` runs all compatible diagnostics; a character vector runs only the selected diagnostic ids.
#' @return Invisibly, the path to the generated HTML file.
#' @export
eco_view <- function(x, file = NULL, open = interactive(), empty_stats = c("na", "error"),
                     diagnostics = FALSE) {
  if (!inherits(x, "econcompare")) .ec_stop("`x` must be an econcompare object.")
  empty_stats <- match.arg(empty_stats)
  cmp <- eco_compare(x, empty_stats = empty_stats, error_policy = "collect")
  if (identical(x$analysis_type, "panel") && "term_label" %in% names(cmp)) cmp$term <- cmp$term_label
  extraction_warnings <- attr(cmp, "extraction_warnings")
  extraction_failures <- attr(cmp, "extraction_failures")
  if (is.null(extraction_warnings)) extraction_warnings <- .ec_empty_extraction_issue()
  if (is.null(extraction_failures)) extraction_failures <- .ec_empty_extraction_issue()
  if (!nrow(cmp)) {
    detail <- if (nrow(extraction_failures)) paste(unique(extraction_failures$message), collapse = "; ") else "unknown extraction failure"
    .ec_stop("No fitted model result could be extracted for the HTML viewer: ", detail)
  }
  issue_rows <- function(z, issue, stage) {
    if (!is.data.frame(z) || !nrow(z)) return(NULL)
    if (!"stage" %in% names(z)) z$stage <- stage
    for (nm in c("model", "engine", "message")) if (!nm %in% names(z)) z[[nm]] <- NA_character_
    z$issue <- issue
    z[, c("issue", "model", "engine", "stage", "message"), drop = FALSE]
  }
  issues <- do.call(rbind, list(
    issue_rows(x$warnings, "warning", "estimation"),
    issue_rows(x$failures, "failure", "estimation"),
    issue_rows(extraction_warnings, "warning", "extraction"),
    issue_rows(extraction_failures, "failure", "extraction")))
  if (is.null(issues)) issues <- data.frame()
  panel_limits <- data.frame()

  if (isFALSE(diagnostics)) {
    diag <- data.frame()
  } else if (isTRUE(diagnostics) || (is.character(diagnostics) && length(diagnostics) == 1L && identical(tolower(diagnostics), "all"))) {
    if (identical(x$analysis_type, "panel")) {
      capabilities <- .ec_panel_capabilities(x)
      selected <- unique(capabilities$test[capabilities$selectable])
      panel_limits <- capabilities[!capabilities$selectable, c("model", "test", "reason"), drop = FALSE]
      diag <- if (length(selected)) eco_panel_diagnostics(x, tests = selected) else data.frame()
    } else if (identical(x$analysis_type, "time_series_system")) {
      dz <- eco_system_diagnostics(x)
      roots <- if (is.data.frame(dz$roots) && nrow(dz$roots)) data.frame(
        diagnostic = paste0("companion root ", dz$roots$root), statistic = dz$roots$modulus,
        p.value = NA_real_, note = dz$root_note, stringsAsFactors = FALSE
      ) else data.frame()
      serial <- if (is.data.frame(dz$serial_correlation) && nrow(dz$serial_correlation)) data.frame(
        diagnostic = dz$serial_correlation$test, statistic = dz$serial_correlation$statistic,
        p.value = dz$serial_correlation$p.value, note = dz$serial_correlation$note, stringsAsFactors = FALSE
      ) else data.frame()
      pieces <- Filter(function(z) is.data.frame(z) && nrow(z), list(roots, serial))
      diag <- if (length(pieces)) do.call(rbind, pieces) else data.frame()
    } else if (identical(x$analysis_type, "time_series")) {
      diag <- eco_time_diagnostics(x)
    } else {
      diag <- eco_diagnostics(x)
    }
  } else if (is.character(diagnostics) && length(diagnostics)) {
    if (identical(x$analysis_type, "panel")) {
      diag <- eco_panel_diagnostics(x, tests = diagnostics)
    } else {
    if (isTRUE(x$analysis_type %in% c("time_series", "time_series_system"))) .ec_stop("Named cross-sectional diagnostic ids are not used for temporal viewers. Use diagnostics = TRUE for the compact temporal diagnostic layer.")
    diag <- eco_diagnostics(x, tests = diagnostics)
    }
  } else {
    .ec_stop("`diagnostics` must be FALSE, TRUE, \"all\", or a character vector of diagnostic ids.")
  }

  fit <- .ec_model_fit_table(cmp, x$outcome_type)
  if (identical(x$analysis_type, "panel")) fit <- .ec_panel_visible_metrics(fit)
  fit_num <- vapply(fit, is.numeric, logical(1))
  fit[fit_num] <- lapply(fit[fit_num], .ec_fmt)

  diag_audit <- data.frame()
  if (identical(x$analysis_type, "panel") && nrow(diag)) {
    diag_audit <- diag[diag$status == "computed_uninterpreted", intersect(c("model", "test", "statistic", "raw_p.value", "note"), names(diag)), drop = FALSE]
    diag <- diag[, setdiff(names(diag), "raw_p.value"), drop = FALSE]
  }
  if (nrow(diag)) {
    diagnostic_p <- diag$p.value
    diag_num <- vapply(diag, is.numeric, logical(1))
    diag[diag_num] <- lapply(diag[diag_num], .ec_fmt)
    if ("p.value" %in% names(diag)) diag$p.value <- .ec_fmt_p(diagnostic_p)
  }

  coef_html <- if (identical(x$outcome_type, "ordinal")) {
    parts <- .ec_split_ordinal_compare(cmp)
    paste0(
      '<h3>Slope coefficients</h3><div class="hint">Ordered-model slopes shift a latent index. Ordered logit and ordered probit use different latent scales, so raw magnitudes should not be compared directly.</div>',
      if (nrow(parts$slopes)) .ec_coef_html_table(parts$slopes) else '<div class="hint">No slope coefficients were extracted.</div>',
      if (nrow(parts$thresholds)) paste0(
        '<details class="technical"><summary>Category thresholds · technical parameters</summary>',
        '<div class="hint">Thresholds delimit adjacent outcome categories on the latent scale. They are not regressor effects.</div>',
        .ec_coef_html_table(parts$thresholds), '</details>'
      ) else ''
    )
  } else {
    .ec_coef_html_table(cmp)
  }

  if (identical(x$analysis_type, "panel")) {
    status_table <- cmp[, c("model", "term", "term_status", "inference", "inference_df", "groups", "observation_unit", "covariance_note"), drop = FALSE]
    coef_html <- paste0(
      '<div class="hint">Static panel models; coefficient scales and inference differ by family. Absorbed terms are unavailable, not zero effects. Inspect each model’s observation unit and covariance below.</div>',
      coef_html, '<details><summary>Term status and inference</summary>', .ec_html_table(status_table), '</details>',
      '<details><summary>Panel interpretation</summary>',
      .ec_html_table(unique(cmp[, c("model", "observation_unit", "comparison_note"), drop = FALSE])), '</details>',
      if (length(x$first_stage_tests)) paste0('<details><summary>IV first-stage relevance</summary>', .ec_html_table(do.call(rbind, x$first_stage_tests)), '</details>') else '',
      if (length(x$components)) paste0('<details><summary>Mundlak components</summary>',
        .ec_html_table(do.call(rbind, x$components)), '</details>') else '')
  }

  if (is.null(file)) file <- tempfile("econcompare-", fileext = ".html")

  html <- paste0(
'<!doctype html><html><head><meta charset="utf-8"><title>econcompare</title>
<style>
body{font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",sans-serif;margin:0;background:#f6f7f9;color:#17202a}
.wrap{max-width:1400px;margin:auto;padding:28px}.hero{background:#17202a;color:white;padding:24px 28px;border-radius:16px;margin-bottom:18px}
.hero h1{margin:0 0 6px;font-size:28px}.hero p{margin:0;opacity:.8}.tabs{display:flex;gap:8px;margin:14px 0}.tab{border:0;background:white;padding:10px 14px;border-radius:10px;cursor:pointer;font-weight:600}.tab.active{background:#17202a;color:white}
.panel{display:none;background:white;padding:18px;border-radius:14px;box-shadow:0 2px 12px rgba(0,0,0,.05);min-width:0;overflow:hidden}.panel.active{display:block}
table{border-collapse:collapse;width:100%;font-size:14px}th,td{text-align:left;padding:9px 10px;border-bottom:1px solid #e8eaed}th{background:#fafafa}.hint{font-size:13px;color:#5f6b76;margin:8px 0 16px}
input{padding:10px 12px;width:100%;box-sizing:border-box;border:1px solid #d7dce1;border-radius:9px;margin-bottom:12px}
.badge{display:inline-block;background:#edf2f7;padding:4px 8px;border-radius:999px;margin-right:6px;font-size:12px}
.table-scroll{overflow-x:auto;max-width:100%;-webkit-overflow-scrolling:touch}.coef-table{min-width:760px}.coef-table .term-head,.coef-table .term{position:sticky;left:0;z-index:2;background:white;font-weight:600;white-space:nowrap}.coef-table .term-head{z-index:4;background:#fafafa}
.coef-table .model-group{text-align:center;font-size:15px;border-left:3px solid #cbd3dc;border-bottom:2px solid #cbd3dc;background:#f1f4f7}.coef-table .model-start{border-left:3px solid #cbd3dc}.coef-table .num{text-align:right;font-variant-numeric:tabular-nums;white-space:nowrap}.coef-table tbody tr:hover td{background:#f7f9fb}.coef-table tbody tr:hover .term{background:#f7f9fb}
</style></head><body><div class="wrap"><div class="hero"><h1>econcompare</h1><p>Beta ', .ec_escape_html(x$version), ' — econometric model exploration</p></div>',
'<div><span class="badge">Models: ', length(x$models), '</span><span class="badge">Generated: ', .ec_escape_html(format(x$created)), '</span></div>',
'<div class="tabs"><button class="tab active" data-tab="coef">Coefficients</button><button class="tab" data-tab="fit">Model fit</button><button class="tab" data-tab="diag">Diagnostics</button>',
if (nrow(issues)) '<button class="tab" data-tab="issues">Warnings / failures</button>' else '',
'</div>',
'<div id="coef" class="panel active"><input id="search" placeholder="Filter by term..."><div class="hint">Each variable is shown once, with coefficient statistics grouped side by side by model.</div>',
coef_html, '</div>',
'<div id="fit" class="panel"><div class="hint">', .ec_escape_html(if (identical(x$analysis_type, "panel")) .ec_panel_fit_note(x) else .ec_model_fit_note(x$outcome_type)), '</div><div class="table-scroll">', .ec_html_table(fit), '</div></div>',
'<div id="diag" class="panel">',
if (nrow(diag)) paste0('<div class="table-scroll">', .ec_html_table(diag), '</div>', if (nrow(diag_audit)) paste0('<details><summary>Raw engine values — audit only</summary><p>Calibration unverified; no automatic conclusion.</p>', .ec_html_table(diag_audit), '</details>') else '') else if (nrow(panel_limits)) '<div class="hint">No supported diagnostic is available for these fitted models. Error dependence has not been assessed.</div>' else '<div class="hint">No diagnostics were run. Pass <code>diagnostics = "all"</code> or explicit diagnostic ids to <code>eco_view()</code> when you want them included.</div>',
if (nrow(panel_limits)) paste0('<details open><summary>Diagnostic limitations</summary><div class="table-scroll">', .ec_html_table(panel_limits), '</div></details>') else '',
'</div>',
if (nrow(issues)) paste0('<div id="issues" class="panel"><div class="hint">Estimation and extraction issues are identified by stage; successful results remain available.</div><div class="table-scroll">', .ec_html_table(issues), '</div></div>') else '',
'<script>
document.querySelectorAll(".tab").forEach(b=>b.addEventListener("click",()=>{document.querySelectorAll(".tab,.panel").forEach(x=>x.classList.remove("active"));b.classList.add("active");document.getElementById(b.dataset.tab).classList.add("active")}));
document.getElementById("search").addEventListener("input",function(){let q=this.value.toLowerCase();document.querySelectorAll("#coef tbody tr").forEach(r=>r.style.display=(r.dataset.term || r.textContent).toLowerCase().includes(q)?"":"none")});
</script></div></body></html>')

  writeLines(html, con = file, useBytes = TRUE)

  if (isTRUE(open)) {
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      rstudioapi::viewer(file)
    } else {
      utils::browseURL(file)
    }
  }
  invisible(normalizePath(file, mustWork = FALSE))
}
