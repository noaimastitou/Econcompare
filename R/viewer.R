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
        vals <- vapply(z[1, , drop = FALSE], function(v) .ec_fmt(as.numeric(v)), character(1))
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
#' @param x An `econcompare` object.
#' @param file Optional path for the generated HTML file.
#' @param open Logical; open the viewer after generating it.
#' @param empty_stats How to handle length-0 fit statistics: `\"na\"` shows them as NA and lists them in Model fit; `\"error\"` stops.
#' @return Invisibly, the path to the generated HTML file.
#' @export
eco_view <- function(x, file = NULL, open = interactive(), empty_stats = c("na", "error")) {
  if (!inherits(x, "econcompare")) .ec_stop("`x` must be an econcompare object.")
  empty_stats <- match.arg(empty_stats)
  cmp <- eco_compare(x, empty_stats = empty_stats)
  diag <- eco_diagnostics(x)

  fit_cols <- intersect(c("model", "engine", "family", "nobs", "r2", "adj_r2", "logLik", "aic", "bic", "deviance", "unavailable_stats"), names(cmp))
  fit <- unique(cmp[fit_cols])
  fit_num <- vapply(fit, is.numeric, logical(1))
  fit[fit_num] <- lapply(fit[fit_num], .ec_fmt)

  diag_num <- vapply(diag, is.numeric, logical(1))
  diag[diag_num] <- lapply(diag[diag_num], .ec_fmt)

  if (is.null(file)) file <- tempfile("econcompare-", fileext = ".html")

  html <- paste0(
'<!doctype html><html><head><meta charset="utf-8"><title>econcompare</title>
<style>
body{font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",sans-serif;margin:0;background:#f6f7f9;color:#17202a}
.wrap{max-width:1400px;margin:auto;padding:28px}.hero{background:#17202a;color:white;padding:24px 28px;border-radius:16px;margin-bottom:18px}
.hero h1{margin:0 0 6px;font-size:28px}.hero p{margin:0;opacity:.8}.tabs{display:flex;gap:8px;margin:14px 0}.tab{border:0;background:white;padding:10px 14px;border-radius:10px;cursor:pointer;font-weight:600}.tab.active{background:#17202a;color:white}
.panel{display:none;background:white;padding:18px;border-radius:14px;box-shadow:0 2px 12px rgba(0,0,0,.05)}.panel.active{display:block}
table{border-collapse:collapse;width:100%;font-size:14px}th,td{text-align:left;padding:9px 10px;border-bottom:1px solid #e8eaed}th{background:#fafafa}.hint{font-size:13px;color:#5f6b76;margin:8px 0 16px}
input{padding:10px 12px;width:100%;box-sizing:border-box;border:1px solid #d7dce1;border-radius:9px;margin-bottom:12px}
.badge{display:inline-block;background:#edf2f7;padding:4px 8px;border-radius:999px;margin-right:6px;font-size:12px}
.table-scroll{overflow-x:auto}.coef-table{min-width:760px}.coef-table .term-head,.coef-table .term{position:sticky;left:0;z-index:2;background:white;font-weight:600;white-space:nowrap}.coef-table .term-head{z-index:4;background:#fafafa}
.coef-table .model-group{text-align:center;font-size:15px;border-left:3px solid #cbd3dc;border-bottom:2px solid #cbd3dc;background:#f1f4f7}.coef-table .model-start{border-left:3px solid #cbd3dc}.coef-table .num{text-align:right;font-variant-numeric:tabular-nums;white-space:nowrap}.coef-table tbody tr:hover td{background:#f7f9fb}.coef-table tbody tr:hover .term{background:#f7f9fb}
</style></head><body><div class="wrap"><div class="hero"><h1>econcompare</h1><p>Beta 0.6.0 — OLS-reference cross-sectional comparison</p></div>',
'<div><span class="badge">Models: ', length(x$models), '</span><span class="badge">Generated: ', .ec_escape_html(format(x$created)), '</span></div>',
'<div class="tabs"><button class="tab active" data-tab="coef">Coefficients</button><button class="tab" data-tab="fit">Model fit</button><button class="tab" data-tab="diag">Diagnostics</button></div>',
'<div id="coef" class="panel active"><input id="search" placeholder="Filter by term..."><div class="hint">Each variable is shown once, with coefficient statistics grouped side by side by model.</div>',
.ec_coef_html_table(cmp), '</div>',
'<div id="fit" class="panel"><div class="hint">NA means that the statistic is unavailable or was returned with length 0 for that model. The unavailable_stats column makes this explicit. Compare fit statistics only when they are defined on a compatible basis.</div>', .ec_html_table(fit), '</div>',
'<div id="diag" class="panel">', .ec_html_table(diag), '</div>',
'<script>
document.querySelectorAll(".tab").forEach(b=>b.addEventListener("click",()=>{document.querySelectorAll(".tab,.panel").forEach(x=>x.classList.remove("active"));b.classList.add("active");document.getElementById(b.dataset.tab).classList.add("active")}));
document.getElementById("search").addEventListener("input",function(){let q=this.value.toLowerCase();document.querySelectorAll("#coef tbody tr").forEach(r=>r.style.display=r.dataset.term.toLowerCase().includes(q)?"":"none")});
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
