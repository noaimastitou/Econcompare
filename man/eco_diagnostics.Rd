\name{eco_diagnostics}
\alias{eco_diagnostics}
\title{Cross-section diagnostics for collected models}
\usage{
eco_diagnostics(x, alpha = 0.05, models = NULL, tests = NULL)
}
\arguments{
\item{x}{An \code{econcompare} object returned by \code{eco_run()}.}
\item{alpha}{Significance level used to describe hypothesis-test outcomes.}
\item{models}{Optional character vector of fitted model names to diagnose. \code{NULL} uses all fitted models.}
\item{tests}{Optional character vector of diagnostic identifiers to execute. \code{NULL} runs all compatible diagnostics; supplying ids runs only that selection.}
}
\value{
A data.frame containing model, engine, diagnostic category, statistic, degrees of freedom, p-value, result label, interpretation and methodological details.
}
\description{
Computes compatibility-aware cross-section diagnostics. The programmatic API can run all compatible diagnostics or an explicit user-selected subset; the Shiny app always requires explicit selection before computation. Coefficient summaries exclude estimator-specific ancillary parameters and, for Heckman models, focus on the outcome equation. Classical residual and specification diagnostics are currently restricted to the OLS engine where validated. IV diagnostics are restricted to IV specifications and are parsed from the documented ivreg diagnostic-table schema.
}
