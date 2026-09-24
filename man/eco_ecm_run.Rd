\name{eco_ecm_run}
\alias{eco_ecm_run}
\title{Estimate a single-equation error-correction model}
\usage{
eco_ecm_run(data, formula, time, p = 1L, q = 0L, q_by_var = NULL,
  long_run_intercept = TRUE, inference = c("classical", "HAC"),
  hac_lag = NULL)
}
\arguments{
\item{data}{A data.frame.}
\item{formula}{Levels relationship such as \code{y ~ x1 + x2}.}
\item{time}{Name of the validated time-index column.}
\item{p}{Number of lagged first differences of the dependent variable.}
\item{q}{Default number of lagged first differences of each explanatory variable.}
\item{q_by_var}{Optional named lag overrides for explanatory variables.}
\item{long_run_intercept}{Logical; include an intercept in the long-run levels equation.}
\item{inference}{\code{"classical"} or \code{"HAC"} for the short-run equation.}
\item{hac_lag}{Optional Newey-West truncation lag.}
}
\value{An \code{econcompare} object containing the short-run ECM, the first-step long-run regression, the error-correction term and temporal metadata.}
\description{Fits a transparent two-step ECM. The first-step levels regression is used to construct the lagged error-correction term but is not treated as proof of cointegration.}

\section{Specification safeguards}{The response must be a single named numeric column; precompute transformations in the input data. Input formulas without an intercept are rejected explicitly. In ECM, use long_run_intercept = FALSE for the first-step intercept. Collisions between generated lag/difference names are rejected before estimation. The provenance component records requested and fitted formulas and engine versions.}
