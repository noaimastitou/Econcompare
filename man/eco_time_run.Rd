\name{eco_time_run}
\alias{eco_time_run}
\title{Estimate explanatory time-series econometric regressions}
\usage{
eco_time_run(data, formula, time, models = "time_static", p = 1L, q = 1L,
  q_by_var = NULL, inference = c("classical", "HAC"), hac_lag = NULL,
  error_policy = c("stop", "collect"))
}
\arguments{
\item{data}{A data.frame.}
\item{formula}{A formula such as \code{y ~ x1 + x2}, using simple numeric columns.}
\item{time}{Name of the explicit time-index column.}
\item{models}{Character vector of engines from \code{eco_time_models()}.}
\item{p}{Number of dependent-variable lags for dynamic/ARDL models.}
\item{q}{Default/common explanatory-variable lag order for distributed-lag/ARDL models.}
\item{q_by_var}{Optional named integer vector or named list overriding \code{q} for selected explanatory variables, for example \code{c(education_spending = 8, unemployment = 2)}. Unspecified variables retain \code{q}.}
\item{inference}{\code{"classical"} or \code{"HAC"}. HAC uses Newey-West inference and requires optional packages \pkg{sandwich} and \pkg{lmtest}. HAC is refused when the time grid is irregular or incomplete.}
\item{hac_lag}{Optional non-negative Newey-West truncation lag.}
\item{error_policy}{Either \code{"stop"} or \code{"collect"}.}
}
\value{An object of class \code{econcompare} with \code{analysis_type = "time_series"}, model-specific lag metadata, and a structured \code{time_metadata} component.}
\description{Fits explanatory temporal regressions. Lagged engines require a complete regular calendar grid so a lag always means a true period lag rather than merely the previous observed row.}
