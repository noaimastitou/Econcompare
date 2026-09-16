\name{eco_time_diagnostics}
\alias{eco_time_diagnostics}
\title{Essential diagnostics for explanatory time-series regressions}
\usage{
eco_time_diagnostics(x, bg_order = 1L)
}
\arguments{
\item{x}{An \code{econcompare} time-series result returned by \code{eco_time_run()}.}
\item{bg_order}{Positive integer lag order for the Breusch-Godfrey test.}
}
\value{A data.frame with model-specific diagnostic results.}
\description{Runs explicit residual serial-correlation diagnostics. Breusch-Godfrey is refused on an irregular or incomplete time grid because an observation lag would not represent a constant calendar lag.}
