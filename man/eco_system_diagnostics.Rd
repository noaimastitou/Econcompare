\name{eco_system_diagnostics}
\alias{eco_system_diagnostics}
\title{Essential diagnostics for VAR/VECM systems}
\usage{eco_system_diagnostics(x, serial_lags = 8L)}
\arguments{
\item{x}{A VAR/VECM result returned by \code{eco_system_run()}.}
\item{serial_lags}{Lag order for the multivariate Portmanteau residual-autocorrelation test.}
}
\value{A list containing companion-root information and a residual serial-correlation table.}
\description{Provides a compact system-level diagnostic layer. The stationary-VAR root rule is not mechanically applied to a cointegrated VECM.}
