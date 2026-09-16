\name{eco_johansen_test}
\alias{eco_johansen_test}
\title{Johansen cointegration rank diagnostics}
\usage{
eco_johansen_test(data, variables, time, K = 2L,
  type = c("trace", "eigen"), ecdet = c("const", "none", "trend"),
  spec = c("transitory", "longrun"))
}
\arguments{
\item{data}{A data.frame.}
\item{variables}{Numeric system variables in levels.}
\item{time}{Time-index column.}
\item{K}{Lag order of the VAR in levels used by the Johansen procedure.}
\item{type}{\code{"trace"} or \code{"eigen"}.}
\item{ecdet}{Deterministic component: \code{"none"}, \code{"const"}, or \code{"trend"}.}
\item{spec}{Johansen specification: \code{"transitory"} or \code{"longrun"}.}
}
\value{A data.frame with rank-null hypotheses, test statistics and critical values.}
\description{Reports Johansen rank evidence and leaves the cointegration-rank decision to the researcher.}
