\name{eco_system_run}
\alias{eco_system_run}
\title{Estimate a VAR or VECM dynamic system}
\usage{
eco_system_run(data, variables, time, model = c("var", "vecm"), p = 2L,
  deterministic = c("const", "trend", "both", "none"), rank = NULL,
  johansen_type = c("trace", "eigen"), ecdet = c("const", "none", "trend"),
  spec = c("transitory", "longrun"))
}
\arguments{
\item{data}{A data.frame.}
\item{variables}{At least two numeric endogenous variables.}
\item{time}{Name of the validated time index.}
\item{model}{\code{"var"} or \code{"vecm"}.}
\item{p}{VAR lag order. For VECM this is the VAR lag order in levels and must be at least 2.}
\item{deterministic}{VAR deterministic terms.}
\item{rank}{Explicit VECM cointegration rank. Required for VECM.}
\item{johansen_type}{Johansen statistic used in the retained VECM object.}
\item{ecdet}{VECM deterministic component.}
\item{spec}{Johansen \code{"transitory"} or \code{"longrun"} specification.}
}
\value{An object inheriting from \code{econcompare_system} and \code{econcompare}, with equation-level results, the retained system fit and temporal metadata.}
\description{Estimates reduced-form VAR or VECM systems. No lag order, cointegration rank or structural identification is imposed automatically.}
