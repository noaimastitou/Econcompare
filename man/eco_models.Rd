\name{eco_models}
\alias{eco_models}
\title{List supported cross-sectional model engines}
\usage{eco_models(outcome_type = NULL)}
\arguments{\item{outcome_type}{Optional filter: \code{"continuous"}, \code{"binary"}, \code{"nominal"}, or \code{"ordinal"}.}}
\value{A data.frame describing supported engines, outcome groups, packages, comparison notes and local availability.}
\description{Lists cross-sectional estimators registered in econcompare, grouped by dependent-variable objective. OLS is an available model rather than a mandatory reference.}
