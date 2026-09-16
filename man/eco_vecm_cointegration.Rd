\name{eco_vecm_cointegration}
\alias{eco_vecm_cointegration}
\title{Inspect normalized VECM cointegrating vectors}
\usage{eco_vecm_cointegration(x)}
\arguments{\item{x}{A VECM result returned by \code{eco_system_run()}.}}
\value{A matrix of normalized cointegrating vectors.}
\description{Returns the normalization used by the fitted VECM. Signs and scale of cointegrating vectors are not unique.}
