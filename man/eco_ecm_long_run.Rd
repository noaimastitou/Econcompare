\name{eco_ecm_long_run}
\alias{eco_ecm_long_run}
\title{Inspect the long-run levels relationship used by an ECM}
\usage{eco_ecm_long_run(x)}
\arguments{\item{x}{An object returned by \code{eco_ecm_run()}.}}
\value{A tidy coefficient table for the first-step levels regression.}
\description{Reports the levels relationship used to construct the ECM error-correction term. The table is not itself a cointegration test.}
