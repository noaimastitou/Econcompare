\name{eco_vecm_rank_test}
\alias{eco_vecm_rank_test}
\title{Inspect Johansen rank evidence retained by a VECM}
\usage{eco_vecm_rank_test(x)}
\arguments{\item{x}{A VECM result returned by \code{eco_system_run()}.}}
\value{A data.frame of Johansen rank-test statistics and critical values.}
\description{Returns the Johansen evidence stored with a fitted VECM without changing the researcher-specified rank.}
