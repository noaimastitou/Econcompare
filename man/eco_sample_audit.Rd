\name{eco_sample_audit}
\alias{eco_sample_audit}
\title{Audit estimation-sample consistency across models}
\usage{eco_sample_audit(x, reference = NULL)}
\arguments{
\item{x}{An \code{econcompare} object returned by \code{eco_run()}.}
\item{reference}{Optional fitted model name used as the technical sample-comparison anchor. The first fitted model is used by default.}
}
\value{A data frame reporting observations used, effective WLS sample size, row-identity certainty, and whether each model matches the chosen sample anchor.}
\description{Helps distinguish model differences from estimation-sample differences. The sample anchor is technical only and is not treated as an econometric benchmark.}
