\name{eco_binary_compare}
\alias{eco_binary_compare}
\title{Compare simple in-sample prediction summaries for binary models}
\usage{
eco_binary_compare(x, threshold = 0.5)
}
\arguments{
\item{x}{An \code{econcompare} object containing binary-outcome models.}
\item{threshold}{Classification threshold used for the descriptive accuracy rate. Must lie strictly between 0 and 1.}
}
\value{A data.frame with model-level in-sample prediction summaries.}
\description{Provides a deliberately small set of common descriptive quantities for LPM, logit and probit models. These summaries support exploration rather than automatic model selection. The Brier score is reported only when fitted values lie in the probability range [0,1].}
