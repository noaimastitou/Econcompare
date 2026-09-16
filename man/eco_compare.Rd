\name{eco_compare}
\alias{eco_compare}
\title{Compare collected model results}
\usage{eco_compare(
  x,
  empty_stats = c("na", "error"),
  error_policy = c("stop", "collect")
)}
\arguments{
\item{x}{An \code{econcompare} object returned by \code{eco_run()}.}
\item{empty_stats}{How to handle unavailable length-zero fit statistics. \code{"na"} converts them to \code{NA}; \code{"error"} stops.}
\item{error_policy}{How result-extraction failures are handled. \code{"stop"} is strict. \code{"collect"} keeps successfully extracted models and records model-specific extraction issues.}
}
\value{A tidy data.frame containing coefficient statistics, model metadata and available fit statistics. With \code{error_policy = "collect"}, attributes \code{extraction_warnings} and \code{extraction_failures} contain model-specific extraction issues.}
\description{Extracts comparable coefficient and fit information from successfully estimated models. Collection mode isolates post-estimation extraction problems so one failing model summary does not discard successful alternatives.}
