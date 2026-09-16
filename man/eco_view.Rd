\name{eco_view}
\alias{eco_view}
\title{Open an HTML comparison viewer}
\usage{eco_view(x, file = NULL, open = interactive(), empty_stats = c("na", "error"), diagnostics = FALSE)}
\arguments{
\item{x}{An \code{econcompare} object.}
\item{file}{Optional output path for the generated HTML file.}
\item{open}{Logical; open the viewer after generation.}
\item{empty_stats}{How to handle unavailable length-zero fit statistics.}
\item{diagnostics}{Diagnostics to include. \code{FALSE} (default) runs none; \code{TRUE} or \code{"all"} runs all compatible diagnostics; a character vector runs only the selected diagnostic ids.}
}
\value{Invisibly, the path to the generated HTML file.}
\description{Generates a lightweight standalone HTML view of coefficients and fit statistics, with diagnostics included only when explicitly requested. Result extraction is isolated by model so successful alternatives remain viewable when another model summary fails.}
