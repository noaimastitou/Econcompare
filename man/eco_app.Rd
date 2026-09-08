\name{eco_app}
\alias{eco_app}
\title{Launch the interactive econcompare model laboratory}
\usage{
eco_app(data, launch.browser = getOption("shiny.launch.browser", interactive()))
}
\arguments{
\item{data}{A data.frame to analyse, for example \code{mtcars}.}
\item{launch.browser}{Passed to \code{shiny::runApp()}.}
}
\value{A Shiny application object, invisibly after the app exits.}
\description{Launches a click-driven interface for choosing variables, models, model-specific parameters and comparing results against an OLS reference.}
\examples{
\dontrun{eco_app(mtcars)}
}
