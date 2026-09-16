\name{eco_app}
\alias{eco_app}
\title{Launch the interactive econcompare model laboratory}
\usage{eco_app(data, launch.browser = getOption("shiny.launch.browser", interactive()))}
\arguments{
\item{data}{A data.frame to analyse.}
\item{launch.browser}{Passed to \code{shiny::runApp()}.}
}
\value{A Shiny application object, invisibly after the app exits.}
\description{Launches a simple cross-section workspace with a pre-model data explorer, user selection of the dependent-variable objective, compatible model selection, comparison views and a short user-controlled diagnostic checklist.}
