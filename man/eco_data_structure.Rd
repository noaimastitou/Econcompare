\name{eco_data_structure}
\alias{eco_data_structure}
\title{Detect the likely observational structure of a dataset}
\usage{eco_data_structure(data)}
\arguments{\item{data}{A data.frame.}}
\value{A list containing the suggested structure, confidence, suggested time variable, reason, and temporal candidates.}
\description{Conservatively detects reliable temporal-index candidates without treating ambiguous date strings or arbitrary numeric codes as dates. Detection is advisory and remains subject to explicit researcher confirmation.}
