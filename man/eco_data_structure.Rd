\name{eco_data_structure}
\alias{eco_data_structure}
\title{Detect the likely observational structure of a dataset}
\usage{eco_data_structure(data)}
\arguments{\item{data}{A data.frame.}}
\value{A list containing the suggested structure, confidence, suggested time variable, reason, temporal candidates, and panel_candidates when plausible individual-period pairs are found.}
\description{Conservatively detects reliable temporal-index candidates without treating ambiguous date strings or arbitrary numeric codes as dates. Name-based panel candidates require repeated individuals and unique individual-period pairs; both indexes must still be selected explicitly. Detection is advisory and remains subject to explicit researcher confirmation.}
