\name{eco_var_lag_selection}
\alias{eco_var_lag_selection}
\title{Compare candidate VAR lag orders}
\usage{
eco_var_lag_selection(data, variables, time, lag_max = 8L,
  deterministic = c("const", "trend", "both", "none"))
}
\arguments{
\item{data}{A data.frame.}
\item{variables}{Numeric endogenous system variables.}
\item{time}{Time-index column.}
\item{lag_max}{Maximum lag order considered.}
\item{deterministic}{Deterministic terms passed to \code{vars::VARselect}.}
}
\value{A list with information-criterion values, package-reported minima and an interpretation note.}
\description{Provides lag-selection evidence without selecting a VAR lag order automatically.}
