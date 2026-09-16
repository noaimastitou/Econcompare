\name{eco_run}
\alias{eco_run}
\title{Estimate and collect cross-sectional econometric models}
\usage{
eco_run(data, formula, models = "ols", model_args = list(),
  iv_formula = NULL, fixest_formula = NULL, vcov = NULL,
  selection_formula = NULL, outcome_formula = NULL,
  binary_event = NULL,
  error_policy = c("stop", "collect"))
}
\arguments{
\item{data}{A data.frame.}
\item{formula}{Main model formula.}
\item{models}{Character vector of model engines. See \code{eco_models()}. OLS is not added automatically. Models must belong to one outcome group.}
\item{model_args}{Named list of engine-specific arguments. For quantile regression, \code{se} and optional \code{summary_args} configure \code{summary.rq()} inference.}
\item{iv_formula}{Optional instrumental-variable formula.}
\item{fixest_formula}{Optional formula passed to \code{fixest::feols()}.}
\item{vcov}{Optional covariance specification passed to \code{fixest::feols()}.}
\item{selection_formula}{Selection equation for the Heckman model.}
\item{outcome_formula}{Outcome equation for the Heckman model. Defaults to \code{formula}.}
\item{binary_event}{For a character binary response, the category that should be coded as event = 1. Factor responses use their explicit level order unless this argument is supplied.}
\item{error_policy}{\code{"stop"} stops at the first estimator failure. \code{"collect"} retains successful models and records failures.}
}
\value{An object of class \code{econcompare}, including fitted models, outcome group, metadata, warnings, failures and sample information.}
\description{Fits selected cross-sectional models through a common interface without imposing OLS as a reference. Binary, nominal, ordinal and continuous engines are kept in separate comparison groups.}
