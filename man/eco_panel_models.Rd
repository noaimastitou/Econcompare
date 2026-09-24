\name{eco_panel_models}
\alias{eco_panel_models}
\title{Available static panel estimators}
\usage{eco_panel_models()}
\value{A data.frame with engine, estimator, model/effect, interpretation, package, outcome_type and availability.}
\description{Lists panel_pooling, panel_fe_individual, panel_fe_time, panel_fe_twoways, panel_re, panel_fd, panel_between panel_mundlak, panel_clogit, panel_poisson and panel_fe_iv. The panel API requires plm for linear models. Explicit differences and means are estimated with stats::lm; their robust covariance additionally requires sandwich.}
\examples{eco_panel_models()}
