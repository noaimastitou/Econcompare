\name{eco_system_models}
\alias{eco_system_models}
\title{List multivariate time-series system engines}
\usage{
eco_system_models()
}
\value{
A data.frame describing VAR and VECM engines and whether their optional packages are available locally.
}
\description{
Lists the multivariate dynamic-system engines available in econcompare. VAR and VECM are kept separate from single-equation temporal regressions because all selected series are treated as endogenous within the system.
}
