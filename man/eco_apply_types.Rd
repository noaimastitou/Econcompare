\name{eco_apply_types}
\alias{eco_apply_types}
\title{Apply explicit variable-type overrides}
\usage{
eco_apply_types(data, overrides, ordinal_levels = NULL)
}
\arguments{
\item{data}{A data.frame.}
\item{overrides}{A named character vector or named list. Names must be data columns and values one of \code{auto}, \code{continuous}, \code{binary}, \code{nominal}, \code{ordinal}, \code{time_auto}, \code{time_year}, \code{time_year_month}, \code{time_year_quarter}, or \code{time_date_iso}.}
\item{ordinal_levels}{Optional named list giving an explicit lowest-to-highest category order for every variable manually typed as \code{ordinal}. Manual ordinal typing never infers rank from labels, factor order, numeric codes, or row order.}
}
\value{A copy of \code{data} with validated conversions. Attributes record the active type overrides and any explicit ordinal levels.}
\description{Lets researchers correct conservative or unsuitable automatic type suggestions before modelling while keeping all substantive ordinal ordering decisions explicit.}
