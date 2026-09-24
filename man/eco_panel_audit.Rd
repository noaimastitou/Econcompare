\name{eco_panel_audit}
\alias{eco_panel_audit}
\title{Audit explicitly selected panel indexes}
\usage{eco_panel_audit(data, id, time)}
\arguments{
\item{data}{A non-empty data.frame.}
\item{id}{Individual identifier column name.}
\item{time}{Period column name: integer periods, Date, or unambiguous year, year-month or year-quarter labels.}
}
\value{A list of summary, by_individual, issues and index tables. Row references are original row positions. The summary includes index_valid, independently of identical observed schedules (balanced). first_time and last_time preserve readable input time labels. Period coordinates are numeric engine coordinates, not display dates.}
\description{Reports balance, duplicated pairs, missing indexes, individual observation counts, singleton individuals and internal calendar gaps without changing the data.}
\details{Balanced means identical observed period schedules, which does not imply a gap-free calendar. Unknown calendar cadence produces NA gap counts. Numeric time values are interpreted as integer period coordinates; use calendar labels for monthly or quarterly data. Frequency inferred from Date values should be checked substantively. The audit does not test attrition or missing-at-random assumptions.}
\examples{
d <- data.frame(id = rep(c("a", "b"), each = 3), year = rep(2020:2022, 2))
eco_panel_audit(d, "id", "year")
}
