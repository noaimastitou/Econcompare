\name{eco_stationarity_tests}
\alias{eco_stationarity_tests}
\title{Run complementary stationarity diagnostics}
\usage{
eco_stationarity_tests(data, variables, time, adf_k = NULL,
  adf_deterministic = c("drift", "none", "trend"),
  kpss_null = c("Level", "Trend"))
}
\arguments{
\item{data}{A data.frame.}
\item{variables}{Names of the numeric series to test. Only explicitly supplied series are tested.}
\item{time}{Name of the validated time index.}
\item{adf_k}{Optional fixed non-negative ADF lag order. If NULL, econcompare uses \code{floor((n - 1)^(1/3))} for the tested series and reports the chosen value.}
\item{adf_deterministic}{ADF deterministic specification: \code{"none"}, \code{"drift"} (intercept), or \code{"trend"} (intercept plus linear trend).}
\item{kpss_null}{KPSS null hypothesis: level or trend stationarity.}
}
\value{A data.frame containing ADF and KPSS statistics. ADF critical values are reported explicitly; ADF p-values are left \code{NA} rather than approximated silently.}
\description{Runs complementary stationarity diagnostics without automatically transforming or differencing data. A complete regular time grid and no internal missing observations are required. ADF uses \pkg{urca} so the deterministic component is an explicit researcher choice; KPSS uses \pkg{tseries}.}
