% --- Source file: SOI.Rd ---
\name{SOI}
\alias{SOI}
\title{Southern Oscillation Index Data}
\usage{SOI}
\description{
The Southern Oscillation Index (SOI) is the difference in barometric
pressure at sea level between Tahiti and Darwin.  Monthly and annual SOI
data, for the years 1876-2011, are given.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{Year}{a numeric vector}
\item{Jan}{average January SOI values for each year}
\item{Feb}{average February SOI values for each year}
\item{Mar}{average March SOI values for each year}
\item{Apr}{average April SOI values for each year}
\item{May}{average May SOI values for each year}
\item{Jun}{average June SOI values for each year}
\item{Jul}{average July SOI values for each year}
\item{Aug}{average August SOI values for each year}
\item{Sep}{average September SOI values for each year}
\item{Oct}{average October SOI values for each year}
\item{Nov}{average November SOI values for each year}
\item{Dec}{average December SOI values for each year}
\item{avsoi}{average annual SOI values}
  }
}
\source{
Australian Bureau of Meteorology web pages:

\url{http://www.bom.gov.au/climate/current/soihtm1.shtml}
}
  \references{
Nicholls, N., Lavery, B., Frederiksen, C.\ and Drosdowsky, W. 1996.
Recent apparent changes in relationships between the El Nino --
southern oscillation and Australian rainfall and temperature.
Geophysical Research Letters 23: 3357-3360.
}
  \examples{
plot(ts(SOI[, "avsoi"], start=1900),
     panel=function(y,...)panel.smooth(1900:2008, y,...))
}
\keyword{datasets}
