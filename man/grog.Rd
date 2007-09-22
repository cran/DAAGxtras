\name{grog}
\alias{grog}
\docType{data}
\title{Alcohol consumption in Australia and New Zealand}
\description{
Data are annual apparent alcohol consumption in  Australia and New
Zealand, in liters of pure alcohol content per annum, separately for
beer, wine, and spirits (including spirit-based products).
}
\usage{data(grog)}
\format{
  A data frame with 18 observations on the following 5 variables.
  \describe{
    \item{\code{Beer}}{liters per annum}
    \item{\code{Wine}}{liters per annum}
    \item{\code{Spirit}}{liters per annum}
    \item{\code{Country}}{a factor with levels \code{Australia}
      \code{NewZealand}}
    \item{\code{Year}}{Year ending in June of the given year}
  }
}
\details{  
  Data are total available pure alcohol content, for the three
  categories, divided by numbers of persons aged 15 years or more.  The
  source data for New Zealand included quarterly figures from December
  1997, and annual data to December for all years.  The annual New
  Zealand figure to June 1998 required an estimate for September
  1997 that was obtained by extrapolating back the third quarter trend
  line from later years.
}
\source{Australian data are from \url{http://www.abs.gov.au}.
  New Zealand data are derived from data from
  \url{http://www.stats.govt.nz/people/health/alcohol.htm}
}
\examples{
data(grog)
library(lattice)
xyplot(Beer+Wine+Spirit ~ Year | Country, data=grog)
xyplot(Beer+Wine+Spirit ~ Year, groups=Country, data=grog, outer=TRUE)
}
\keyword{datasets}
