\name{edcT}
\alias{edcT}
\docType{data}
\title{EPICA Dome C Ice Core 800KYr Temperature Estimates}
\description{
  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(edcT)}
\format{
  A data frame with 5788 observations on the following 5 variables.
  \describe{
    \item{\code{Bag}}{Bag number}
    \item{\code{ztop}}{Top depth (m)}
    \item{\code{Age}}{Years before 1950}
    \item{\code{Deuterium}}{Deuterium dD data}
    \item{\code{dT}}{Temperature difference from the average of the last
    1000 years ~ -54.5degC}
  }
}
\details{
  Temperature was estimated from the deuterium data, after making various
  corrections.
}
\source{
\url{http://www.ncdc.noaa.gov/paleo/icecore/antarctica/domec/domec_epica_data.html}
}
\references{
Jouzel, J., et al.  2007.
EPICA Dome C Ice Core 800KYr Deuterium Data and Temperature Estimates. 
IGBP PAGES/World Data Center for Paleoclimatology 
Data Contribution Series \# 2007-091.
NOAA/NCDC Paleoclimatology Program, Boulder CO, USA.

Jouzel, J., et al.  2007.  Orbital and Millennial Antarctic Climate
Variability over the Past 800,000 Years.  Science, Vol. 317, No. 5839,
pp.793-797, 10 August 2007.  }
\examples{ data(edcT) }
\keyword{datasets}
