\name{aulatlong}
\alias{aulatlong}
\docType{data}
\title{Latitudes and longitudes for ten Australian cities}
\description{Latitudes and longitudes for
Adelaide, Alice, Brisbane, Broome, Cairns, Canberra, 
Darwin, Melbourne, Perth and Sydney; i.e., for the cities to which the
road distances in \code{audists} relate.
}
\usage{aulatlong}
\format{
  A data frame with 10 observations on the following 2 variables.
  \describe{
    \item{\code{latitude}}{Latitude, as a decimal number}
    \item{\code{longitude}}{Latitude, as a decimal number}
  }
  }
\source{
Map of Australia showing latitude and longitude information.
}
\examples{
data(aulatlong)
## maybe str(aulatlong) ; plot(aulatlong) ...
}
\keyword{datasets}
