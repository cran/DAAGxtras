\name{hotspots}
\alias{hotspots}
\docType{data}
\title{Hawaian island chain hotspot Potassium-Argon ages}
\description{
  K-Ar Ages (millions of years) and distances (km) from Kilauea 
  along the trend of the chain of Hawaian volcanic islands and
  other seamounts that are believed to have been created by a
  moving "hot spot". The age of Kilauea is given as 0-0.4 Ma.
}
\usage{data(hotspots)}
\format{
  A data frame with 36 observations on the following 6 variables.
  \describe{
    \item{\code{ID}}{Volcano identifier}
    \item{\code{name}}{Name}
    \item{\code{distance}}{Distance in kilometers}
    \item{\code{age}}{K-Ar age in millions of years}
    \item{\code{error}}{Standard error of estimate?}
    \item{\code{source}}{Data source; see information on web site below.}
  }
}
\details{
  For details of the way that errors werre calculated, refer to the
  original papers. See also the comments under \code{hotspots2006}.
  In general, errors do not account for geological uncertainty.
}
\source{
\url{http://www.soest.hawaii.edu/GG/HCV/haw_formation.html}
}
\examples{
data(hotspots)
plot(age ~ distance, data=hotspots)
abline(lm(age ~ distance, data=hotspots))
}
\keyword{datasets}
