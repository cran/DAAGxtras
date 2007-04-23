\name{hotspots2006}
\alias{hotspots2006}
\docType{data}
\title{Hawaian island chain hotspot Argon-Argon ages}
\description{
  Ar-Ar Ages (millions of years) and distances (km) from Kilauea 
  along the trend of the chain of Hawaian volcanic islands and
  other seamounts that are believed to have been created by a
  moving "hot spot". 
}
\usage{data(hotspots2006)}
\format{
  A data frame with 10 observations on the following 6 variables.
  \describe{
    \item{\code{age}}{Ar-Ar age}
    \item{\code{CI95lim}}{Measurement error; 95\% CI}
    \item{\code{geoErr}}{Geological Uncertainty}
    \item{\code{totplus}}{Total uncertainty (+)}
    \item{\code{totminus}}{Total uncertainty (-)}
    \item{\code{distance}}{Distance in kilometers}
  }
}
\details{
  Note that measurement error is small relative to geological
  uncertainty. Geological uncertainty arises because lavas are
  likely to have erupted, over a period of up to 2 million years,
  somewhat after passage over the hot spot's centre.  Dredging
  or drilling will in general have accessed larvas from the
  younger half of this interval.  Hence the asymmetry in the
  geological uncertainty.
}
\source{
  Warren D. Sharp and David A. Clague, 50-Ma initiation of
  Hawaiian-Emperor bend records major change in Pacific Plate
  motion. Science 313: 1281-1284 (2006).
}
\examples{
data(hotspots2006)
}
\keyword{datasets}
