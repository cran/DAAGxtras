\name{worldRecords}
\alias{worldRecords}
\docType{data}
\title{Record times for track and road races, at August 9th 2006}
\usage{data(worldRecords)}
\description{Record times for track and road races, at August 9th 2006}
\format{
  A data frame with 40 observations on the following 9 variables.
  \describe{
    \item{\code{Distance}}{distance in kilometers}
    \item{\code{roadORtrack}}{a factor with levels \code{road} \code{track}}
    \item{\code{Place}}{place; a character vector}
    \item{\code{Time}}{time in minutes}
    \item{\code{Date}}{a Date}
  }
}
\details{
  For further details, and some additional details, see the web site
  that is the source of the data.
}
\source{
  \url{http://www.gbrathletics.com/wrec.htm}
}
\examples{
data(worldRecords)
library(lattice)
xyplot(log(Time) ~ log(Distance), groups=roadORtrack, data=worldRecords)
xyplot(log(Time) ~ log(Distance), groups=roadORtrack, data=worldRecords,
       type=c("p","r"))
xyplot(log(Time) ~ log(Distance), groups=roadORtrack, data=worldRecords,
       type=c("p","smooth"))
}
\keyword{datasets}
