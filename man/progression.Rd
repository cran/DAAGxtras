\name{progression}
\alias{progression}
\docType{data}
\title{Progression of Record times for track races, 1912 - 2008}
\usage{data(progression)}
\description{Progression in world record times for track and road races.}
\format{
  A data frame with 227 observations on the following 4 columns.
  \describe{
    \item{\code{year}}{Year that time was first recorded}
    \item{\code{Distance}}{distance in kilometers}
    \item{\code{Time}}{time in minutes}
    \item{\code{race}}{character; descriptor for event (100m, mile, ...)}
  }
}
\details{  
Record times for men's track events, from 1912 onwards.  The
series starts with times that were recognized as record times in 1912,
where available.
}
\source{
Links to sources for the data are at

\url{http://en.wikipedia.org/wiki/Athletics_world_record}
}
\examples{
data(progression)
plot(log(Time) ~ log(Distance), data=progression)
library(lattice)
xyplot(log(Time) ~ log(Distance), data=progression, type=c("p","r"))
xyplot(log(Time) ~ log(Distance), data=progression,
       type=c("p","smooth"))
res <- resid(lm(log(Time) ~ log(Distance), data=progression))
plot(res ~ log(Distance), data=progression,
     ylab="Residuals from regression line on log scales") 
}
\keyword{datasets}
