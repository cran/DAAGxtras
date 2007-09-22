\name{nihills}
\alias{nihills}
\docType{data}
\title{Record times for Northern Ireland mountain running}
\description{
  Data are taken from the 2007 calendar for the Northern Ireland
  Mountain Running Association.
}
\usage{data(nihills)}
\format{
  A data frame with 23 observations on the following 4 variables.
  \describe{
    \item{\code{dist}}{distances in miles}
    \item{\code{climb}}{amount of climb in feet}
    \item{\code{time}}{record time for males}
    \item{\code{timef}}{record time for females}
  }
}
\details{These data make an interesting comparison with the dataset
  \code{hills2000} in the DAAG package.}
\source{
  \url{http://www.nimra.org.uk/calendar.asp}
}
\examples{
data(nihills)
lm(formula = log(time) ~ log(dist) + log(climb), data = nihills)
lm(formula = log(time) ~ log(dist) + log(climb/dist), data = nihills)
}
\keyword{datasets}
