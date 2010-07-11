\name{greatLakes}
\alias{greatLakes}
\docType{data}
\title{
Yearly averages of Great Lake heights: 1918 - 2009
}
\description{
  Heights, stored as a multivariate time series, are for the lakes Erie,
  Michigan/Huron, Ontario and St Clair
}
\usage{data(greatLakes)}
\format{
  The format is:
 mts [1:92, 1:4] 174 174 174 174 174 ...
 - attr(*, "dimnames")=List of 2
  ..$ : NULL
  ..$ : chr [1:4] "Erie" "michHuron" "Ontario" "StClair"
 - attr(*, "tsp")= num [1:3] 1918 2009 1
 - attr(*, "class")= chr [1:2] "mts" "ts"
}
\details{
For more details, go to the website that is the source of the data.
}
\source{
  \url{http://www.lre.usace.army.mil/greatlakes/hh/greatlakeswaterlevels/historicdata/}
}
\examples{
data(greatLakes)
plot(greatLakes)
## maybe str(greatLakes) 
}
\keyword{datasets}
