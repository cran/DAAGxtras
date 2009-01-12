\name{audists}
\alias{audists}
\docType{data}
\title{Road distances between 10 Australian cities}
\description{
  Distances between the Australian cities of
Adelaide, Alice, Brisbane, Broome, Cairns, Canberra, 
Darwin, Melbourne, Perth and Sydney   
}
\usage{audists}
\format{
  The format is:
Class 'dist', i.e., a distance matrix.
}
\source{
Australian road map
}
\examples{
data(audists)
library(lattice)
audists.cmd <- cmdscale(audists)
xyplot(audists.cmd[,2] ~ audists.cmd[,1], groups=row.names(audists.cmd),
       panel = function(x, y, subscripts, groups)  
                        ltext(x = x, y = y, label = groups[subscripts],
                        cex=1, fontfamily = "HersheySans"))
}
\keyword{datasets}
