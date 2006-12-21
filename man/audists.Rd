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
Class 'dist'  atomic [1:45] 1690 2130 4035 2865 1210 ...
  ..- attr(*, "Labels")= chr [1:10] "Adelaide" "Alice" "Brisbane" "Broome" ...
  ..- attr(*, "Size")= int 10
  ..- attr(*, "call")= language as.dist.default(m = audists)
  ..- attr(*, "Diag")= logi FALSE
  ..- attr(*, "Upper")= logi FALSE
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
