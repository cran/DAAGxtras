\name{zzDAAGxdb}
\alias{zzDAAGxdb}
\docType{data}
\title{List, each of whose elements hold rows of a file, in character format}
\description{
  This is the default alternative database for use with the function
  \code{datafile}, which uses elements of this list to place files
  in the working directory. The names of the list elements are
  bestTimes and bostonc.
}
\usage{data(zzDAAGxdb)}
\format{
  Successive elements in this list hold character vectors from which the
  corresponding files can be readily generated.  
}
\examples{
data(zzDAAGxdb)
names(zzDAAGxdb)
}
\keyword{datasets}
