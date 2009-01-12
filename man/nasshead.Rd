\name{nasshead}
\alias{nasshead}
\docType{data}
\title{Documentation of names of columns in nass9702cor}
\description{
  \code{SASname} and \code{longname} are from the SAS XPT file
  nass9702cor.XPT that is available from the webite noted below.
  The name \code{shortname} is the name used in the data frame
  \code{nass9702cor}, not included in this package, but available
  from my website that is noted below.  It is also used in
  \code{nassCDS}, for columns that \code{nassCDS} includes.
}
\usage{data(nasshead)}
\format{
  A data frame with 56 observations on the following 3 variables.
  \describe{
    \item{\code{shortname}}{a character vector}
    \item{\code{SASname}}{a character vector}
    \item{\code{longname}}{a character vector}
  }
}
\details{
For full details of the coding of values in columns of
\code{nass9702cor}, consult one of the SAS format files that
can be obtained by following the instructions on Dr Meyer's web site
that is noted below.
}
\source{
  \url{http://www.stat.uga.edu/~mmeyer/airbags.htm}\\
  \url{ftp://ftp.nhtsa.dot.gov/nass/}\\
  Click, e.g., on 1997 and then on SASformats.
See also
\url{http://www.maths.anu.edu.au/~johnm/datasets/airbags}
}  
\references{
  Meyer, M.C. and Finney, T. (2005): \emph{Who wants airbags?}. Chance 
  18:3-16.

  Farmer, C.H. 2006. \emph{Another look at Meyer and Finney's `Who wants
    airbags?'}. Chance 19:15-22.

  Meyer, M.C. 2006.  \emph{Commentary on "Another look at Meyer and
    Finney's `Who wants airbags?'"}. Chance 19:23-24.
}
\examples{
data(nasshead)
}
\keyword{datasets}
