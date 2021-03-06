\name{covtest}
\alias{covtest}
\docType{data}
\title{Sample of UCI Machine Learning Forest Cover Dataset}
\description{
  Dataset used as test data in the study cited below. These are
  observations 11341 to 15120, out of 581012, in the dataset on
  the UCI site.  Forest cover type is recorded, together with
  information on physical geographical variables that may account for
  the forest cover type.
}
\usage{data(covtest)}
\format{
  A data frame with 11318 observations on the following 55 variables.
  \describe{
     \item{\code{V1}}{a numeric vector}
    \item{\code{V2}}{a numeric vector}
    \item{\code{V3}}{a numeric vector}
    \item{\code{V4}}{a numeric vector}
    \item{\code{V5}}{a numeric vector}
    \item{\code{V6}}{a numeric vector}
    \item{\code{V7}}{a numeric vector}
    \item{\code{V8}}{a numeric vector}
    \item{\code{V9}}{a numeric vector}
    \item{\code{V10}}{a numeric vector}
    \item{\code{V11}}{a numeric vector}
    \item{\code{V12}}{a numeric vector}
    \item{\code{V13}}{a numeric vector}
    \item{\code{V14}}{a numeric vector}
    \item{\code{V15}}{a numeric vector}
    \item{\code{V16}}{a numeric vector}
    \item{\code{V17}}{a numeric vector}
    \item{\code{V18}}{a numeric vector}
    \item{\code{V19}}{a numeric vector}
    \item{\code{V20}}{a numeric vector}
    \item{\code{V21}}{a numeric vector}
    \item{\code{V22}}{a numeric vector}
    \item{\code{V23}}{a numeric vector}
    \item{\code{V24}}{a numeric vector}
    \item{\code{V25}}{a numeric vector}
    \item{\code{V26}}{a numeric vector}
    \item{\code{V27}}{a numeric vector}
    \item{\code{V28}}{a numeric vector}
    \item{\code{V29}}{a numeric vector}
    \item{\code{V30}}{a numeric vector}
    \item{\code{V31}}{a numeric vector}
    \item{\code{V32}}{a numeric vector}
    \item{\code{V33}}{a numeric vector}
    \item{\code{V34}}{a numeric vector}
    \item{\code{V35}}{a numeric vector}
    \item{\code{V36}}{a numeric vector}
    \item{\code{V37}}{a numeric vector}
    \item{\code{V38}}{a numeric vector}
    \item{\code{V39}}{a numeric vector}
    \item{\code{V40}}{a numeric vector}
    \item{\code{V41}}{a numeric vector}
    \item{\code{V42}}{a numeric vector}
    \item{\code{V43}}{a numeric vector}
    \item{\code{V44}}{a numeric vector}
    \item{\code{V45}}{a numeric vector}
    \item{\code{V46}}{a numeric vector}
    \item{\code{V47}}{a numeric vector}
    \item{\code{V48}}{a numeric vector}
    \item{\code{V49}}{a numeric vector}
    \item{\code{V50}}{a numeric vector}
    \item{\code{V51}}{a numeric vector}
    \item{\code{V52}}{a numeric vector}
    \item{\code{V53}}{a numeric vector}
    \item{\code{V54}}{a numeric vector}
    \item{\code{V55}}{a numeric vector}   
  }
  For details, see
  \url{http://kdd.ics.uci.edu/databases/covertype/covertype.data.html}
}
\details{For further details, see
    \url{http://kdd.ics.uci.edu/databases/covertype/covertype.data.html}.
  Note the omission of any information on geographical location.
  Distance through the data seems however to be, in part, a proxy
  for geographical location. 
}
\source{
  \url{http://kdd.ics.uci.edu/databases/covertype/covertype.html}
}
\references{
  Blackard, Jock A. 1998. "Comparison of Neural Networks and Discriminant
  Analysis in Predicting Forest Cover Types." Ph.D. dissertation.
  Department of Forest Sciences. Colorado State University. Fort
  Collins, Colorado.
}
\examples{
data(covtest)
}
\keyword{datasets}
