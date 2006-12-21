\name{nassCDS}
\alias{nassCDS}
\docType{data}
\title{Airbag and other influences on accident fatalities}

\description{US data, for 1997-2002, from police-reported car crashes in
which there is a harmful event (people or property), and from which at
least one vehicle was towed. Data are restricted to front-seat
occupants, include only a subset of the variables recorded, and
are restricted in other ways also.}
\usage{nassCDS}
\format{
  A data frame with 26217 observations on the following 9 variables.
  \describe{
    \item{\code{dvcat}}{ordered factor with levels (estimated impact
      speeds) \code{1-9km/h}, \code{10-24}, \code{25-39}, \code{40-54},
      \code{55+}
    }
    \item{\code{weight}}{Weight that should be attached to observation,
    in order to account for sampling probability in a probabilistic
    sampling scheme}
    \item{\code{dead}}{factor with levels \code{alive} \code{dead}}
    \item{\code{airbag}}{a factor with levels \code{none} \code{airbag}}
    \item{\code{seatbelt}}{a factor with levels \code{none} \code{belted}}
    \item{\code{frontal}}{a numeric vector; 0 = non-frontal, 1=frontal impact}
    \item{\code{sex}}{a factor with levels \code{f} \code{m}}
    \item{\code{ageOFocc}}{age of occupant in years}
    \item{\code{yearacc}}{year of accident}
  }
  }
\details{
  Data were collected according to a multi-stage sampling scheme.
  Hence the need to weight observations by the inverse of the
  selection probability, called the national inflation factor
  (\code{NIF}) in the original data from NASS.  These data include
  only a subset of the variables from the NASS data.}
}
\source{
  \url{http://www.stat.uga.edu/~mmeyer/airbags.htm}\\
See also
\url{http://www.maths.anu.edu.au/~johnm/datasets/airbags}
}
\references{
  Meyer, M.C. and Finney, T. (2005): \emph{Who wants airbags?}. Chance 
  18:3-16.

  Farmer, C.H. 2005. \emph{Another look at Meyer and Finney's `Who wants
    airbags?'}

  \url{http://www.ihs.org}
}
\examples{
data(nassCDS)
xtabs(weight ~ dead + airbag, data=nassCDS)
xtabs(weight ~ dead + airbag + seatbelt + dvcat, data=nassCDS)
}
\keyword{datasets}
