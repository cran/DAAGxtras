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
  A data frame with 26217 observations on the following 14 variables.
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
     \item{\code{yearVeh}}{Year of model of vehicle; a numeric vector} 
     \item{\code{abcat}}{Did one or more (driver or passenger) airbag(s)
       deploy? This factor has levels \code{deploy} \code{nodeploy}
       \code{unavail}}
    \item{\code{occRole}}{a factor with levels \code{driver} \code{pass}}
    \item{\code{deploy}}{a numeric vector: 0 if an airbag was
      unavailable or did not deploy; 1 if one or more bags deployed.}
    \item{\code{injSeverity}}{a numeric vector; 0:none, 1:possible injury,
      2:no incapacity, 3:incapacity, 4:killed; 5:unknown, 6:prior death}   
  }
  }
\details{
  Data were collected according to a multi-stage sampling scheme.
  Hence the need to weight observations by the inverse of the
  selection probability, called the national inflation factor
  (\code{national}) in the original data from NASS.  These data include
  a subset of the variables from the nass9702cor dataset.  Variables
  that are coded here as factors are coded as numeric values in that
  dataset. 
}

\source{
  \url{http://www.stat.uga.edu/~mmeyer/airbags.htm}
  
  \url{ftp://ftp.nhtsa.dot.gov/nass/}
  
See also
\url{http://www.maths.anu.edu.au/~johnm/datasets/airbags}
}
\references{
  Meyer, M.C. and Finney, T. (2005): \emph{Who wants airbags?}. Chance 
  18:3-16.

  Farmer, C.H. 2006. \emph{Another look at Meyer and Finney's \sQuote{Who wants
    airbags?}}. Chance 19:15-22.

  Meyer, M.C. 2006.  \emph{Commentary on "Another look at Meyer and
    Finney's 'Who wants airbags?'"}. Chance 19:23-24.
}
\examples{
data(nassCDS)
xtabs(weight ~ dead + airbag, data=nassCDS)
xtabs(weight ~ dead + airbag + seatbelt + dvcat, data=nassCDS)
tab <- xtabs(weight ~ dead + abcat, data=nassCDS,
             subset=dvcat=="25-39"&frontal==0)[, c(3,1,2)]
round(tab[2, ]/apply(tab,2,sum)*100,2)
}
\keyword{datasets}
