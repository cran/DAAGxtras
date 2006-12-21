\name{fumig}
\alias{fumig}
\docType{data}
\title{Profiles of fumigant concentration over time}
\description{
  Fumigant concentrations are given at six times through a 120 minute
  fumigation, for seven different runs of the fumigation procedure.
}
\usage{fumig}
\format{
  A data frame with 8 observations on the following 8 variables.
  \describe{
    \item{\code{testnam}}{a factor with levels \code{Applied Test 1} \code{Applied Test 2} \code{Applied Test 3} \code{Confirmation Test } \code{Query} \code{Applied Test 4} \code{Applied Test} \code{Applied Test 5} \code{Applied Test 6}}
    \item{\code{Cultivar}}{a factor with levels \code{Bogapple}
      \code{Chewton Pear} (the names are invented)}
    \item{\code{X1}}{concentration (gm/cm$^3$) at 5 minutes}
    \item{\code{X2}}{concentration at 10 min}
    \item{\code{X3}}{concentration at 30 min}
    \item{\code{X4}}{concentration at 60 min}
    \item{\code{X5}}{concentration at 90 min}
    \item{\code{X6}}{concentration at 120 min}
  }
}
\details{
  Sortpion of fumigant by the fruit, different between different
  cultivars, is the main reason for the decline in concentration over time.
}
\source{
John Maindonald
}
\references{
For s discussion of the technology, see:
  
  Maindonald, J.H.; Waddell B.C.; Birtles D.B. 1991. Response to Methyl
  Bromide Fumigation of Codling Moth (Lepidoptera: Tortricidae) on
  Cherries.  \emph{Journal of Economic Entomology} 85: 1222-1230.
}
\examples{
data(fumig)
}
\keyword{datasets}
