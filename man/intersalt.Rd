\name{intersalt}
\alias{intersalt}
\docType{data}
\title{Blood pressure versus Salt; inter-population data}
\description{
  Median blood pressure, as a fuction of salt intake, for each of 52
  human populations.
}
\usage{intersalt}
\format{
  A data frame with 52 observations on the following 4 variables.
  \describe{
    \item{\code{b}}{a numeric vector}
    \item{\code{bp}}{mean diastolic blood pressure (mm Hg)}
    \item{\code{na}}{mean sodium excretion (mmol/24h)}
    \item{\code{country}}{a character vector}
  }
}
\details{
  For each population took a sample of 25 males and 25 females from each
  decade in the age range 20 - 50, i.e. 200 individuals in all.
}
\source{
  Intersalt Cooperative Research Group. 1988.  Intersalt: an international
  study of electrolyte excretion and blood pressure: results for 24 hour
  urinary sodium and potassium excretion.  \emph{British Medical Journal}
  297: 319-328.  
}
\references{
Maindonald, J.H. \emph{The Design of Research Studies ? A Statistical
Perspective}, viii + 109pp. Graduate School Occasional Paper 00/2, 
Australian National University 2000.
}
\examples{
data(intersalt)
plot(bp ~ na, data=intersalt, xlab="Median sodium excretion (mmol/24h)",
     ylab="Median diatoluc blood pressure (mm Hg)")
}
\keyword{datasets}
