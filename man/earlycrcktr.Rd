\name{earlycrcktr}
\alias{earlycrcktr}
\docType{data}
\title{Lifespans of UK 1st class cricketers born prior to 1840}
\description{
  Year and birth, lifespan, etc, of British first class cricketers,
  born prior to 1840, whose handedness could be determined.
}
\usage{data(earlycrcktr)}
\format{
  A data frame with 211 observations on the following 8 variables.
  \describe{
    \item{\code{left}}{a factor with levels \code{right} \code{left}}
    \item{\code{year}}{numeric, year of birth}
    \item{\code{life}}{numeric, lifespan}
    \item{\code{dead}}{numeric (all 1 = dead)}
    \item{\code{acd}}{numeric (0 = not accidental or not dead, 1
     = accidental death)}
    \item{\code{kia}}{numeric (all 0 = not killed in action)}
    \item{\code{inbed}}{numeric (0 = did not die in bed, 1
     = died in bed)}
   \item{\code{cause}}{a factor with levels \code{alive} \code{acd}
     (accidental death) \code{inbed} (died in bed)}
  }
}
\source{
  John Aggleton, Martin Bland. Data were collated as described in
Aggleton et al.  
}
\references{
  Aggleton JP, Bland JM, Kentridge RW, Neave NJ 1994.  Handedness and
  longevity: an archival study of cricketers.  British Medical Journal
  309, 1681-1684.

  Bailey P, Thorne P, Wynne-Thomas P. 1993. Who's Who of Cricketers.
  2nd ed, London, Hamlyn.
  
  Bland M and Altman D. 2005. Do the left-handed die young?
  Significance 2, 166-170. 
}
\seealso{\code{cricketer}.}
\examples{
data(earlycrcktr)
}
\keyword{datasets}
