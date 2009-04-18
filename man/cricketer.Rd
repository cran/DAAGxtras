\name{cricketer}
\alias{cricketer}
\docType{data}
\title{Lifespans of UK 1st class cricketers born 1840-1960}
\description{
  Year and birth, lifespan, etc, of British first class cricketers,
  born 1840-1960, whose handedness could be determined from
  information in the Who's who of cricketers.  The status
  (alive=0, dead =1), and lifetime or lifespan, is for 1992.
}
\usage{data(cricketer)}
\format{
  A data frame with 5960 observations on the following 8 variables.
  \describe{
    \item{\code{left}}{a factor with levels \code{right} \code{left}}
    \item{\code{year}}{numeric, year of birth}
    \item{\code{life}}{numeric, lifetime or lifespan to 1992}
    \item{\code{dead}}{numeric (0 = alive (censored), 1 = dead, in 1992)}
    \item{\code{acd}}{numeric (0 = not accidental or not dead, 1
     = accidental death)}
    \item{\code{kia}}{numeric (0 = not killed in action, 1
     = killed in action)}
    \item{\code{inbed}}{numeric (0 = did not die in bed, 1
     = died in bed)}
   \item{\code{cause}}{a factor with levels \code{alive} \code{acd}
     (accidental death) \code{inbed} (died in bed)}
  }
}
\details{
  Note that those 'killed in action' (mostly during World Wars I and II)
  form a subset of those who died by accident.
}
\source{
John Aggleton, Martin Bland.  Data were collated as described in
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
\seealso{\code{earlycrcktr}.}
\examples{
data(cricketer)
numLH <- xtabs(~ left+year, data=cricketer)
propLH <- prop.table(numLH, margin=2)[2,]
yr <- as.numeric(colnames(numLH))
plot(propLH ~ yr)
cricketer$lh <- unclass(cricketer$left)-1
left2.hat <- fitted(lm(lh ~ poly(year,2), data=cricketer))
ord <- order(cricketer$year)
lines(left2.hat[ord] ~ cricketer$year[ord])
library(splines)
ns3.hat <- fitted(lm(lh ~ ns(year,3), data=cricketer))
lines(ns3.hat[ord] ~ cricketer$year[ord], col="red")
library(survival)
summary(coxph(Surv(life, kia) ~ bs(year,3) +left, data=cricketer))
cricketer$notacdDead <- with(cricketer, {dead[acd==1]<-0; dead})
summary(coxph(Surv(life, notacdDead) ~ ns(year,2) +left, data=cricketer))
}
\keyword{datasets}
