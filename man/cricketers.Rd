\name{cricketers}
\alias{cricketers}
\docType{data}
\title{Lifespans of cricketers}
\description{
  Data were derived from the Who's who of cricketers, with year of birth
  up to 1960, lifespan or lifetime up to 1992.
}
\usage{data(cricketers)}
\format{
  A data frame with 6172 observations on the following 8 variables.
  \describe{
    \item{\code{left}}{a factor with levels \code{right} \code{left}}
    \item{\code{year}}{numeric, year of birht}
    \item{\code{life}}{numeric, lifetime or lifespan to 1992}
    \item{\code{dead}}{numeric (0 = alive, 1 = dead, by 1992)}
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
John Aggleton, Martin Bland.
}
\references{
  Bland M and Altman D. 2005. Do the left-handed die young?
  Significance 2, 166-170. 
  
  Aggleton JP, Bland JM, Kentridge RW, Neave NJ 1994.  Handedness and
  longevity: an archival study of cricketers.  British Medical Journal
  309, 1681-1684. 
}
\examples{
data(cricketers)
cricket <- subset(cricketers, year>=1840 & year <= 1960)
numLH <- xtabs(~ left+year, data=cricket)
propLH <- prop.table(numLH, margin=2)[2,]
yr <- as.numeric(colnames(numLH))
plot(propLH ~ yr)
cricket$lh <- unclass(cricket$left)-1
left2.hat <- fitted(lm(lh ~ poly(year,2), data=cricket))
ord <- order(cricket$year)
lines(left2.hat[ord] ~ cricket$year[ord])
library(splines)
ns3.hat <- fitted(lm(lh ~ ns(year,3), data=cricket))
lines(ns3.hat[ord] ~ cricket$year[ord], col="red")
library(survival)
summary(coxph(Surv(life, kia) ~ bs(year,3) +left, data=cricket))
cricket$notacdDead <- with(cricket, {dead[acd==1]<-0; dead})
summary(coxph(Surv(life, notacdDead) ~ ns(year,2) +left, data=cricket))
}
\keyword{datasets}
