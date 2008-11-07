\name{simulateSampDist}
\alias{simulateSampDist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simulated sampling distribution of mean or other statistic}
\description{
  Simulates the sample distribution of the specified statistic,
  for samples of the size(s) specified in \code{numINsamp}.
  Additionally a with replacement) sample is drawn from the
  specified population.
}
\usage{
simulateSampDist(rpop = rnorm, numsamp = 100, numINsamp = c(4, 16),
                 FUN = mean, seed=NULL
      )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rpop}{Either a function that generates random samples from
    the specified distribution, or a vector of values that define the
    population (i.e., an empirical distribution)}
  \item{numsamp}{Number of samples that should be taken.  For close
    approximation of the asymptotic distribution (e.g., for the mean)
    this number should be large}
  \item{numINsamp}{Size(s) of each of the \code{numsamp} sample(s)}
  \item{FUN}{Function to calculate the statistic whose sampling
    distribution is to be simulated}
  \item{seed}{Optional seed for random number generation}
}
\value{
List, with elements \code{values}, \code{numINsamp} and \code{FUN}
\item{values}{Matrix, with dimensions \code{numsamp} by
  \code{numINsamp + 1}.  The first column has a random with
  replacement sample from the population, while the remaining
  \code{length(numINsamp)} columns hold simulated values from sampling
  distributions with samples of the specified size(s)}
\item{numINsamp}{Input value of \code{numINsamp}}
\item{numsamp}{Input value of \code{numsamp}}
}
\references{Maindonald, J.H. and Braun, W.J. (2nd edn, 2006) \emph{Data
               Analysis and Graphics Using R}, 2nd edn, Section 4.1}
\author{John Maindonald}
\seealso{\code{help(plotSampDist)}}
\examples{
## By default, sample from normal population
simAvs <- simulateSampDist()
par(pty="s")
plotSampDist(simAvs)
## Sample from empirical distribution
simAvs <- simulateSampDist(rpop=rivers)
plotSampDist(simAvs)


## The function is currently defined as
function(rpop=rnorm, numsamp=100, numINsamp=c(4,16), FUN=mean,
seed=NULL){
    if(!is.null(seed))set.seed(seed)
    funtxt <- deparse(substitute(FUN))
    nDists <- length(numINsamp)+1
    values <- matrix(0, nrow=numsamp, ncol=nDists)
    if(!is.function(rpop)) {
      x <- rpop
      rpop <- function(n)sample(x, n, replace=TRUE)
    }
    values[,1] <- rpop(numsamp)
    for(j in 2:nDists){
      n <- numINsamp[j-1]
      for(i in 1:numsamp)values[i, j] <- FUN(rpop(n))
    }
    colnames(values) <- paste("Size", c(1, numINsamp))
    invisible(list(values=values, numINsamp=numINsamp, FUN=funtxt))
  }
}
\keyword{datagen}
\keyword{distribution}
