\name{MaskedPriming}
\alias{MaskedPriming}
\docType{data}
\title{Masked Repetition Priming Data}
\description{
  Words (\code{words}), preceded by an invisible identical or unrelated
  \dQuote{prime}, or non-words, were flashed in front of subjects (\code{id}).
The time taken by the subject to identify the letter combination
  as \dQuote{not word} or \dQuote{word} was then measured.  
}
\usage{data(MaskedPriming)}
\format{A data frame with 6381 correct responses to words on the
  following 10 variables.
  \describe{
    \item{\code{subjects}}{a factor with levels \code{1} to \code{72}}   
    \item{\code{words}}{a factor with levels \code{1} to \code{192}}
    \item{\code{e}}{the level of familiarity, a factor with levels
      \code{1} \code{2} \code{3}}
    \item{\code{ct}}{a factor with levels \code{HI} \code{HU} \code{LI}
      \code{LU}. Here, \code{HI} = high freq, identical prime;
      HU = high freq, unrelated prime; \code{LI} = low, identical;
LU = low, unrelated}
    \item{\code{f}}{the word frequency, a numeric vector with values -0.5
      (High) and 0.5 (Low)}
    \item{\code{p}}{priming, a numeric vector with values -0.5
      (Identical word) and 0.5 (Unrelated word)}
    \item{\code{rt}}{ reaction time (milliseconds), a numeric vector}
    \item{\code{srt}}{reaction time (sec) = \code{rt}/1000, a numeric vector}
    \item{\code{lrt}}{loge(reaction time), a numeric vector}
    \item{\code{rrt}}{negative of speed of reaction = -1/\code{srt},
      a numeric vector}
  }
}
\details{
  This combines the datasets from Bodner and Masson (1997, Exp 1 and Exp
    2a) and Kinoshita (2006, Exp 2).
}
\source{
  \cite{Kliegl et al (2008)}
}
\references{Bodner, G.E., and Masson, M. E. J. 1997 Masked repetition
priming of words and nonwords: Evidence for a nonlexical basis for
priming. \emph{Journal of Memory and Language} \bold{37}, 268-293.

Kinoshita, S. 2006 Additive and interactive effects of word frequency
and masked repetition in the lexical decision task.
\emph{Psychonomic Bulletin & Review} \bold{13}, 668-673.

Kliegl, R., Masson, M. E. J. and Richter, E. M. 2008. A linear
mixed-effects model analysis of masked repetition priming.
\emph{Manuscript}.
  }
\examples{
data(MaskedPriming)
str(MaskedPriming)
plot(MaskedPriming[sample(6381,100), 7:10])
\dontrun{
library(lme4)
cmat <- matrix(c(-1,  1,  0,
	             -1, -1,  2),  3,  2, 
	             dimnames=list(c("BM1", "BM2", "SK"), 
	                              c(".BM1-2", ".BM-SK")))
m0 <- lmer(rrt ~ p*f*e + (1 | subjects) + (0 + p | subjects) +
           (0 + f | subjects) + (1 | words), contrasts=list(e=cmat),
           data=d)
m1p <- lmer(rrt ~ p*f*e + (p | subjects) + (0+f | subjects) + (1 | words), 
                 contrasts=list(e=cmat)
m2 <- lmer(rrt ~ p*f*e + (p + f | subjects) + (1 | words), 
                 contrasts=list(e=cmat), data=d)
anova(m0, m1p, m2)
}
}
\keyword{datasets}
