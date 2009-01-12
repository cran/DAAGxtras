\name{excessRisk}
\alias{excessRisk}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create and analyze multiway frequency or weighted frequency
  table}
\description{This function creates a multi-way table of counts
for the \code{response} given a set of classifying factors.  Output
facilitates a check on how the factor specified as \code{margin}
may, after accounting for other classifying factors, affect the
response.  }
\usage{ excessRisk(form = weight ~ seatbelt + airbag, response
= "dead", margin = "airbag", data = nassCDS, decpl = 4,
printResults=TRUE) }
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{form}{\code{form} is a formula in which classifying factors
    appear on the right, with an optional weight variable on the left.}
  \item{response}{\code{response} is a binary variable or two-level
    factor such that the response of interest is the relative number
    in the two levels.
  }
  \item{margin}{\code{margin} is the factor whose effect on the
    response,
    after accounting for other classifying factors, is of interest}
  \item{data}{\code{data} is a data frame in which variables and
    factors may be found}
  \item{decpl}{\code{decpl} is the number of decimal places in
    proportions that appear in the output}
  \item{printResults}{if \code{TRUE}, a tabular summary is printed.}  
}
\details{
  The best way to understand what this function does may be to run it
  with the default parameters, and/or with examples that appear below.
}
\value{
  The function returns a data frame, with one row for each combination of
  levels of factors on the right of the formula, but excluding the
  factor specified as \code{margin}
  \item{}{Count for level 2 of response \& level 1 of margin}
  \item{}{Total tount for level 1 of margin}
  \item{}{Count for level 2 of response \& level 2 of margin}
  \item{}{Total count for level 2 of margin}
  \item{}{Proportion; divide count for level 1 of margin by total}
  \item{}{Proportion; divide count for level 2 of margin by total}
  \item{}{Excess count for level 2 of response in row; relative to
    the assumption that, in that row, there is no association between
    \code{response} and \code{margin}.  This is the observed response
    (for the default arguments, number of dead) for level 2 (airbag deployed),
    less the number that would have been expected if the proportion
    had been that for level 1. (Negative values favor airbags.)}
}
\references{See \code{help(nassCDS)}}
\author{John Maindonald}
\seealso{\code{xtabs}}
\examples{
excessRisk()
excessRisk(weight ~ airbag+seatbelt+dvcat)
UCB <- as.data.frame.table(UCBAdmissions)
excessRisk(Freq~Gender, response="Admit", margin="Gender",data=UCB)
excessRisk(Freq~Gender+Dept, response="Admit", margin="Gender",data=UCB)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{multivariate}
\keyword{survey}
