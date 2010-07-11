\name{confusion}
\alias{confusion}
\title{
  Given actual and predicted group assignments, give the confusion matrix
}
\description{
  Given actual and predicted group assignments, give the confusion matrix
}
\usage{
confusion(actual, predicted, names = NULL, rowcol=c("actual",
"predicted"), printit = TRUE, prior = NULL)
}
\arguments{
  \item{actual}{
Actual (prior) group assigments
}
  \item{predicted}{
    Predicted group assigments.
  }
\item{names}{
Names for groups, if different from \code{levels(actual)}
}
\item{rowcol}{For predicted categories to appear as rows,
  specify \code{rowcol="predicted"}
}
  \item{printit}{
Logical. Should results be printed?
}
  \item{prior}{
    Prior probabilities for groups, if different from the relative group
    frequencies
}
}
\details{
  Predicted group assignments should be estimated from cross-validation
  or from bootstrap out-of-bag data.  Better still, work with assignments
  for test data that are completely separate from the data used to
  dervive the model.
}
\value{
Confusion matrix
}
\references{
  Maindonald and Braun: 'Data Analysis and Graphics Using R',
  3rd edition 2010, Section 12.2.2

}
\author{
John H Maindonald
}
\examples{
library(MASS)
library(DAAG)
cl <- lda(species ~ length+breadth, data=cuckoos, CV=TRUE)$class
confusion(cl,cuckoos$species)

## The function is currently defined as
function(actual, predicted, names=NULL, rowcol=c("actual","predicted"),
           printit=TRUE, prior=NULL){
    if(is.null(names))names <- levels(actual)
    tab <- table(actual, predicted)
    acctab <- t(apply(tab, 1, function(x)x/sum(x)))
    dimnames(acctab) <- list(Actual=names,
                             "Predicted (cv)"=names) 
    if(is.null(prior)){
      relnum <- table(actual)
      prior <- relnum/sum(relnum)
      acc <- sum(tab[row(tab)==col(tab)])/sum(tab)
    } else
    {
      acc <- sum(prior*diag(acctab))
      names(prior) <- names
    }    
    if(printit)print(round(c("Overall accuracy"=acc, 
                             "Prior frequency"=prior),4))
    if(rowcol[1]=="predicted")acctab <- t(acctab)
    if(printit){
      cat("\nConfusion matrix", "\n")
      print(round(acctab,4))
    }
    invisible(acctab)
  }
}
\keyword{statistics}
\keyword{multivariate}
