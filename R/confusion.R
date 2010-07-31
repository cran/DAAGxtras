confusion <-
function(actual, predicted, gpnames=NULL, rowcol=c("actual","predicted"),
           printit=TRUE, prior=NULL, digits=3){
    if(is.null(gpnames))gpnames <- levels(actual)
    tab <- table(actual, predicted)
    acctab <- t(apply(tab, 1, function(x)x/sum(x)))
    dimnames(acctab) <- list(Actual=gpnames,
                             "Predicted (cv)"=gpnames) 
    if(is.null(prior)){
      relnum <- table(actual)
      prior <- relnum/sum(relnum)
      acc <- sum(tab[row(tab)==col(tab)])/sum(tab)
    } else
    {
      acc <- sum(prior*diag(acctab))
    }    
    names(prior) <- gpnames
    if (printit){
    cat("Overall accuracy =", round(acc, digits), "\n")
    cat("This assumes the following prior frequencies:", "\n")
    print(round(prior, digits)) 
    cat("\nConfusion matrix", "\n")
    print(round(acctab, digits))
  }
    invisible(acctab)
  }

