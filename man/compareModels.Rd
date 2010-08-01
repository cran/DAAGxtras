\name{compareModels}
\alias{compareModels}
\title{Compare accuracy of alternative classification methods}
\description{
  Compare, between models, probabilities that the models assign to
  membership in the correct group or class. Probabilites should be
  estimated from cross-validation or from bootstrap out-of-bag data
  or preferably for test data that are completely separate from the
  data used to dervive the model.
}
\usage{
compareModels(groups = fgl$type, estprobs = list(lda = NULL, rf = NULL),
            gpnames = levels(fgl$type), robust = TRUE, print = TRUE) 
}
\arguments{
  \item{groups}{Factor that specifies the groups}
  \item{estprobs}{
    List whose elements (with names that identify the models) are
    matrices that give for each observation (row) estimated
    probabilities of membership for each of the groups (columns).}
    \item{gpnames}{Character: names for groups, if different from
    \code{levels(groups)}}  
  \item{robust}{Logical, \code{TRUE} or \code{FALSE}}
  \item{print}{Logical.  Should results be printed?}
}
\details{
The estimated probabilities are compared directly,  under normal
distribution assumptions.  An effect is fitted for each observation,
plus an effect for the method.  Comparison on a logit scale may
sometimes be preferable.  An option to allow this is scheduled for
incorporation in a later version.
}
\value{
\item{modelAVS}{Average accuracies for models}
\item{modelSE}{Approximate average SE for comparing models}
\item{gpAVS}{Average accuracies for groups}
\item{gpSE}{Approximate average SE for comparing groups}
\item{obsEff}{Effects assigned to individual observations}
}
\author{
John Maindonald
}
\note{
  The analysis estimates effects due to model and group (\code{gp}),
  after accounting for differences between observations.
}
\examples{
library(MASS)
library(DAAG)
ldahat <- lda(species ~ length+breadth, data=cuckoos, CV=TRUE)$posterior
qdahat <- qda(species ~ length+breadth, data=cuckoos, CV=TRUE)$posterior
rfhat <- predict(randomForest(species ~ length+breadth, data=cuckoos),
                 type="prob")
compareModels(groups=cuckoos$species, estprobs=list(lda=ldahat,
              qda=qdahat, rf=rfhat), robust=FALSE)

## The function is currently defined as
function (groups = fgl$type, estprobs = list(lda = NULL, rf = NULL),
            gpnames = levels(fgl$type), robust = TRUE, print = TRUE) 
{
  tab <- table(groups)
  checknam <- sapply(estprobs, function(x) all(names(x) == 
                                               names(tab)))
  if (!all(checknam)) 
    stop(c(paste("Levels of 'groups' are:", names(tab)), 
           paste("List elements", paste(names(tab)[!checknam], 
                                        collapse = " & "), "do not match these levels")))
  models <- factor(names(estprobs), levels = names(estprobs))
  if (is.null(models)) 
    stop("Elements of the list 'estprobs' must be named")
  g <- length(levels(groups))
  n <- length(groups)
  m <- length(estprobs)
  selmat <- cbind(1:n, unclass(groups))
  probs <- as.vector(sapply(estprobs, function(x) x[selmat]))
  df <- data.frame(gp = rep(groups, m), prob = probs, model = rep(models, 
                                                        rep(n, m)), obs = factor(rep(1:n, m)))
  if (robust) mod <- rlm(prob ~ model + obs, data = df) else
    mod <- lm(prob ~ model + obs, data = df)
  pred <- predict(mod, type = "terms", terms = c("model", "obs")) 
  bmod <- pred[match(models, df$model), "model"] + attr(pred, "constant")
  gpmod <- lm(pred[, "obs"] ~ -1+gp, data=df)
  gptab <- summary(gpmod)$coef
  bgp <- gptab[,1] + attr(pred, "constant")
  names(bgp) <- gpnames
  avsegp <- sqrt(mean(gptab[,2]^2))
  names(bmod) <- levels(models)
  coeff <- summary(mod)$coef[, 1:2]
  cnam <- rownames(coeff)
  modlab <- paste("model", levels(models), sep = "")
  modrows <- match(modlab[-1], cnam)
  semod <- coeff[modrows, 2]
  avsemod <- sqrt(mean(semod^2))
  resrows <- resid(gpmod)
  if (print) {
    print("Average accuracies for groups:")
    print(round(bgp, 4))
    print(c(`Approx sed` = round(avsegp, 4)))
    print("Average accuracies for methods:")
    print(round(bmod, 4))
    print(c(`Approx sed` = round(avsemod, 4)))
  }
  invisible(list(modelAVS = bmod, modelSE = avsemod, gpAVS = bgp, 
                 gpSE = avsegp, 
                 obsEff = resrows))
}
}
\keyword{statistics}
\keyword{multivariate}
