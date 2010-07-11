compareModels <-
function(groups=fgl$type,
           estprobs=list(lda=NULL, rf=NULL),
           robust=TRUE, print=TRUE){
    tab <- table(groups)
    checknam <- sapply(estprobs, function(x)all(names(x)==names(tab)))
    if(!all(checknam))stop(c(paste("Levels of 'groups' are:", names(tab)),
                             paste("List elements",
                                   paste(names(tab)[!checknam], collapse=" & "),
                                   "do not match these levels")))
    models <- factor(names(estprobs), levels=names(estprobs))
    if(is.null(models))stop("Elements of the list 'estprobs' must be named")
    g <- length(levels(groups))
    n <- length(groups)
    m <- length(estprobs)  # Number of models
    selmat <- cbind(1:n, unclass(groups))
    probs <- as.vector(sapply(estprobs, function(x)x[selmat]))
    df <- data.frame(gp=rep(groups,m), prob=probs,
                     model=rep(models, rep(n, m)),
                     obs=as.factor(rep(1:n, m)))
    modsum <- summary(lm(prob ~ model*gp+obs, data=df))
    silas <- modsum$aliased
    if(any(silas))
      {omitnam <- names(silas)[silas]
       omitobs <- as.numeric(substring(omitnam, 4, nchar(omitnam)))
       n1 <- 1:n
       n1[omitobs] <- 1
       df <- data.frame(gp=rep(groups,m), prob=probs,
                        model=rep(models, rep(n, m)),
                        obs=factor(rep(n1, m), levels=unique(n1)))
       mod <- lm(prob ~ model*gp+obs, data=df)
     }
    if(robust){
      mod <- rlm(prob ~ model*gp+obs, data=df)
    }
    gpfac <- factor(levels(groups), levels=levels(groups))
    dfnew <- expand.grid(model=models, gp=gpfac)
    dfnew$obs <- df$obs[1]
    pred <- predict(mod, type="terms", terms=c("model","gp","model:gp"),
                    newdata=dfnew)
    bmod <- pred[match(models, dfnew$model),"model"]+attr(pred,"constant")
    names(bmod) <- levels(models)
    bgp <- pred[match(gpfac, dfnew$gp),"gp"]+attr(pred,"constant")
    names(bgp) <- levels(gpfac)
    biact <- with(dfnew, xtabs(pred[,"model:gp"] ~ model+gp))
    coeff <- summary(mod)$coef[,1:2]    
    cnam <- rownames(coeff)
    modlab <- paste("model",levels(models), sep="")
    gplab <- paste("gp",levels(gpfac), sep="")
    modrows <- match(modlab[-1], cnam)
    gprows <- match(gplab[-1], cnam)
    iactnam <- outer(modlab, gplab, paste, sep=":")              
    iactrows <- match(iactnam, cnam, nomatch=0)
    segp <- coeff[gprows, 2]
    avsegp <- sqrt(mean(segp^2))
    semod <- coeff[modrows, 2]    
    avsemod <- sqrt(mean(semod^2))
    seiact <- semod <- coeff[iactrows, 2]
    avseiact <-  sqrt(mean(seiact^2))
    obsrows <- substring(cnam,1,3)=="obs"
    obseff <- coeff[obsrows, 1]    
    if(print){
      print("Average accuracies for groups:")
      print(round(bgp,4))
      print(c("Approx sed" = round(avsegp,4)))  
      print("Average accuracies for methods:")
      print(round(bmod,4))
      print(c("Approx sed" = round(avsemod,4)))
      print("Model-group interactions")
      print(round(biact,4))
      print(c("Approx sed" = round(avseiact,4)))
    }
    invisible(list(modelAVS=bmod, modelSE=avsemod, gpAVS=bgp, gpSE=avsegp,
                   model_gp=biact, model_gpSE=avseiact, obsEff=obseff))
  }

