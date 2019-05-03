#' multiDA prediction of the class membership of a matrix of new observations.
#'
#' @export
#' @rdname multiDA
#' @param object trained multiDA object
#' @param newdata matrix of observations to predict. Each row corresponds to a new observation.
#' @return list predicted class memberships of each row in newdata
#' @export
#'


predict.multiDA <-function(object, newdata, ...){
  # Fit class probabilities

  if (!inherits(object, "multiDA"))  {
    stop("object not of class 'multiDA'")
  }
  if (is.vector(newdata)) {
    newdata <- matrix(newdata, 1, object$p)
  }

  n.test <- nrow(newdata)

  ##################################################

  rho.y <- colMeans(object$mY)

  ###################################################

  mA <- c(0, cumsum(apply(object$mS, 2, max)))
  ind.mat <- matrix(0, nrow = object$K, ncol = object$V)
  for (s in 1:object$V)
  {
    G <- max(object$mS[, s])
    mA.sub <- (mA[s] + 1):mA[s + 1]
    for (g in 1:G)
    {
      inds <- which(object$mS[, s] == g)
      ind.mat[inds, s] <- mA.sub[g]
    }
  }

  ###################################################

  mEta.y <- matrix(0, n.test, object$K)

  for (i in 1:n.test)
  {
    vx <- matrix(newdata[i, ], object$p, 1)
    X.til <- vx %*% matrix(1, 1, object$V)
    for (k in 1:object$K)
    {
      mMu <- object$res$mMu[, ind.mat[k, ]]
      mD <- X.til - mMu
      if (!object$equal.var) {
        # multiQDA
        mSig2 <- object$res$mSigma2[, ind.mat[k, ]]
        mL <- object$res$mGamma * (-0.5 * log(mSig2) - 0.5 * mD * mD / mSig2)
      } else {
        # multiLDA
        mL <- object$res$mGamma * (-0.5 * mD * mD / object$res$mSigma2)
      }
      mEta.y[i, k] <- sum(mL[, -1]) + rho.y[k] * log(rho.y[k])
    }
  }

  mY.hat<- .logMatToGamma(mEta.y)
  y.pred <- apply(mY.hat, 1, which.max)

  if(object$fac.input){
    y.fac <- object$y.fac
    y.pred<-as.factor(purrr::map_chr(y.pred, .num.2.fac,y.fac))
  }

  return(list(y.pred = y.pred, probabilities=mY.hat))

}

