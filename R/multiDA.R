#' the multiDA function
#'
#' @title multiDA
#' @param vy vector of class values (for training)
#' @param mX matrix of data corresponding to \code{vy} (for training)
#' @param mX.test matrix of data for testing data set
#' @param penalty choice of penalty for use in training. Options include \code{"AIC"}, \code{"BIC"}
#' @param pen.options if \code{"user"} is selected for \code{penalty}, pen options....
#' @param equal.var a \code{LOGICAL} value, indicating whether group specific variances should be equal or allowed to vary.
#' @param set.options options for set partition matrix S.
#' @return mGamma
#' @export


multiDA <- function(vy, mX, mX.test, penalty=c("AIC", "BIC", "GIC-2", "GIC-3", "GIC-4", "GIC-5", "GIC-6","Chi-Sq"),
                    pen.options=NULL, equal.var, set.options=c("exhaustive", "onevsrest", "onevsall", "ordinal", "restrict", "user"),
                    MAXGROUPS=NULL, sUser=NULL){

  fac.input=is.factor(vy)

  if(fac.input){
    vy.fac=vy
  }


  # Turn vy into a binary matrix of indicators
  mY <- .vec2mat(vy)

  # Set algorithm dimensions
  n <- nrow(mX)
  p <- ncol(mX)
  K <- ncol(mY)

  if (class(mX.test) == "numeric") {
    mX.test <- matrix(mX.test, 1, p)
  }
  n.test <- nrow(mX.test)

  ##############################################

  # Find all partitions of K variables


  if (set.options == "exhaustive") {
    mS <- setparts(K)
    mS <- apply(mS, 2, reord)
    mS <- mS
  }

  # Reduce the set of tests if the classes are ordinal
  if (set.options == "ordinal") {
    mS <- setparts(K)
    mS <- apply(mS, 2, reord)
    mD <- apply(mS, 2, diff)
    mS <- mS[, apply(mD, 2, allpositive)]
  }

  if (set.options == "onevsrest") {
    # mS = setparts(K)
    # mS = apply(mS,2,reord)
    mS=matrix(nrow=K, ncol=(K+1),1)
    diag(mS[,(1:K+1)])=2
  }

  if (set.options == "onevsseparate") {
    mS <- cbind(1, 1:K)
  }

  # Find the number of groups in each partition
  vg <- apply(mS, 2, max)

  # Restrict the number of groups
  if (set.options == "restrict") {
    #MAXGROUPS <- 2
    mS <- mS[, vg <= MAXGROUPS]
    mS <- cbind(1, 1:K)
  }

  if (set.options == "user") {
    # check user matrix is subset of S matrix
    check <- all(sUser %in% mS)
    if (check == FALSE) {
      stop("User defined S matrix is not a subset of all set partitions")
      break
    } else {
      mS <- sUser
    }
  }


  # Define the number of partitions
  V <- ncol(mS)

  # Recalculate the number of groups in each partition
  vg <- apply(mS, 2, max)

  ##############################################


  # Calculate the degrees of freedom for each partition
  if (equal.var) {
    vnu <- vg - vg[1]
  } else {
    vnu <- 2 * (vg - vg[1])
  }

  # Calculate penalty for the calculated degrees of freedom
  alpha <- 0.01
  vpen <- rep(0, V)
  if (penalty == "AIC") {
    # AIC
    vpen <- vnu * 2
  } else if (penalty == "BIC") {
    # BIC
    vpen <- vnu * log(n)
  } else if (penalty == "GIC-2") {
    # GIC pen 2

    vpen <- vnu*(vg * p)^(1/3)
  } else if (penalty == "GIC-3") {
    # GIC pen 3
    vpen <- vnu*2*log(p*vg)

  } else if (penalty == "Chi-Sq") {
    # Control type 1 error for each test against the null hypothesis
    vpen <- qchisq(alpha, vnu, lower.tail = FALSE)

  } else if (penalty == "GIC-4") {
    # GIC pen 4
    vpen <- vnu*2*(log(p*vg)+log(log(p*vg)))

  } else if (penalty == "GIC-5") {
    # GIC pen 5
    vpen <- vnu*log(log(n))*log(p*vg)

  } else if (penalty == "GIC-6") {
    # GIC pen 6
    vpen <- vnu*log(n)*log(p*vg)

  } else if (penalty == "user") {
    vpen <- pen.options[1] * vnu + pen.options[2]
  }
  vpen[1] <- 0

  ##############################################

  if (equal.var) {
    res <- .test_LDA(mX, mY, mS, vpen)
  } else {
    res <- .test_QDA(mX, mY, mS, vpen)
  }

  ##############################################

  # Fit class probabilities
  rho.y <- colMeans(mY)

  ###################################################

  pos <- c(0, cumsum(apply(mS, 2, max)))
  ind.mat <- matrix(0, nrow = K, ncol = V)
  for (s in 1:V)
  {
    G <- max(mS[, s])
    pos.sub <- (pos[s] + 1):pos[s + 1]
    for (g in 1:G)
    {
      inds <- which(mS[, s] == g)
      ind.mat[inds, s] <- pos.sub[g]
    }
  }

  ###################################################

  # Begin Prediction Step
  w <- 1


  mEta.y <- matrix(0, n.test, K)
  # mEta.y.alt = matrix(0,n.test,K)
  for (i in 1:n.test)
  {
    vx <- matrix(mX.test[i, ], p, 1)
    mX.til <- vx %*% matrix(1, 1, V)
    for (k in 1:K)
    {
      mMu <- res$mMu[, ind.mat[k, ]]
      mD <- mX.til - mMu
      if (!equal.var) {
        # QDA
        mSig2 <- res$mSigma2[, ind.mat[k, ]]
        mL <- res$mGamma * (-0.5 * log(mSig2) - 0.5 * mD * mD / mSig2)
      } else {
        # LDA
        mL <- res$mGamma * (-0.5 * mD * mD / res$mSigma2)
      }
      mEta.y[i, k] <- sum(mL[, -1]) + rho.y[k] * log(rho.y[k])
    }
  }

  mY.hat<- .logMatToGamma(mEta.y)
  vy.pred <- apply(mY.hat, 1, which.max)

  if(fac.input){
    vy.pred=as.factor(map_chr(vy.pred, num.2.fac))
  }


  return(list(mGamma = res$mGamma, vy.pred = vy.pred, probabilities=mY.hat, mS = mS))
}
