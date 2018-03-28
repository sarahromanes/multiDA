.mat2vec <- function(mY) {
  classval <- c()
  for (i in 1:nrow(mY)) {
    v <- mY[i, ]
    classval[i] <- which(v == 1)
  }
  return(classval)
}

###############################################################

.vec2mat <- function(vy) {
  vy=as.numeric(vy)
  K <- length(unique(vy))
  n <- length(vy)
  mY <- matrix(0, nrow = n, ncol = K)
  for (i in 1:length(vy)) {
    val <- vy[i]
    mY[i, val] <- 1
  }
  return(mY)
}

##############################################################

.logMatToGamma <- function(log.mA) {
  R <- nrow(log.mA)
  C <- ncol(log.mA)
  vm <- matrixStats::rowMaxs(log.mA)
  mM <- matrix(vm, R, C, byrow = FALSE)
  mA.til <- exp(log.mA - mM)
  vs <- matrixStats::rowSums2(mA.til)
  mS <- matrix(vs, R, C, byrow = FALSE)
  mGamma <- mA.til / mS
  return(mGamma)
}

###############################################################

.test_LDA <- function(mX, mY, mS, vpen) {
  # Note that we assume
  # that mY has 1 and exactly one value in each row.
  # that mY is binary

  n <- nrow(mY) # Number of samples
  p <- ncol(mX) # Number of variables
  V <- ncol(mS) # Number of partitions

  # Number of groups in each partition
  vg <- apply(mS, 2, max)
  vp <- c(0, cumsum(vg))

  nMeans <- sum(vg) # Total number of means to be estimated
  nVars <- V # Total number of variances to be estimated

  mMu <- matrix(0, nrow = p, ncol = nMeans)
  mSigma2 <- matrix(0, nrow = p, ncol = nVars)

  for (v in 1:V)
  {
    # Number of groups in current partition
    G <- vg[v]
    vs <- mS[, v]

    # Matrix of means and square deviations from the mean
    mM <- matrix(0, nrow = p, ncol = G)
    mD <- matrix(0, nrow = p, ncol = G)

    for (g in 1:G)
    {
      grp <- which(vs == g)
      siz <- length(grp)
      mY.til <- matrix(mY[, grp], n, siz)

      inds <- which(matrixStats::rowSums2(mY.til) == 1)


      n.til <- length(inds)
      mM[, g] <- matrixStats::colMeans2(mX[inds, ])
      mD[, g] <- matrixStats::colVars(mX[inds, ]) * (n.til - 1)
    }

    mSigma2[, v] <- matrixStats::rowSums2(mD) / n

    mMu[, (vp[v] + 1):vp[v + 1]] <- mM
  }

  mEta <- -0.5 * n * log(mSigma2)
  mLambda <- 2 * (mEta - mEta[, 1]) - matrix(vpen, p, V, byrow = TRUE)
  mLambda[, 1] <- 0

  mGamma <- .logMatToGamma(0.5 * mLambda)

  return(list(mGamma = mGamma, mSigma2 = mSigma2, mMu = mMu))
}

##############################################################

.test_QDA <- function(mX, mY, mS, vpen) {
  # Note that we assume
  # that mY has 1 and exactly one value in each row.
  # that mY is binary

  n <- nrow(mY) # Number of samples
  p <- ncol(mX) # Number of variables
  V <- ncol(mS) # Number of partitions

  # Number of groups in each partition
  vg <- apply(mS, 2, max)
  vp <- c(0, cumsum(vg))

  nMeans <- sum(vg) # Total number of means to be estimated
  nVars <- sum(vg) # Total number of variances to be estimated

  mMu <- matrix(0, nrow = p, ncol = nMeans)
  mSigma2 <- matrix(0, nrow = p, ncol = nVars)

  for (v in 1:V)
  {
    # Number of groups in current partition
    G <- vg[v]
    vs <- mS[, v]

    # Matrix of means and square deviations from the mean
    mM <- matrix(0, nrow = p, ncol = G)
    mD <- matrix(0, nrow = p, ncol = G)

    for (g in 1:G)
    {
      grp <- which(vs == g)
      siz <- length(grp)
      mY.til <- matrix(mY[, grp], n, siz)

      inds <- which(matrixStats::rowSums2(mY.til) == 1)

      n.til <- length(inds)
      mM[, g] <- matrixStats::colMeans2(mX[inds, ])
      mD[, g] <- matrixStats::colVars(mX[inds, ]) * (n.til - 1) / n.til
    }

    mSigma2[, (vp[v] + 1):vp[v + 1]] <- mD + 1.0E-8
    mMu[, (vp[v] + 1):vp[v + 1]] <- mM
  }

  # mSigma2[mSigma2==0] = 1.0E-2

  mEta <- matrix(0, nrow = p, ncol = V)
  for (v in 1:V)
  {
    G <- vg[v]
    vs <- mS[, v]

    veta <- matrix(0, nrow = p, ncol = G)
    mS2 <- as.matrix(mSigma2[, (vp[v] + 1):vp[v + 1]])

    for (g in 1:G)
    {
      grp <- which(vs == g)
      siz <- length(grp)
      mY.til <- matrix(mY[, grp], n, siz)
      inds <- which(matrixStats::rowSums2(mY.til) == 1)
      n.til <- length(inds)
      veta[, g] <- -0.5 * n.til * log(mS2[, g])
    }
    mEta[, v] <- matrixStats::rowSums2(veta)
  }

  mLambda <- 2 * (mEta - mEta[, 1]) - matrix(vpen, p, V, byrow = TRUE)
  mLambda[, 1] <- 0
  mGamma <- .logMatToGamma(0.5 * mLambda)

  return(list(mGamma = mGamma, mSigma2 = mSigma2, mMu = mMu, mLambda = mLambda))
}


##############################################################

.reord <- function(x) {
  return(match(x, unique(x)))
}

##############################################################

.allpositive <- function(x) {
  return(all(x >= 0))
}

##############################################################

.num.2.fac=function(x, vy.fac){
  dat <- as.numeric(unique(vy.fac))
  names(dat) <- unique(vy.fac)
  val=names(which(dat==x))
  return(val)
}


################################################################


