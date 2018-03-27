#' the multiDA function
#'
#' @title multiDA
#' @export
#' @param vy vector of class values (for training)
#' @param mX matrix containing the training data. The rows are the sample observations, and the columns are the features.
#' @param penalty choice of penalty for use in training. Options include \code{"AIC"}, \code{"BIC"}
#' @param pen.options if \code{"user"} is selected for \code{penalty}, pen options....
#' @param equal.var a \code{LOGICAL} value, indicating whether group specific variances should be equal or allowed to vary.
#' @param set.options options for set partition matrix S.
#' @return \code{multiDA} object that contains the trained multiDA classifier


#' @examples
#' n <- nrow(iris)
#' train <- sample(seq_len(n), n / 2)
#' dlda_out <- dlda(Species ~ ., data = iris[train, ])
#' predicted <- predict(dlda_out, iris[-train, -5])$class
#'
#' dlda_out2 <- dlda(x = iris[train, -5], y = iris[train, 5])
#' predicted2 <- predict(dlda_out2, iris[-train, -5])$class
#' all.equal(predicted, predicted2)


#' @rdname multiDA
#' @export


multiDA.default <- function(vy, mX, penalty=c("AIC", "BIC", "GIC-2", "GIC-3", "GIC-4", "GIC-5", "GIC-6","Chi-Sq"),
                    pen.options=NULL, equal.var, set.options=c("exhaustive", "onevsrest", "onevsall", "ordinal", "restrict", "user"),
                    MAXGROUPS=NULL, sUser=NULL){

  fac.input=is.factor(vy)

  vy.fac=NULL

  if(fac.input){
    vy.fac=vy
  }


  # Turn vy into a binary matrix of indicators
  mY <- .vec2mat(vy)

  # Set algorithm dimensions
  n <- nrow(mX)
  p <- ncol(mX)
  K <- ncol(mY)



  ##############################################

  # Find all partitions of K variables


  if (set.options == "exhaustive") {
    mS <- setparts(K)
    mS <- apply(mS, 2, .reord)
    mS <- mS
  }

  # Reduce the set of tests if the classes are ordinal
  if (set.options == "ordinal") {
    mS <- setparts(K)
    mS <- apply(mS, 2, .reord)
    mD <- apply(mS, 2, diff)
    mS <- mS[, apply(mD, 2, .allpositive)]
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

  obj <- (list(mS = mS,res=res, n=n, K=K, V=V, p=p, vnu=vnu, vg=vg, vpen=vpen, fac.input=fac.input, mY=mY, vy.fac=vy.fac, equal.var=equal.var))
  class(obj) <- "multiDA"
  return(obj)
}



#' multiDA prediction of the class membership of a matrix of new observations.
#'
#' @export
#' @rdname multiDA
#' @param object trained multiDA object
#' @param newdata matrix of observations to predict. Each row corresponds to a new observation.
#' @return list predicted class memberships of each row in newdata
#' @export


predict.multiDA <-function(object, newdata, ...){
  # Fit class probabilities

  if (!inherits(object, "multiDA"))  {
    stop("object not of class 'multiDA'")
  }
  if (is.vector(newdata)) {
    #newdata <- as.matrix(newdata)
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
    mX.til <- vx %*% matrix(1, 1, object$V)
    for (k in 1:K)
    {
      mMu <- object$res$mMu[, ind.mat[k, ]]
      mD <- mX.til - mMu
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
  vy.pred <- apply(mY.hat, 1, which.max)

  if(object$fac.input){
    vy.fac=object$vy.fac

    .num.2.fac=function(x){
      dat=as.numeric(unique(vy.fac))
      names(dat)=unique(vy.fac)
      val=names(which(dat==x))
      return(val)
    }

    vy.pred=as.factor(map_chr(vy.pred, .num.2.fac))
  }

  return(list(vy.pred = vy.pred, probabilities=mY.hat))

}



#' Outputs the summary for a DLDA classifier object.
#'
#' Summarizes the trained DLDA classifier in a nice manner.
#'
#' @param x object to print
#' @param ... unused
#' @export
print.dlda <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
  cat("Sample Size:\n")
  print(x$N)
  cat("Number of Features:\n")
  print(x$p)
  cat("Classes:\n")
  print(x$groups)
  cat("Prior Probabilities:\n")
  print(sapply(x$est, function(z) z$prior))
}

