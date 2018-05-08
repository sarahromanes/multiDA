#' the multiDA function
#'
#' @title multiDA
#' @param mX matrix containing the training data. The rows are the sample observations, and the columns are the features.
#' @param vy vector of class values (for training)
#' @param penalty choice of penalty for use in training. Options include \code{"AIC"}, \code{"BIC"}....
#' @param equal.var a \code{LOGICAL} value, indicating whether group specific variances should be equal or allowed to vary.
#' @param set.options options for set partition matrix S.
#' @param sUser if \code{set.options} is set to \code{"user"}, \code{sUser} is a user input matrix for paritions to be considered. \code{sUser} MUST be a subset of the full partition matrix..
#' @return \code{multiDA} object that contains the trained multiDA classifier


#' @examples
#' #train the multiDA classifier using the SRBCT dataset, and find the resubstitution error rate
#' data(SRBCT)
#' vy   <- SRBCT$vy
#' mX   <- SRBCT$mX
#' res  <- multiDA(mX, vy, penalty="GIC-4", equal.var=TRUE, set.options="exhaustive")
#' vals <- predict(res, newdata=mX)$vy.pred          #vy.pred returns class labels
#' rser <- sum(vals!=vy)/length(vy)

#' @rdname multiDA
#' @export

multiDA <- function(mX,vy, penalty=c("AIC", "BIC", "GIC-2", "GIC-3", "GIC-4", "GIC-5", "GIC-6","Chi-Sq", "TEST1", "TEST2", "TEST3"),
                  equal.var=TRUE, set.options=c("exhaustive", "onevsrest", "onevsall", "ordinal", "user"), sUser=NULL){

  fac.input=is.factor(vy)

  vy.fac=NULL

  if(fac.input){
    vy.fac <- vy
  }

  # Generate column names for mX and/or make column names for mX unique

  if(is.null(colnames(mX))){
    colnames(mX)<-paste("V",1:ncol(mX), sep="")
  }else{
    colnames(mX)<-make.unique(colnames(mX))
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
    mS <- partitions::setparts(K)
    mS <- apply(mS, 2, .reord)
    mS <- mS
  }

  # Reduce the set of tests if the classes are ordinal
  if (set.options == "ordinal") {
    mS <- partitions::setparts(K)
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

  if (set.options == "user") {
    mS <- partitions::setparts(K)
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

  } else if (penalty == "TEST1") {
    vpen <- 0.5*vnu*log(n)+log(p)

  }else if (penalty == "TEST2") {

    vpen <- 0.5*vnu*log(n)+(2*log(p))
    vpen[1] <- 0.5*vnu*log(n)

  }else if (penalty == "TEST3") {
  vpen <- 0.5*vnu*log(n)+ (2*log(p*vg))
}
  vpen[1] <- 0

  ##############################################

  if (equal.var) {
    res <- .test_LDA(mX, mY, mS, vpen)
  } else {
    res <- .test_QDA(mX, mY, mS, vpen)
  }

  ##############################################

  #Generate rankings to either be used by print() or plot(), or for further analysis by the user

  inds <- which(apply(res$mGamma,1,which.max)!=1) #non null cases
  est.gamma <- apply(res$mGamma[inds,],1,max)

  mR <- data.frame("rank"=rank(-est.gamma),"feature ID" = colnames(mX)[inds],"gamma.hat"=est.gamma,"partition"=apply(res$mGamma,1,which.max)[inds])
  mR <- mR[order(mR$rank),]
  rownames(mR)<-c()

  #####################################################

  obj <- (list(mS = mS,res=res, mGamma=res$mGamma, mR=mR,n=n, K=K, V=V, p=p, vnu=vnu, vg=vg, vpen=vpen, fac.input=fac.input, mY=mY, vy.fac=vy.fac,
               equal.var=equal.var, mX=mX,set.options=set.options))
  class(obj) <- "multiDA"
  return(obj)
}







