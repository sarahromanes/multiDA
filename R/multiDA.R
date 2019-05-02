#' the multiDA function
#'
#' @title multiDA
#' @param mX matrix containing the training data. The rows are the sample observations, and the columns are the features.
#' @param vy vector of class values (for training)
#' @param penalty default is in the form of the EBIC, which penalises based on the number of features. If option \code{penalty="BIC"} is specified, the penalty reverts back to the BIC.
#' @param equal.var a \code{LOGICAL} value, indicating whether group specific variances should be equal or allowed to vary.
#' @param set.options options for set partition matrix S.
#' @param sUser if \code{set.options} is set to \code{"user"}, \code{sUser} is a user input matrix for paritions to be considered. \code{sUser} MUST be a subset of the full partition matrix..
#' @return \code{multiDA} object that contains the trained multiDA classifier


#' @examples
#' #train the multiDA classifier using the SRBCT dataset, and find the resubstitution error rate
#'
#' vy   <- SRBCT$vy
#' mX   <- SRBCT$mX
#' res  <- multiDA(mX, vy, equal.var=TRUE, set.options="exhaustive", penalty="EBIC")
#' vals <- predict(res, newdata=mX)$vy.pred          #vy.pred returns class labels
#' rser <- sum(vals!=vy)/length(vy)

#' @rdname multiDA
#' @export
#' @importFrom stats qchisq
#' @importFrom ggplot2  ggplot
#' @importFrom ggplot2  aes
#' @importFrom ggplot2  ggtitle
#' @importFrom ggplot2  geom_density
#' @importFrom ggplot2  ggsave
#' @importFrom ggplot2  scale_colour_brewer
#' @importFrom ggplot2  scale_fill_brewer
#' @importFrom ggplot2  theme
#' @importFrom ggplot2  element_text
#' @importFrom purrr  map
#' @importFrom purrr  map_chr
#' @importFrom arrayhelpers  vec2array

multiDA <- function(mX,vy, penalty=c("EBIC", "BIC"),
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

  remove <- which(matrixStats::colMads(mX)==0)

  if(length(remove) > 0)
  {
    mX <-mX[, -remove]
    message(length(remove), "uninformative features ignored for classification.")
  }

  # Turn vy into a binary matrix of indicators
  mY <- .vec2mat(vy)

  # Set algorithm dimensions
  n <- nrow(mX)
  p <- ncol(mX)
  K <- ncol(mY)



  ##############################################

  # Find all partitions of K variables
  set.options <- match.arg(set.options)

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

  penalty <- match.arg(penalty)

  if(penalty=="BIC"){

    vpen <- 2*vnu*log(n)
    vpen[1] <- 0

  } else if (penalty=="EBIC"){
    vpen <- vnu*(log(n)+2*log(p))
    vpen[1] <- 0
  }




  ##############################################

  if (equal.var) {
    res <- .test_LDA(mX, mY, mS, vpen)
  } else {
    res <- .test_QDA(mX, mY, mS, vpen)
  }

  ##############################################

  #Generate rankings to either be used by print() or plot(), or for further analysis by the user

  inds <- which(apply(res$mGamma,1,which.max)!=1) #non null cases
  non.inds <- which(apply(res$mGamma,1,which.max)==1) #non nul

  if(is.numeric(inds)){
    est.gamma <- apply(as.matrix(res$mGamma[inds,]),1,max)
  }else{
    est.gamma <- apply(res$mGamma[inds,],1,max)
  }

  if(is.numeric(non.inds)){
    est.gamma1 <- apply(as.matrix(res$mGamma[non.inds,]),1,max)
  }else{
    est.gamma1 <- apply(res$mGamma[non.inds,],1,max)
  }


  mR <- data.frame("rank"=rank(-est.gamma),"feature ID" = colnames(mX)[inds],"gamma.hat"=est.gamma,"partition"=apply(res$mGamma,1,which.max)[inds])
  mR <- mR[order(mR$rank),]

  add <- data.frame("rank"=(rank(est.gamma1)+nrow(mR)),"feature ID" = colnames(mX)[non.inds],"gamma.hat"=est.gamma1,"partition"=rep(1, length(non.inds)))
  add <- add[order(add$rank),]

  mR <- rbind(mR,add)
  rownames(mR)<-c()

  #####################################################

  obj <- (list(mS = mS,res=res, mGamma=res$mGamma, mR=mR,n=n, K=K, V=V, p=p, vnu=vnu, vg=vg, vpen=vpen, fac.input=fac.input, mY=mY, vy.fac=vy.fac,
               equal.var=equal.var, mX=mX,set.options=set.options))
  class(obj) <- "multiDA"
  return(obj)
}






