#' multiDA "broom" functions
#'
#' @title multiDA "broom" functions
#'
#' @param x a trained \code{multiDA} object
#' @return  a tidy data frame, with key results from the trained \code{multiDA object}, namely, a data.frame of significant features and their ranks. In the spirit of "tidy" from the broom package.
#' @export


#' @title tidy multiDA
#' @rdname multiDA.broom
#'
tidy_multiDA <-function(x){
  mR<- x$mR
  return(x$mR)
}

#' @title augment multiDA
#' @rdname multiDA.broom
#' @param x a trained \code{multiDA} object
#' @return  a tidy data frame, returning back the original class, matrix of features, augmented with the paritioning of each feature as given by the algorithm. In the spirit of "augment" from the broom package.
#' @export

augment_multiDA <- function(x){

  vy=as.numeric(x$vy)

  mM <- apply(x$mGamma,1,which.max)
  mC <- .labelPartitions(x)

  inds1 <- which(mM==1)
  inds2 <- which(mM==ncol(mC))
  inds <- union(inds1,inds2)

  vs <- seq(1:ncol(x$mX))
  names(vs) <- vs
  vs <- vs[-inds]

  mL <- matrix(0,nrow=nrow(x$mX), ncol=ncol(x$mX))
  colnames(mL)<-paste("Grouping",colnames(x$mX))

  mL[,inds1] <- rep(mC[1,1],nrow(x$mX))
  mL[,inds2] <- vy

  for(i in vs){
    part <- mC[,mM[i]]
    for(j in 1:length(part)){
      inds=which(vy==j)
      mL[inds, vs]=part[j]
    }
  }

  mJ <- data.frame(Class=vy,x$mX, mL)

  return(mJ)

}

#' @title glimpse multiDA
#' @rdname multiDA.broom
#' @param x a trained \code{multiDA} object
#' @return a one row data frame, with quick summaries from the algorithm. In the spirit of the "glance" function from the broom package.
#' @export

glimpse_multiDA <- function(x){

  sig <- nrow(x$mR)
  sG <- x$mR$gamma.hat[1]
  K <- x$K
  sP <- ncol(x$mS)

  df <- data.frame("Classes" = K, "Number of Partitions" = sP, "Number of Signficant Features" = sig, "Top Gamma" = sG)

  return(df)
}
