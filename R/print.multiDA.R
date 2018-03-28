#' Outputs the summary for a multiDA classifier object.
#'
#' Summarizes the trained multiDA classifier in a nice manner. User can select number of features to summarise
#'
#' @param object object to print
#' @param max.rank number of significant features to display. If \code{"ALL"}, all features are displayed.
#' @export
print.multiDA <- function(object, max.rank=10) {

  if (!inherits(object, "multiDA"))  {
    stop("object not of class 'multiDA'")
  }


  if(is.null(colnames(object$mX))){
    rownames(object$res$mGamma)<-as.character(1:nrow(object$res$mGamma))
    colnames(object$mX)<-rownames(object$res$mGamma)
  }else{
    rownames(object$res$mGamma)<-make.unique(colnames(object$mX))
  }

  inds<-which(apply(object$res$mGamma,1,which.max)!=1) #non null cases
  est.gamma<-apply(object$res$mGamma[inds,],1,max)

  df<-data.frame("rank"=rank(-est.gamma),"gamma.hat"=est.gamma,"partition"=apply(object$res$mGamma,1,which.max)[inds])
  
  df<-df[order(df$rank),]

  if(max.rank!="ALL" & nrow(df)>=max.rank){
    df<-df[1:max.rank, ]
  }

  cat("Sample Size:\n")
  print(object$n)
  cat("Number of Features:\n")
  print(object$p)
  cat("Classes:\n")
  print(object$K)
  cat("Equal Variance Assumption:\n")
  print(object$equal.var)
  cat("Number of Significant Features:\n")
  print(sum(apply(object$res$mGamma,1,which.max)!=1))
  cat("Summary of Significant Features:\n")
  print(df)
  cat("Partition Matrix:\n")
  print(object$set.options)
  print(object$mS)
}
