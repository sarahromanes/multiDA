#' Outputs the summary for a multiDA classifier object.
#'
#' Summarizes the trained multiDA classifier in a nice manner. User can select number of features to summarise
#'
#' @param x object to print
#' @param max.rank number of significant features to display. If \code{"ALL"}, all features are displayed.
#' @export
print.multiDA <- function(x, max.rank=10) {

  if (!inherits(x, "multiDA"))  {
    stop("x not of class 'multiDA'")
  }


  if(is.null(colnames(x$mX))){
    rownames(x$res$mGamma)<-as.character(1:nrow(x$res$mGamma))
    colnames(x$mX)<-rownames(x$res$mGamma)
  }else{
    rownames(x$res$mGamma)<-make.unique(colnames(x$mX))
  }

  inds<-which(apply(x$res$mGamma,1,which.max)!=1) #non null cases
  est.gamma<-apply(x$res$mGamma[inds,],1,max)

  df<-data.frame("rank"=rank(-est.gamma),"gamma.hat"=est.gamma,"partition"=apply(x$res$mGamma,1,which.max)[inds])

  df<-df[order(df$rank),]

  if(max.rank!="ALL" & nrow(df)>=max.rank){
    df<-df[1:max.rank, ]
  }

  cat("Sample Size:\n")
  print(x$n)
  cat("Number of Features:\n")
  print(x$p)
  cat("Classes:\n")
  print(x$K)
  cat("Equal Variance Assumption:\n")
  print(x$equal.var)
  cat("Number of Significant Features:\n")
  print(sum(apply(x$res$mGamma,1,which.max)!=1))
  cat("Summary of Significant Features:\n")
  print(df)
  cat("Partition Matrix:\n")
  print(x$set.options)
  print(x$mS)
}
