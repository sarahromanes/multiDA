#' Outputs the summary for a multiDA classifier object.
#'
#' Summarizes the trained multiDA classifier in a nice manner. User can select number of features to summarise
#'
#' @param x object to print
#' @param max.rank number of significant features to display. If \code{"ALL"}, all features are displayed.
#' @param ... Any other variables which will be ignored.
#' @export
#'
print.multiDA <- function(x, max.rank=10,...) {

  if (!inherits(x, "multiDA"))  {
    stop("x not of class 'multiDA'")
  }

  mR=x$mR

  if(max.rank!="ALL" & nrow(x$mR)>=max.rank){
    mR<-mR[1:max.rank, ]
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
  print(mR)
  cat("Partition Matrix:\n")
  print(x$set.options)
  print(x$mS)
}
