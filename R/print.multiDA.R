#' Outputs the summary for a multiDA classifier object.
#'
#' Summarizes the trained multiDA classifier in a nice manner.
#'
#' @param x object to print
#' @export
print.multiDA <- function(x) {
  cat("Sample Size:\n")
  print(x$n)
  cat("Number of Features:\n")
  print(x$p)
  cat("Classes:\n")
  print(x$K)
  cat("Number of Significant Features")
  print(sum(apply(x$res$mGamma,1,which.max)!=1))
}
