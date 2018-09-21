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
