#' plot ranked features from a trained multiDA object
#'
#' @export
#' @rdname multiDA
#' @param x trained multiDA object
#' @param ranks a vector of which ranked features should be plot
#' @param ... Any other variables which will be ignored
#' @return plots
#' @export
#'

plot.multiDA<-function(x, ranks=1:10, ...){

  if (!inherits(x, "multiDA"))  {
    stop("x not of class 'multiDA'")
  }

  mR <- x$mR

  if(nrow(mR)<max(ranks)){
    ranks <- 1:nrow(mR)
  }

  ###########################################################

  mC <- .labelPartitions(x )

  #############################################################


  data=list()

  for(j in 1:nrow(mR)){
    vs <- x$mS[,mR$partition[j]]
    vc <- mC[,mR$partition[j]]
    vy <- .mat2vec(x$mY)
    grouping <- vc[vy]
    value <- x$mX[, as.character(mR$feature.ID[j])]
    rank.feature <- rep(j, length(vy))
    dat <- data.frame(value, grouping, rank.feature)
    data[[j]] <- dat
  }

  p=function(r){

    p1 <- ggplot2::ggplot(data[[r]],aes(x=value, fill=grouping, color=grouping)) + geom_density(alpha=0.25) +
      ggtitle(paste("Feature:", mR$feature.ID[r], ", Rank:", mR$rank[r], ", gamma.hat=",signif(mR$gamma.hat[r],4)))+
      scale_colour_brewer(palette="Dark2")+
      scale_fill_brewer(palette="Dark2")
      p1
    }


  return(purrr::map(ranks, p))
}
