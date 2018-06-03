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

plot.multiDA<-function(x, ranked=TRUE, ranks=1:10, features=NULL){

  if (!inherits(x, "multiDA"))  {
    stop("x not of class 'multiDA'")
  }

  mR <- x$mR

  if(ranked==TRUE){

    if(nrow(mR)<max(ranks)){
      ranks <- 1:nrow(mR)
    }
  }

  ###########################################################

  mC <- .labelPartitions(x )

  #############################################################


  data=list()

  inds=1:nrow(mR)

  if(ranked==FALSE){
    inds <- which(res$mR$feature.ID%in%features)
  }

  for(j in inds){
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

  if(ranked==TRUE){
    inds.plot=ranks
  }
  if(ranked==FALSE){
    inds.plot=inds
  }

  return(purrr::map(inds.plot, p))
}
