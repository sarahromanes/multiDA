#' plot ranked features from a trained multiDA object
#'
#' @export
#' @rdname multiDA
#' @param x trained multiDA object
#' @param ranks a vector of which ranked features should be plot
#' @param save.plot logical value indicating whether plots should be saved (\code{TRUE}) or plotted on graphics device (\code{FALSE})
#' @return plots
#' @export
#'

plot.multiDA<-function(x, ranks=1:10, save.plot=FALSE){

  if (!inherits(x, "multiDA"))  {
    stop("x not of class 'multiDA'")
  }


  if(is.null(colnames(x$mX))){
    rownames(x$res$mGamma)<-as.character(1:nrow(x$res$mGamma))
    colnames(x$mX)<-rownames(x$res$mGamma)
  }else{
    rownames(x$res$mGamma)<-colnames(x$mX)
  }

  inds<-which(apply(x$res$mGamma,1,which.max)!=1) #non null cases
  est.gamma<-apply(x$res$mGamma[inds,],1,max)

  df<-data.frame("est.gamma"=est.gamma, "rank"=rank(-est.gamma),"partition"=apply(x$res$mGamma,1,which.max)[inds])
  df<-df[order(df$rank),]

  if(nrow(df)<max(ranks)){
    ranks=1:nrow(df)
  }

###########################################################

G=apply(x$mS, 2, max)
mC=x$mS

for(s in 1:ncol(x$mS)){
  for (g in 1:G[s]){
    inds=which(x$mS[,s]==g)
    vals=c()
    for(i in 1:length(inds)){

      if(x$fac.input){
        vy.fac <- x$vy.fac
        labels<-purrr::map_chr(inds, .num.2.fac, vy.fac)
      }else{
        labels<-inds
      }

      if(i==length(inds)){
        vals[i]=as.character(labels[i])
      }else{
      vals[i]=as.character(paste(labels[i], "+ "))
      }
    }
    mC[inds,s]=paste(vals,collapse="")
  }
}

#############################################################


data=list()

for(j in 1:nrow(df)){
  vs=x$mS[,df$partition[j]]
  vc=mC[,df$partition[j]]
  vy=.mat2vec(x$mY)
  grouping=vc[vy]
  value=x$mX[, rownames(df)[j]]
  rank.feature <- rep(j, length(vy))
  dat=data.frame(value, grouping, rank.feature)
  data[[j]]<-dat
}

p=function(r){

  p1<-ggplot2::ggplot(data[[r]],aes(x=value, fill=grouping, color=grouping)) + geom_density(alpha=0.25) +
      ggtitle(paste("Feature:", rownames(df)[r], ", Rank:", df$rank[r], ", gamma.hat=",signif(df$est.gamma[r],4)))+
      scale_colour_brewer(palette="Dark2")+
      scale_fill_brewer(palette="Dark2")
  if(save.plot==TRUE){
    p1
    ggplot2::ggsave(paste("multiDA-feature-rank",r,".pdf"))
  }
  else{
    p1
  }
}

return(purrr::map(ranks, p))
}

