#' plot ranked features from a trained multiDA object
#'
#' @export
#' @rdname multiDA
#' @param object trained multiDA object
#' @param ranks a vector of which ranked features should be plot
#' @param save.plot logical value indicating whether plots should be saved (\code{TRUE}) or plotted on graphics device (\code{FALSE})
#' @return plots
#' @export
#'

plot.multiDA<-function(object, ranks, save.plot=TRUE){

  if(is.null(colnames(object$mX))){
    rownames(object$res$mGamma)<-as.character(1:nrow(object$res$mGamma))
    colnames(object$mX)<-rownames(object$res$mGamma)
  }else{
    rownames(object$res$mGamma)<-colnames(object$mX)
  }

  inds<-which(apply(object$res$mGamma,1,which.max)!=1) #non null cases
  est.gamma<-apply(object$res$mGamma[inds,],1,max)

  df<-data.frame("est.gamma"=est.gamma, "rank"=rank(-est.gamma),"partition"=apply(object$res$mGamma,1,which.max)[inds])
  df<-df[order(df$rank),]

###########################################################
  
  
G=apply(object$mS, 2, max)
mC=object$mS

for(s in 1:ncol(object$mS)){
  for (g in 1:G[s]){
    inds=which(object$mS[,s]==g)
    vals=c()
    for(i in 1:length(inds)){
      
      if(object$fac.input){
        vy.fac <- object$vy.fac
        labels<-map_chr(inds, .num.2.fac, vy.fac)
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
  vs=object$mS[,df$partition[j]]
  vc=mC[,df$partition[j]]
  vy=.mat2vec(object$mY)
  grouping=vc[vy]
  value=object$mX[, rownames(df)[j]]
  rank.feature <- rep(j, length(vy))
  dat=data.frame(value, grouping, rank.feature)
  data[[j]]<-dat
}

p=function(x){
  
  p1<-ggplot(data[[x]],aes(x=value, fill=grouping, color=grouping)) + geom_density(alpha=0.25) +
      ggtitle(paste("Feature:", rownames(df)[x], ", Rank:", df$rank[x], ", gamma.hat=",signif(df$est.gamma[x],4)))+
      scale_colour_brewer(palette="Dark2")+
      scale_fill_brewer(palette="Dark2")
  if(save.plot==TRUE){
    p1
    ggsave(paste("multiDA-feature-rank",x,".pdf"))
  }
  else{
    p1
  }
}

return(map(ranks, p))
}

