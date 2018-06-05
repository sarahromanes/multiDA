#' explore ranked features from a trained multiDA object
#'
#' @export
#' @param x trained multiDA object
#' @param G return rankings and features based on the number of groupings (G) per feature to be considered.
#' @param ... Any other variables which will be ignored
#' @return dRank, a dataframe of summarised features, from which feature names can easily be fed into the plot.multiDA function for easy visualisation of the groupings
#' @export
#'

rank.multiDA<-function(x, G){

  if (!inherits(x, "multiDA"))  {
    stop("x not of class 'multiDA'")
  }

  mS=x$mS
  g=apply(mS,2,max)
  inds=which(g==G)

  colnames(x$mGamma) = paste("Partition", c(1:ncol(x$mGamma)))
  mGam_sub=x$mGamma[,inds]

  if(G==1 | G==max(g)){
    d=length(mGam_sub)
    r.gam=rank(-mGam_sub)
    names(r.gam)=c(1:length(r.gam))
    r.gam=sort(r.gam)

    mC=as.matrix(.labelPartitions(x)[,inds])

    dRank=data.frame("Rank"=c(1:d),"Gamma Est"=mGam_sub[as.numeric(names(r.gam))], "Feature" = colnames(x$mX)[as.numeric(names(r.gam))], "Partition"= rep(inds,d), "No.Groups"=rep(G, d))
  }else{

    d=dim(mGam_sub)
    r.gam=rank(-mGam_sub)
    names(r.gam)=c(1:length(r.gam))
    r.gam=sort(r.gam)
    coord=arrayhelpers::vec2array(as.numeric(names(r.gam)),d)

    mC=as.matrix(.labelPartitions(x)[,inds])
    colnames(mC)=(paste("Partition", c(1:ncol(x$mGamma))))[inds]

    if(FALSE){
    vals=c()
    for(s in 1:ncol(mC)){

      labels=mC[,s]
      temp=c()

      for(i in 1:length(unique(labels))){

        if(i==length(unique(labels))){
          temp[i]=paste("(", labels[i], ")")
        }else{
          temp[i]=as.character(paste("(", labels[i], ")", " vs. "), sep="")
        }
      }

      vals[s]=paste(temp,collapse="")
    }

    names(vals)=colnames(mC)

    groups=c()
    for(i in 1:nrow(coord)){
      inds=match(colnames(mC)[coord[i,2]],names(vals))
      groups[i]=vals[inds]
    }

  }

    dRank=data.frame("Rank"=c(1:nrow(coord)),"Gamma Est"=mGam_sub[coord], "Feature" = colnames(x$mX)[coord[,1]], "Partition"= as.numeric(unlist(lapply(strsplit(colnames(mC)[coord[,2]], " "), function(x) x[2]))), "No.Groups"=c(rep(G, nrow(coord))))
  }

  return(dRank)

}
