GlobalFieldMean<-function(arr,lats=NULL,na.rm=TRUE){
  dimi<-dim(arr)
  if(length(dimi)==3){
  if(is.null(lats)) lats<-rep(0,dimi[2])
  redarr<-sapply(1:dimi[2],function(i) colMeans(arr[,i,],na.rm = na.rm))
  return(apply(redarr,1,function(i) weighted.mean(i,cos(lats*pi/180),na.rm = na.rm)))
  }else{
    lonmean<-colMeans(arr,na.rm = na.rm)
    return(weighted.mean(lonmean,cos(lats*pi/180),na.rm = na.rm))
  }
  
}