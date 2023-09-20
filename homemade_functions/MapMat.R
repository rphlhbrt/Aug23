MapMat<-function(mat,lon=NULL,lat=NULL,colscheme = "vik",sbst=NULL,opacity=0.7,dropbelow=NULL,dropabove=NULL,colsig=c("blue","orange"),cexsig=1,lwdsig=5,pchsig="x",...){
  dimi<-dim(mat)
  if(is.null(lon)){
    lonres<-360/dimi[1]
    lon<-seq(-180+lonres/2,180-lonres/2,lonres)
  }
  if(is.null(lat)){
    latres<-180/dimi[2]
    lat<-seq(-90+latres/2,90-latres/2,latres)
  }
  values<-as.vector(mat)
  if(is.null(sbst)) sbst<-seq_along(values)
  if(is.null(dropbelow) & is.null(dropabove)){
    MapPolygon(cbind(expand.grid(lon,lat),values)[sbst,],opacity = rep(opacity,length(lon)*length(lat)),colscheme = colscheme,...)
    }else{
    MapPolygon(cbind(expand.grid(lon,lat),values),opacity = rep(opacity,length(lon)*length(lat)),colscheme = colscheme,...)
    lon[which(lon>190)]<-lon[which(lon>190)]-360
    if(!is.null(dropbelow)) points(expand.grid(lon,lat)[intersect(sbst,which(values<dropbelow)),1],
           expand.grid(lon,lat)[intersect(sbst,which(values<dropbelow)),2],col=colsig[1],pch=pchsig,cex=cexsig,lwd=lwdsig)
    if(!is.null(dropabove)) points(expand.grid(lon,lat)[intersect(sbst,which(values>dropabove)),1],
                              expand.grid(lon,lat)[intersect(sbst,which(values>dropabove)),2],col=colsig[2],pch=pchsig,cex=cexsig,lwd=lwdsig)
    
        }
}
