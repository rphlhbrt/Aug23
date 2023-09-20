
# Make map
#' Title
#'
#' @param lonordata Vector of Longitudes or 3-column matrix with (lon,lat,value)
#' @param lat Vector of Latitudes
#' @param data  Allows for the lon,lat,value to be input in a matrix with 3 columns
#' @param negseq Vector of values for the bins of the first color, eg. negative values (will be made automatically if left to NULL)
#' @param posseq Vector of values for the bins of the second color, eg. positive values (will be made automatically if left to NULL)
#' @param cutoff Vector of 2 values specifying the minimum and maximum values at which to cut the color scale
#' @param cex Expansion factor for the points on the map
#' @param edge Color of the edge of the points
#' @param pch Markers to be used for the points (usually the filled markers from 21 to 25)
#' @param opacity Opacity of the color in the points
#' @param latlim Vector of 2 values specifying the minimum and maximum values of the latitudes to be shown
#' @param lonlim Vector of 2 values specifying the minimum and maximum values of the longitudes to be shown
#' @param legpos Position of the legend, can be a marker such as 'topleft', or a pair of values c(longitude,latitude)
#' @param namevari Name of the variable to be indicated in the legend
#' @param col Vector of three colors indicating the first (negative), the transition (null), and the second (positive) colors.
#' @param add Logical, if TRUE, points are added on an existing map previously generated
#' @param legend Logical, if TRUE a legend is generated
#' @param legcex Expansion factor for the legend
#' @param legbg Specify color of legend's background
#' @param leg.pt.cex  Blow up factor for legend point
#' @param return.colors Logical, if TRUE, the coordinates with the colors of the points is returned
#' @param return.legend.colors Logical, if TRUE, returns the colours in the legend
#' @param lonres
#' @param latres
#'
#' @return
#' @export
#'
#' @examples
MapPolygon<-function(lonordata,lat=NULL,value=NULL,negseq=NULL,n.classes=10,
                  posseq=NULL,cutoff=NULL,cex=1,distop=NULL,borderop=1.,
                  edge=NULL,pch=21,border='black',
                  opacity=NULL,latlim=c(-90,90),return.colors=FALSE,return.legend.colors=FALSE,
                  lonlim=c(-210,191.5),legpos=c(-210,80),mar=rep(0,4),
                  namevari=latex2exp::TeX("$\\beta$"),lakes=FALSE,rivers=FALSE,
                  add=FALSE,log10values=FALSE,bty='n',colscheme='batlow',
                  legend=TRUE,legcex=1,legbg='white',revcolours=FALSE,leg.n.classes=NULL,
                  leg.pt.cex=NULL,n.negseq=5,n.posseq=5,lonres=NULL,latres=NULL,...){
  if(length(dim(lonordata))<2){
    lon<-lonordata
    lon<-lon[which(!is.na(value))]
    lat<-lat[which(!is.na(value))]
    if(!is.null(distop)) distop<-distop[which(!is.na(value))]
    if(!is.null(opacity) & length(opacity)>1) opacity<-opacity[which(!is.na(value))]
    value<-value[which(!is.na(value))]
  }else{
    data<-lonordata
    value<-data[,3]
    lon<-data[which(!is.na(value)),1]
    lat<-data[which(!is.na(value)),2]
    if(!is.null(distop)) distop<-distop[which(!is.na(value))]
    if(!is.null(opacity) & length(opacity)>1) opacity<-opacity[which(!is.na(value))]
    value<-value[which(!is.na(value))]
  }
  lon[which(lon> (191.45))]<-lon[which(lon> (191.45))]-360
  lon[which(lon< (-168.55))]<-lon[which(lon< (-168.55))]+360
if(is.null(opacity)) opacity<-rep(1,length(lon))
  if(is.null(leg.pt.cex))leg.pt.cex<-cex
  if(!add) maps::map(ylim=latlim,xlim=lonlim,interior = FALSE,mar=mar)
  if(rivers) library(mapdata)
  if(rivers) maps::map('rivers',add=TRUE,col='skyblue',lwd=2,mar=mar)

  keep<-which(!is.na(value))
  value<-value[keep]
  lon<-lon[keep]
  lat<-lat[keep]
  if(is.null(cutoff)) cutoff<-quantile(value,prob=c(0.1,0.9))#c(min(value),max(value))
  seqclasses<-seq(cutoff[1],cutoff[2],diff(cutoff)/n.classes)
  if(length(colscheme)==2){
    if(length(revcolours)==1) revcolours<-rep(revcolours,length(colscheme))
    pal1<-khroma::colour(colscheme[[1]])(n.classes/2)
    if(revcolours[[1]]) pal1<-rev(pal1)
    pal2<-khroma::colour(colscheme[[2]])(n.classes/2)
    if(revcolours[[2]]) pal2<-rev(pal2)
    mypal<-c(pal1,pal2)
    }else{
  mypal<-khroma::colour(colscheme)(n.classes)
  if(revcolours) mypal<-rev(mypal)
  }
  

  
  if(is.null(lonres)){ dlon<-abs(diff(lon))
  lonres<-rep(median(dlon[which(dlon>0)]),length(unique(lon)))}
  if(is.null(latres)){ dlat<-abs(diff(lat))
  latres<-rep(median(dlat[which(dlat>0)]),length(unique(lat)))}
  

  lon.minus<-lonres[1]/2*-1
  lon.plus<-lonres[1]/2
  lat.minus<-latres[1]/2*-1
  lat.plus<-latres[1]/2
  lon.corners<-c(lon.minus,lon.minus,lon.plus,lon.plus)#,lon.minus)
  
  lat.corners<-c(lat.minus,lat.plus,lat.plus,lat.minus)#,lat.minus)
  
  col.in<-mypal[findInterval(value,seqclasses,all.inside = TRUE)]
  if(is.null(edge)) edge<-col.in
  if(length(edge)==1) edge<-rep(edge,length(col.in))
  if(length(opacity)==1) edge<-rep(opacity,length(col.in))
  for(i in 1:length(value)){ 

    polygon(lon[i]+lon.corners,lat[i]+lat.corners,pch=pch,border = adjustcolor(edge[i],borderop),
            col=adjustcolor(col.in[i],opacity[i]),cex=cex,...)
  }
 # return("Dobe")
  
  maps::map(ylim=latlim,xlim=lonlim,interior = FALSE,add = TRUE)
  
  if(length(opacity)>1) opacity<-1
  if(legend & !log10values){
    legend(legpos[1],legpos[2],title = namevari,cex=legcex,bg=legbg,
           legend=round(seqclasses[-1]-diff(seqclasses)/2,2),bty = bty,
           pch=pch,col=edge,pt.cex=leg.pt.cex,pt.bg =adjustcolor(mypal,opacity))
  }
  if(legend & log10values){
    legend(legpos[1],legpos[2],title = namevari,cex=legcex,bg=legbg,
           legend=round(10^(seqclasses[-1]-diff(seqclasses)/2),2),bty = bty,
           pch=pch,col=edge,pt.cex=leg.pt.cex,pt.bg =adjustcolor(mypal,opacity))
  }
  if(!is.null(leg.n.classes)) MapPoints(lonordata = rep(-360,100),lat = rep(-180,100),colscheme = colscheme,leg.pt.cex=leg.pt.cex,cutoff = cutoff,log10values = log10values,
                                        value = rnorm(100),n.classes = 10,add = TRUE,legpos = legpos,bty=bty,namevari=namevari,pch=22)
}


roof<-function(ser,val){
  ser[which(ser>val)]<-rep(val,length(which(ser>val)))
  return(ser)
}
