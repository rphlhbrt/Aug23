# Source some homemade functions ----
sapply(list.files("homemade_functions",full.names = TRUE),source)
colas<-c("#01665e", "#8c510a", "#c51b7d", "#fec44f", "#bcbddc", "#9ecae1", "#bdbdbd", "#08519c", "#88419d", "#238b45", "#88419d", "#ccebc5", "#fc8d59"    )


# Figure of HadSST for the pre-computed averages for a month in 2023, by default August ----
{
  month_index<-8 
  month_index_2023<-173*12+month_index
  pdf("figures/HadSST_Aug23_Global_NH_SH_Tropics.pdf",width = 7,height = 7)
par(mfrow=c(2,2),mar=c(1,1,1,1),oma=c(2.5,2.5,0,0))
pathz<-ListGrepFiles("HadSST.4.0.1.0_monthly","data")
regions<-c("Global","Northern Hemisphere","Southern Hemisphere","Tropics")
for(iii in 1:4){
  hadmon_GMST<-read.csv(pathz[iii])$anomaly
  zHadAnomalies<-zoo::zoo(c(hadmon_GMST,NA,NA,NA,NA),order.by = 1850+seq_along(c(1,1,1,1,hadmon_GMST))/12)
  zHadAnom_DCO2<-DetrendCO2(zHadAnomalies)
  zHadAnom_DCO2_Month<-SelMonth(zHadAnom_DCO2,i = month_index)
  print(zHadAnomalies[2080:2085])
  CustomAxis(seq(1850,2000,50),xlims = c(1850,2024),xlog10 = FALSE,ylims = c(-2,1.3),ylog10 = FALSE,seq(-2,1,0.5),col.grid = FALSE,ylab=expression(T[anom]~(K)),xlab = "Year CE",
             xcol = if(iii %in% c(3,4)){"black"}else{adjustcolor("white",0)}, # Makes the x-axis invisible for the top plots, remove to have axis on all plots
             ycol = if(iii %in% c(1,3)){"black"}else{adjustcolor("white",0)}) # Makes the y-axis invisble for the right plots
  lines(zHadAnomalies,col=colas[1])
  points(zHadAnomalies[which(zHadAnom_DCO2[1:month_index_2023]>=zoo::coredata(zHadAnom_DCO2[month_index_2023]))],col=colas[5])
  points(SelMonth(zHadAnomalies,time = zoo::index(zHadAnomalies),i = month_index)[which(zHadAnom_DCO2_Month>=zoo::coredata(zHadAnom_DCO2[month_index_2023]))],col=colas[4],pch=10)
  lines(DetrendCO2(zHadAnomalies,forced.cmpnt = TRUE),col=colas[2],lwd=3)
  lines(zHadAnom_DCO2-1.5,col=colas[3],lwd=1)
  points(-1.5+zHadAnom_DCO2[which(zHadAnom_DCO2[1:month_index_2023]>=zoo::coredata(zHadAnom_DCO2[month_index_2023]))],col=colas[5])
  points(-1.5+SelMonth(zHadAnom_DCO2,time = zoo::index(zHadAnomalies),i = month_index)[which(zHadAnom_DCO2_Month>=zoo::coredata(zHadAnom_DCO2[month_index_2023]))],col=colas[4],pch=10)
  mtext(regions[iii],line = -2)
}
dev.off()
}

# Figure of HadSST for the pre-computed averages for the average of several months in 2023, by default JJA ----
{
  month_index<-c(7,8)
  year_index_2023<-174
  pdf("figures/HadSST_JJA23_Global_NH_SH_Tropics.pdf",width = 7,height = 7)
  par(mfrow=c(2,2),mar=c(1,1,1,1),oma=c(2.5,2.5,0,0))
  pathz<-ListGrepFiles("HadSST.4.0.1.0_monthly","data")
  regions<-c("Global","Northern Hemisphere","Southern Hemisphere","Tropics")
  for(iii in 1:4){
    hadmon_GMST<-read.csv(pathz[iii])$anomaly
    zHadAnomalies<-zoo::zoo(c(hadmon_GMST,NA,NA,NA,NA),order.by = 1850+seq_along(c(1,1,1,1,hadmon_GMST))/12)
    zHadAnom_DCO2<-DetrendCO2(zHadAnomalies)
    zHadAnomalies_MultiMonthAvg<-MonSelMean(zHadAnomalies,is = month_index)
    zHadAnom_DCO2_MultiMonthAvg<-MonSelMean(zHadAnom_DCO2,is = month_index)
    CustomAxis(seq(1850,2000,50),xlims = c(1850,2024),xlog10 = FALSE,ylims = c(-2,1.3),ylog10 = FALSE,seq(-2,1,0.5),col.grid = FALSE,ylab=expression(T[anom]~(K)),xlab = "Year CE",
               xcol = if(iii %in% c(3,4)){"black"}else{adjustcolor("white",0)}, # Makes the x-axis invisible for the top plots, remove to have axis on all plots
               ycol = if(iii %in% c(1,3)){"black"}else{adjustcolor("white",0)}) # Makes the y-axis invisble for the right plots
    lines(zHadAnomalies_MultiMonthAvg,col=colas[1])
    points(zHadAnomalies_MultiMonthAvg[which(zHadAnom_DCO2_MultiMonthAvg[1:year_index_2023]>=zoo::coredata(zHadAnom_DCO2_MultiMonthAvg[year_index_2023]))],col=colas[5])
    lines(DetrendCO2(zHadAnomalies_MultiMonthAvg,forced.cmpnt = TRUE),col=colas[2],lwd=3)
    lines(zHadAnom_DCO2_MultiMonthAvg-1.5,col=colas[3],lwd=1)
    points(-1.5+zHadAnom_DCO2_MultiMonthAvg[which(zHadAnom_DCO2_MultiMonthAvg[1:year_index_2023]>=zoo::coredata(zHadAnom_DCO2_MultiMonthAvg[year_index_2023]))],col=colas[5])
    mtext(regions[iii],line = -2)
  }
  dev.off()
}


# Figure of HadSST for selected region average for the month of August ----
{
Had_lat<-GetVari("data/HadSST.4.0.1.0_median.nc",varid = "latitude")
lat_bands<-rbind(c(-60,60),c(30,60),c(60,90),c(-90,-60))
pdf("figures/HadSST_Aug23_Lat_Bands.pdf",width = 7,height = 7)
par(mfrow=c(2,2),mar=c(1,1,1,1),oma=c(2.5,2.5,0,0))
for(iii in 1:4){
lat_kept<-which(Had_lat>(lat_bands[iii,1]) & Had_lat<lat_bands[iii,2])
month_index<-8 
month_index_2023<-173*12+month_index
hadmon_GMST<-GlobalFieldMean(GetVari("data//HadSST.4.0.1.0_median.nc",varid = "tos")[,lat_kept,],Had_lat[lat_kept])
hadmon_GMST<-read.csv(pathz[iii])$anomaly
zHadAnomalies<-zoo::zoo(c(hadmon_GMST,NA,NA,NA,NA),order.by = 1850+seq_along(c(1,1,1,1,hadmon_GMST))/12)
zHadAnom_DCO2<-DetrendCO2(zHadAnomalies)
zHadAnom_DCO2_Month<-SelMonth(zHadAnom_DCO2,i = month_index)
print(zHadAnomalies[2080:2085])
CustomAxis(seq(1850,2000,50),xlims = c(1850,2024),xlog10 = FALSE,ylims = c(-2,1.3),ylog10 = FALSE,seq(-2,1,0.5),col.grid = FALSE,ylab=expression(T[anom]~(K)),xlab = "Year CE",
           xcol = if(iii %in% c(3,4)){"black"}else{adjustcolor("white",0)}, # Makes the x-axis invisible for the top plots, remove to have axis on all plots
           ycol = if(iii %in% c(1,3)){"black"}else{adjustcolor("white",0)}) # Makes the y-axis invisble for the right plots
lines(zHadAnomalies,col=colas[1])
points(zHadAnomalies[which(zHadAnom_DCO2[1:month_index_2023]>=zoo::coredata(zHadAnom_DCO2[month_index_2023]))],col=colas[5])
points(SelMonth(zHadAnomalies,time = zoo::index(zHadAnomalies),i = month_index)[which(zHadAnom_DCO2_Month>=zoo::coredata(zHadAnom_DCO2[month_index_2023]))],col=colas[4],pch=10)
lines(DetrendCO2(zHadAnomalies,forced.cmpnt = TRUE),col=colas[2],lwd=3)
lines(zHadAnom_DCO2-1.5,col=colas[3],lwd=1)
points(-1.5+zHadAnom_DCO2[which(zHadAnom_DCO2[1:month_index_2023]>=zoo::coredata(zHadAnom_DCO2[month_index_2023]))],col=colas[5])
points(-1.5+SelMonth(zHadAnom_DCO2,time = zoo::index(zHadAnomalies),i = month_index)[which(zHadAnom_DCO2_Month>=zoo::coredata(zHadAnom_DCO2[month_index_2023]))],col=colas[4],pch=10)
mtext(paste0("Latitude Band: ",lat_bands[iii,1]," to ",lat_bands[iii,2]))
}
dev.off()
}


# Map of the # of standard deviation away of the local August 23 w.r.t to the logCO2 detrended local series ----

lastanom_to_sd<-function(ser,indx=2084) ser[indx]/sd(ser,na.rm=TRUE)
anom_sd<-apply(GetVari("data/HadSST.4.0.1.0_median.nc",varid = "tos"),c(1,2),function(i) if(length(which(!is.na(i)))>(50*12)){lastanom_to_sd(DetrendCO2(zoo::zoo(i,order.by = seq_along(i)/12+1850)))}else{NA})
dim(anom_sd)
coords.5by5<-expand.grid(seq(-177.5,177.5,5),seq(-87.5,87.5,5))
MapMat(anom_sd,cutoff=c(-3,3))
points(coords.5by5[which(as.numeric(anom_sd)>2),]) # points more than 2 standard deviations away




# Figure of HadSST for selected region average for the month of August: Just a zoomed version on the years 1970-2024 in order to see better the recent years ----
# It seems like there is a bit of a change in statistical properties for the 30N-60N band starting in 2005 or so when ARGO floats were introduced, just an observation
{
  Had_lat<-GetVari("data/HadSST.4.0.1.0_median.nc",varid = "latitude")
  lat_bands<-rbind(c(-60,60),c(30,60),c(60,90),c(-90,-60))
  pdf("figures/HadSST_Aug23_Lat_Bands_Zoom_1970_2024.pdf",width = 7,height = 7)
  par(mfrow=c(2,2),mar=c(1,1,1,1),oma=c(2.5,2.5,0,0))
  for(iii in 1:4){
    lat_kept<-which(Had_lat>(lat_bands[iii,1]) & Had_lat<lat_bands[iii,2])
    month_index<-8 
    month_index_2023<-173*12+month_index
    hadmon_GMST<-GlobalFieldMean(GetVari("data//HadSST.4.0.1.0_median.nc",varid = "tos")[,lat_kept,],Had_lat[lat_kept])
    hadmon_GMST<-read.csv(pathz[iii])$anomaly
    zHadAnomalies<-zoo::zoo(c(hadmon_GMST,NA,NA,NA,NA),order.by = 1850+seq_along(c(1,1,1,1,hadmon_GMST))/12)
    zHadAnom_DCO2<-DetrendCO2(zHadAnomalies)
    zHadAnom_DCO2_Month<-SelMonth(zHadAnom_DCO2,i = month_index)
    print(zHadAnomalies[2080:2085])
    CustomAxis(seq(1970,2020,10),xlims = c(1970,2024),xlog10 = FALSE,ylims = c(-2,1.3),ylog10 = FALSE,seq(-2,1,0.5),col.grid = FALSE,ylab=expression(T[anom]~(K)),xlab = "Year CE",
               xcol = if(iii %in% c(3,4)){"black"}else{adjustcolor("white",0)}, # Makes the x-axis invisible for the top plots, remove to have axis on all plots
               ycol = if(iii %in% c(1,3)){"black"}else{adjustcolor("white",0)}) # Makes the y-axis invisble for the right plots
    lines(zHadAnomalies,col=colas[1])
    points(zHadAnomalies[which(zHadAnom_DCO2[1:month_index_2023]>=zoo::coredata(zHadAnom_DCO2[month_index_2023]))],col=colas[5])
    points(SelMonth(zHadAnomalies,time = zoo::index(zHadAnomalies),i = month_index)[which(zHadAnom_DCO2_Month>=zoo::coredata(zHadAnom_DCO2[month_index_2023]))],col=colas[4],pch=10)
    lines(DetrendCO2(zHadAnomalies,forced.cmpnt = TRUE),col=colas[2],lwd=3)
    lines(zHadAnom_DCO2-1.5,col=colas[3],lwd=1)
    points(-1.5+zHadAnom_DCO2[which(zHadAnom_DCO2[1:month_index_2023]>=zoo::coredata(zHadAnom_DCO2[month_index_2023]))],col=colas[5])
    points(-1.5+SelMonth(zHadAnom_DCO2,time = zoo::index(zHadAnomalies),i = month_index)[which(zHadAnom_DCO2_Month>=zoo::coredata(zHadAnom_DCO2[month_index_2023]))],col=colas[4],pch=10)
    mtext(paste0("Latitude Band: ",lat_bands[iii,1]," to ",lat_bands[iii,2]))
  }
  dev.off()
}
