RCP85<-read.table("data/RCP85_MIDYEAR_CONCENTRATIONS.DAT",skip = 38,header = TRUE)
#RCP60<-read.table("~/Documents/data/RCP6_MIDYEAR_CONCENTRATIONS.DAT",skip = 38,header = TRUE)
zrfCO2<-zoo::zoo(c(rep(0.01,15+50),log2(RCP85$CO2/277)),order.by = c(1700:1764,RCP85$YEARS))
#RF6<-zoo::zoo(c(rep(0.01,15+50),log2(RCP20$CO2EQ/277)),order.by = c(1700:1764,RCP20$YEARS))
DetrendCO2<-function(zser,zCO2=zrfCO2,TCS=FALSE,plot=FALSE,forced.cmpnt=FALSE,N.min=70*12){
  zout<-zser
  zsercore<-zoo::coredata(zser)
  zindex<-zoo::index(zser)
  zindex<-zindex[which(!is.na(zsercore))]
  if(length(zindex)<N.min & TCS ) return(NA)
  zsercore<-zsercore[which(!is.na(zsercore))]
  approxCO2<<-approx(x=zoo::index(zCO2),y = zoo::coredata(zCO2),xout = zindex)$y
  lmodel<-lm(zsercore ~ approxCO2)
  detrended<-residuals(lmodel)
  zout[which(!is.na(zoo::coredata(zser)))]<-detrended
  if(plot){
    plot(zindex,detrended+4,type='l',col='forestgreen',ylim=c(-9,9),xlab='Years',ylab="Anomaly")
  lines(zser-4,col='navyblue')
lines(zindex,coef(lmodel)[1]+coef(lmodel)[2]*approxCO2-4,lwd=3,col='indianred')
}
  if(TCS) return(coef(lmodel)[2])
  if(forced.cmpnt) return(zoo::zoo(coef(lmodel)[1]+coef(lmodel)[2]*approxCO2,order.by = zindex))
  return(zout)
}

