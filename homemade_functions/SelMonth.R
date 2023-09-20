#' Select a given month in a timeseries
#'
#' @param ser series, can be a zoo object
#' @param i Index of the month, i.e. 7 is July, 8 is August
#' @param fill.nas 
#' @param time Index of time steps for ser
#'
#' @return
#' @export
#'
#' @examples
SelMonth<-function(ser,i=7,fill.nas=TRUE,time=NULL){
  if(zoo::is.zoo(ser)){
    time<-zoo::index(ser)
    ser<-zoo::coredata(ser)
  }
  selser<-matrix(ser[1:(12*floor(length(ser)/12))],12,floor(length(ser)/12))[i,]
  if(length(which(is.na(selser)))>0 & fill.nas) selser[which(is.na(selser))]<-mean(selser,na.rm=TRUE)
  if(is.null(time)) return(selser)
  timeser<-matrix(time[1:(12*floor(length(time)/12))],12,floor(length(time)/12))[i,]
  return(zoo::zoo(selser,order.by = timeser))
}