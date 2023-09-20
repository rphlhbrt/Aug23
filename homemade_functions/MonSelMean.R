#' Average of several month every year
#'
#' @param ser series, can be a zoo object
#' @param is 
#' @param fill.nas 
#' @param time 
#'
#' @return
#' @export
#'
#' @examples
MonSelMean<-function(ser,is,fill.nas=TRUE,time=NULL){
  if(zoo::is.zoo(ser)){
    time<-zoo::index(ser)
    ser<-zoo::coredata(ser)
  }
  selser<-colMeans(matrix(ser[1:(12*floor(length(ser)/12))],12,floor(length(ser)/12))[is,],na.rm = TRUE)
  if(length(which(is.na(selser)))>0 & fill.nas) selser[which(is.na(selser))]<-mean(selser,na.rm=TRUE)
  if(is.null(time)) return(selser)
  timeser<-colMeans(matrix(time[1:(12*floor(length(time)/12))],12,floor(length(time)/12)))
  return(zoo::zoo(selser,order.by = timeser))
}