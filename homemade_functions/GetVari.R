#' Simple wrapper to extract a variable from a netCDF file
#'
#' @param path Path to the netCDF file
#' @param varid Name of the target variable to extract from the netCDF
#'
#' @return
#' @export
#'
#' @examples
GetVari<-function(path,varid){
  ncfile<-ncdf4::nc_open(path)
  arr<-ncdf4::ncvar_get(ncfile,varid = varid)
  ncdf4::nc_close(ncfile)
  return(arr)
}
