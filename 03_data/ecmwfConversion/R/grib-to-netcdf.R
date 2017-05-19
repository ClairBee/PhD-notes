
#' GRIB to NETCDF conversion
#' 
#' Creates a new file containing the specified data, converted to NETCDF format
#' @details Downloaded data must be converted to .nc format before importing with the other functions. This is a wrapper function to call 'grib_to_netcdf' from 'eccodes' package without having to go to the command line
#' 
#' @param fnm Filename to be converted (including full path)
#' 
#' @export
#' 
grib.convert <- function(fnm) {
    
    # if .grib extension supplied, remove it
    fnm <- gsub(".grib$", "", fnm)
    
    # only convert if expected output file doesn't already exist
    if (file.exists(paste0(fnm, ".nc"))) {
        cat("A netcdf file of that name already exists. \n")
    } else {
        system(paste("grib_to_netcdf", paste0(fnm, ".grib"), "-o", paste0(fnm, ".nc"), "-T"))
    }
}