
########################################################################################################
# GENERAL SUPPORT FUNCTIONS                                                                         ####

#' Get longitudes
#' 
#' Plotting function to concisely extract longitude range from an array with named dimensions
#' 
#' @param arr Array from which longitude is to be extracted
#' 
#' @return Vector of longitude values
#' 
#' @export
#' 
lon <- function(arr) {
    as.numeric(dimnames(arr)$longitude)
}

#' Get latitudes
#' 
#' Plotting function to concisely extract latitude range from an array with named dimensions
#' 
#' @param arr Array from which latitude is to be extracted
#' 
#' @return Vector of numeric latitude values
#' 
#' @export
#' 
lat <- function(arr) {
    as.numeric(dimnames(arr)$latitude)
}


########################################################################################################
# LAND-SEA MASKS                                                                                    ####

#' Import land-sea mask
#' 
#' Import the invariant land-sea mask used by ERA-interim from a netcdf download
#' 
#' @param fnm Filename containing .nc land-sea mask. Defaults to last storage location.
#' @param NWSE Four separate parameters indicating N,W,S,E boundaries of region of interest (in degrees). Default selects a small region centred on the UK.
#' 
#' @return Labelled array of 0 (= sea) or 1 (= land)
#' 
#' @export
#' 
land.sea.mask <- function(fnm = "./01-raw-data/invariant/ERA-int-lsm.nc", N = 60, W = -6, S = 50, E = 2) {
    
    # open the netCDF file
    ncin <- nc_open(fnm)
    
    # extract lat, long, mask
    lon <- sort(ncvar_get(ncin, "longitude"))
    lat <- sort(ncvar_get(ncin, "latitude"))
    mask <- ncvar_get(ncin, "lsm")[order(ncvar_get(ncin, "longitude")),
                                   order(ncvar_get(ncin, "latitude"))]
    
    # crop region
    mask <- mask[findInterval(lon, c(W,E), rightmost.closed = T) == 1,
                 findInterval(lat, c(S,N), rightmost.closed = T) == 1]
    
    lon <- lon[findInterval(lon, c(W,E), rightmost.closed = T) == 1]
    lat <- lat[findInterval(lat, c(S,N), rightmost.closed = T) == 1]
    
    # close netcdf file & return mask
    nc_close(ncin)
    return(array(mask, dim = dim(mask), dimnames = list("longitude" = lon, "latitude" = lat)))
}



#' Create north-south mask
#' 
#' Define the regions considered as 'north UK' and 'south UK' in the study
#' @details The 'northern' region consists of all land regions above 54.5 degrees north, with the 'southern' region containing all land regions in the UK below this latitude.
#' 
#' @return List of labelled land-sea masks, with sea values marked as NA. Each mask can therefore be multiplied by a temperature field of the same dimensions to obtain only the values of interest.
#' 
#' @export
#' 
ns.mask <- function() {
    
    lsm[lsm == 0] <- NA
    lsm.n <- lsm.s <- lsm
    
    # cut at latitude 54.5 as specified in original data
    lsm.n[,lat(lsm.n) < 54.5] <- NA
    lsm.s[,lat(lsm.s) > 54.5] <- NA
    
    return(list("n" = lsm.n, "s" = lsm.s))
}


########################################################################################################