
# ERA-interim invariant land-sea mask

########################################################################################################
# Import land-sea mask for region of interest                                                       ####

library(ncdf4); library(maps)

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

ls <- land.sea.mask(N = 90, W = -180, S = 0, E = 180)

image(lsm$lon, lsm$lat, lsm$mask, xlim = c(-10,10), ylim = c(50,60), 
      col = mapply(adjustcolor, c("dodgerblue", "green3"), alpha = 0.5))
map("world", add = T)
abline(h = 49.5:60.5, v = -10.5:10.5, lty = 2, col = adjustcolor("black", alpha = 0.4))

lsm <- land.sea.mask()
lsm[lon(lsm) == 2,] <- 0                # remove that corner of France
saveRDS(lsm, "./02-objects/lsm.rds")    # save as RDS for independent access

# save as package data
setwd("./ecmwfConversion")
use_data(lsm)
setwd("..")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Masks for N and S regions                                                                         ####

ns.mask <- function() {
    
    lsm[lsm == 0] <- NA
    lsm.n <- lsm.s <- lsm
    
    # cut at latitude 54.5 as specified in original data
    lsm.n[,lat(lsm.n) < 54.5] <- NA
    lsm.s[,lat(lsm.s) > 54.5] <- NA
    
    return(list("n" = lsm.n, "s" = lsm.s))
}

nsm <- ns.mask()

# 'internal error -2 in R_decompress1' when saving with use_data
# so create using function instead




map("world", xlim = range(lon(lsm.n)), ylim = range(lat(lsm.n)))
image(lon(lsm.n), lat(lsm.n), lsm.n, col = adjustcolor("dodgerblue", alpha = 0.3), add = T)
image(lon(lsm.s), lat(lsm.s), lsm.s, col = adjustcolor("red", alpha = 0.3), add = T)
abline(h=49.5:60.5, v=-5.5:2.5, lty=  3, col = adjustcolor("black", alpha = .4))

lsmNorth <- lsm.n

