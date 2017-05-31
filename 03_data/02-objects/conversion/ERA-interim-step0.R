
library("ecmwfConversion")

# ERA-interim reanalysis data at step 0

# Instantaneous forecasts of 2m surface temp, u & v wind components, MSLP

########################################################################################################
# Example of Python code used to extract raw data                                                   ####

##!/usr/bin/env python
#from ecmwfapi import ECMWFDataServer
#server = ECMWFDataServer()
#
#server.retrieve({
#    "class"	: "ei",
#    "dataset"	: "interim",
#    "date"	: "2007-11-01/to/2008-02-28",
#    "expver"	: "1",
#    "grid"	: "1/1",
#    "area"    	: "60/-6/50/2",
#    "levtype"	: "sfc",
#    "param"	: "151.128/165.128/166.128/167.128",
#    "step"	: "0",
#    "stream"	: "oper",
#    "time"	: "00:00:00/12:00:00",
#    "type"	: "an",
#    "format"	: "netcdf",
#    "target"	: "../ei-2007-step0.nc",
#})

########################################################################################################
# Load data                                                                                         ####

library(ncdf4); library(abind)

load.reanalysis <- function(fnm) {
    
    ncin <- nc_open(fnm)
    
    # dimensions of data
    lon <- ncvar_get(ncin, "longitude")
    lat <- ncvar_get(ncin, "latitude")
    date <- gsub("-", "", as.Date(ncvar_get(ncin, "time")/24, origin = "1900-01-01"))
    hr <- formatC(ncvar_get(ncin, "time") %% 24, width = 2, flag = 0)
    
    dat <- abind(invisible(sapply(names(ncin$var), function(varb) {
        hh <- ncvar_get(ncin, varb)
        dimnames(hh) <- list("lon" = lon, "lat" = lat, "date" = date)
        abind(invisible(sapply(unique(hr), function(s) {
            hh[,,hr == s]
        }, simplify = F)), rev.along = 0)
    }, simplify = F)), along = 0)
    
    # sort lat & long in ascending order
    dat <- dat[,order(lon),order(lat),,]
    
    names(dimnames(dat)) <- c("varb", "lon", "lat", "date", "hr")
    
    nc_close(ncin)
    return(dat)
}

yy <- gsub(".+-", "", gsub("-step.+", "", list.files("./data/era-interim", pattern = "step0")))

era <- abind(invisible(sapply(yy, function(y) {
    load.reanalysis(paste0("./data/era-interim/ei-", y, "-step0.nc"))
}, simplify = F)), along = 3.5)

names(dimnames(era)) <- c("varb", "lon", "lat", "year", "day", "hour")
dimnames(era)$day <- substr(dimnames(era)$day, 5,8)

saveRDS(era, "./02-objects/step0-ERAint.rds")

########################################################################################################
# extract north & south regions                                                                     ####

nsm <- ns.mask()

era <- readRDS("../../../03_data/02-objects/step0-ERAint.rds")

ns.obs <- abind("temp.n" = apply(sweep(era["t2m",,,,,], 1:2, nsm$n, "*"), 3:5, mean, na.rm = T),
                "temp.s" = apply(sweep(era["t2m",,,,,], 1:2, nsm$s, "*"), 3:5, mean, na.rm = T),
                along = 0)

names(dimnames(ns.obs)) <- c("varb", "year", "day", "time")

dimnames(ns.obs)$day <- paste(sapply(dimnames(ns.obs)$day, substr, 1, 2), sapply(dimnames(ns.obs)$day, substr, 3,4), sep = "-")

saveRDS(ns.obs, "../../../03_data/02-objects/ns-temp-obs.rds")
