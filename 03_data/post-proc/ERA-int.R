
library(rgdal); library(maps); library(reshape2)

########################################################################################################
# Mini data set: single location, timestep, parameter                                               ####


#server.retrieve({
#    "class": "ei",
#    "dataset": "interim",
#    "date": "2017-02-01/to/2017-02-28",                                # 28 days
#    "expver": "1",
#    "grid": "1/1",
#    "area"    : "60/-6/50/2",
#    "levtype": "sfc",
#    "param": "151.128/164.128/165.128/166.128/167.128",                # 5 parameters
#    "step": "0",                                                       # 1 time step
#    "stream": "oper",
#    "time": "00:00:00",
#    "type": "an",
#    "target": "ERA-int-2017-02.grib",
#})

grib <- readGDAL("ERA-interim/ERA-int-2017-02.grib")
lat <- 50:60; lon <- -6:2; dm <- c(length(lon), length(lat))

# 5 variables, 28 days, 1 time step = 140 observations

# 151.128 - MSLP
# 165.128 - 10m u-component
# 166.128 - 10m v-component
# 167.128 - 2m temperature
# 164.128 - total cloud cover

arr <- array(as.array(grib), dim = c(dm,5,28),
             dimnames = list("lon" = lon, "lat" = lat, "varb" = c("mslp", "u", "v", "temp", "cc"),
                             "date" = as.Date("2017-02-01")+(0:27)))

image(lon,lat,arr[,,"cc",1], asp = T); map("world", add = T)

range(arr[,,"cc",])



# 2m temperature over the UK: 0h and 24h control forecasts made on May 9th 
cb <- seq(min(pretty(grib$band1, grib$band2)), max(pretty(grib$band1, grib$band2)))

image(lon,lat, array(grib$band5, dim = dm), asp = T); map("world", add = T)
