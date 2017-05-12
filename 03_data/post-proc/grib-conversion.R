
library(rgdal); library(maps)

########################################################################################################
# Mini data set: single location, timestep, parameter                                               ####

grib <- readGDAL("TIGGE/ECMWF.grib")

lat <- 51:58; lon <- -6:1

# 2m temperature over the UK: 0h and 24h control forecasts made on May 9th 
cb <- seq(min(pretty(grib$band1, grib$band2)), max(pretty(grib$band1, grib$band2)))

image(lon,lat,array(grib$band1, dim = c(8,8)), asp = T, main = "9-May-17: 0h", 
      col = heat.colors(length(cb)-1), breaks = cb); map("world", add = T)
image(lon,lat,array(grib$band2, dim = c(8,8)), asp = T, main = "9-May-17: 24h", 
      col = heat.colors(length(cb)-1), breaks = cb); map("world", add = T)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Full data set with multiple dates, parameters & timesteps                                         ####

grib <- readGDAL("../../03_data/TIGGE/ECMWF-2017-02.grib")

plot(grib$band1)

image(x = c(-6:2), y = c(50:60), z = array(grib$band1, dim = c(9,11)), asp = T)
map("world", add = T)

# 151 - MSLP
# 165 - 10m u-component
# 166 - 10m v-component
# 167 - 2m temperature
# 228164 - total cloud cover
# 228228 - total precipitation

# seems to repeat after every 6 fields, so presumably orography & lsm don't repeat
# 228002 - orography
# 172 - land-sea mask

# 6 fields
# 16 timesteps > 96 columns per timestep
# 50 ensemble members
# 28 dates
6*16 * 50 * 28


gr <- attributes(grib)$grid
# 
