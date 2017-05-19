
# trying to match data downloaded in May 2017 with original dataset

library("ecmwfConversion")

# Kate's data uses midday reanalysis (& presumably forecast?)

# still to try: average over all intervening timesteps?
# the data log refers to 24h average temperature - however this doesn't appear in the download GUI...

########################################################################################################
# ERA-interim MSLP (gridded) : MATCHED                                                              ####

# load data from Kate
{
    load("./99-org-data/kf-era-int/ERAint_mslp_36yr.rda")
    load("./99-org-data/kf-era-int/ERAint_europe_lon.rda")
    load("./99-org-data/kf-era-int/ERAint_europe_lat.rda")
    load("./99-org-data/kf-era-int/ERAint_europe_starttime.rda")
    
    kf.dates <- apply(model_time/24, 1:2, as.Date, origin = "1800-01-01")
    kf.dates[1:5,1:5]
    # these seem to be taken from 12:00 forecast, not 00:00

    image(lon, lat, model_var[,,1,1])
    map("world", add = T)
    abline(h = .5:80.5, v = -50.5:50.5, lty = 2, col = adjustcolor("black", alpha = .4))
    # definitely 1-degree resolution
}

# load new download (Europe only)
ei <- readRDS("./02-objects/step0-ERAint.rds")

# extract slice for single date 
dt <- "2008-12-01"
ind <- which(kf.dates == as.numeric(as.Date(dt, origin = "1800-01-01") + 0.5), arr.ind = T)

kf.mslp <- aaply(model_var[lon %in% dimnames(ei)$lon, lat %in% dimnames(ei)$lat,,], 1:2,"[",ind)
cb.mslp <- ei["msl",,, substr(dt,1,4), gsub("-","",substr(dt,6,10)), "12"]

all(round(cb.mslp, 1) == round(kf.mslp * 100,1))
all(round(cb.mslp, 0) == round(kf.mslp * 100,0))

mslp.diff <- cb.mslp - (kf.mslp*100)
range(mslp.diff)            # well, that's a start. Less than 0.1 in either direction. But still!

ei.mslp <- ei["msl",,,1:8,31:120,"12"]
kf.mslp <- aperm(model_var[lon %in% dimnames(ei)$lon, lat %in% dimnames(ei)$lat, 1:90, 29:36], c(1,2,4,3))

all.diffs <- ei.mslp - (kf.mslp * 100)
range(all.diffs)
# still quite small discrepancy - less than 0.25 - but should be exactly the same!

# but it is a very small discrepancy...
plot(ei.mslp, kf.mslp, pch = 20)

# maybe a rounding error? Think this is close enough to say it's same data, anyway.
ei.mslp[2,1,1,1] 
kf.mslp[2,1,1,1] * 100

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# ECMWF MSLP                                                                                        ####

# load data from Kate
{
    load("./99-org-data/kf-ecmwf/ECMWFctrl_mslp_8yr.rda")
    load("./99-org-data/kf-ecmwf/ECMWF_europe_lat.rda")
    load("./99-org-data/kf-ecmwf/ECMWF_europe_lon.rda")
    load("./99-org-data/kf-ecmwf/ECMWF_europe_starttime.rda")
    load("./99-org-data/kf-ecmwf/ECMWF_europe_leadtime.rda")
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# ERA-interim temp at 850mb : MATCHED                                                               ####

# load newly downloaded data
{
    ncin <- nc_open("./99-org-data/cb-era-int/ei-pressure-varbs.nc")
    names(ncin$dim); names(ncin$var)
    
    tmp <- ncvar_get(ncin, "t")
    dimnames(tmp) <-  sapply(names(ncin$dim), function(dd) ncvar_get(ncin, dd))
    
    nc_close(ncin); remove(ncin)
    # nothing filtered or reordered yet
    
    times <- as.numeric(dimnames(tmp)$time) %% 24
}

# load data from Kate
{
    load("./99-org-data/kf-era-int/ERAint_temp850mb_36yr.rda")
    load("./99-org-data/kf-era-int/ERAint_europe_lon.rda")
    load("./99-org-data/kf-era-int/ERAint_europe_lat.rda")
    load("./99-org-data/kf-era-int/ERAint_europe_starttime.rda")
    
    k.dates <- apply(model_time, 1:2, function(dt) as.character(as.Date(dt/24, origin = "1800-01-01")))
}

cb.tmp <- tmp[lon(tmp) %in% lon, lat(tmp) %in% lat, "850", times == 12]
dt <- as.character(as.Date(as.integer(dimnames(cb.tmp)$time[32])/24, origin = "1900-01-01"))

cb.tmp <- cb.tmp[,36:1,]

k.tmp <- apply(model_var, 1:2, "[", which(k.dates == dt, arr.ind = T))[lon %in% lon(cb.tmp), lat %in% lat(cb.tmp)]

image(cb.tmp[,,32])
image(k.tmp)

all(cb.tmp[,,32] == k.tmp)

plot(cb.tmp[,,32], k.tmp, pch = 20)
# close enough that I'm happy I've got the right download!

range(cb.tmp[,,32] - k.tmp)
# tiny weeny errors. Assume just unpacking/rounding errors.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# load newly downloaded data
{
    ncin <- nc_open("./99-org-data/cb-era-int/ei-pressure-varbs.nc")
    names(ncin$dim); names(ncin$var)
    
    temp <- ncvar_get(ncin, "t")
    
    dn <- sapply(names(ncin$dim), function(dd) ncvar_get(ncin, dd))
    dates <- sapply(dn$time/24, function(hh) gsub("-","", as.Date(hh, origin = "1900-01-01")))
    times <- dn$time %% 24
    dimnames(temp) <- dn; remove(dn)
    
    nc_close(ncin); remove(ncin)
    temp <- temp[,order(lat(temp)),,]
}

# load data from Kate
{
    load("./99-org-data/kf-era-int/ERAint_temp850mb_36yr.rda")
    load("./99-org-data/kf-era-int/ERAint_europe_lon.rda")
    load("./99-org-data/kf-era-int/ERAint_europe_lat.rda")
    load("./99-org-data/kf-era-int/ERAint_europe_starttime.rda")
    
    kf.dates <- apply(model_time/24, 1:2, as.Date, origin = "1800-01-01"); remove(model_time)
    # these seem to be taken from 12:00 forecast, not 00:00
}

# select only data of interest (small subset since dates don't align easily)
cb.temp <- temp[lon(temp) %in% lon, lat(temp) %in% lat, "850", times == 12]
dimnames(cb.temp)$time <- gsub("-","",as.Date(as.numeric(dimnames(cb.temp)$time)/24, origin = "1900-01-01"))

cb.temp <- cb.temp[,,which(dimnames(cb.temp)$time == "20071201"):which(dimnames(cb.temp)$time == "20080228")]
kf.temp <- model_var[lon %in% lon(cb.temp), lat %in% lat(cb.temp), 1:90, 29]

dt <- sapply(model_time[1:90,29], function(dt) as.character(as.Date(dt/24, origin = "1800-01-01")))
all(dimnames(cb.temp)$time == gsub("-","",dt))
# same dates. Same geographical region. Same mb height. How does this not match?!

plot(cb.temp, kf.temp, pch = 20)    # ARGH

image(lon(cb.temp), lat(cb.temp), cb.temp[,,1]); map("world", add = T)
image(lon(cb.temp), lat(cb.temp), kf.temp[,,1]); map("world", add = T)

# le sigh. Le so much sigh.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# ERA-interim geopotential at 1000mb : MATCHED                                                      ####

# load newly downloaded data
{
    ncin <- nc_open("./99-org-data/cb-era-int/ei-pressure-varbs.nc")
    names(ncin$dim); names(ncin$var)
    
    gp <- ncvar_get(ncin, "z")
    dimnames(gp) <-  sapply(names(ncin$dim), function(dd) ncvar_get(ncin, dd))
    
    nc_close(ncin); remove(ncin)
    # nothing filtered or reordered yet
    
    times <- as.numeric(dimnames(gp)$time) %% 24
}

# load data from Kate
{
    load("./99-org-data/kf-era-int/ERAint_geoH1000mb_36yr.rda")
    load("./99-org-data/kf-era-int/ERAint_europe_lon.rda")
    load("./99-org-data/kf-era-int/ERAint_europe_lat.rda")
    load("./99-org-data/kf-era-int/ERAint_europe_starttime.rda")
    
    k.dates <- apply(model_time, 1:2, function(dt) as.character(as.Date(dt/24, origin = "1800-01-01")))
}

cb.gp <- gp[lon(gp) %in% lon, lat(gp) %in% lat, "1000", times == 12]
dt <- as.character(as.Date(as.integer(dimnames(cb.gp)$time[32])/24, origin = "1900-01-01"))

cb.gp <- cb.gp[,36:1,]

k.gp <- apply(model_var, 1:2, "[", which(k.dates == dt, arr.ind = T))[lon %in% lon(cb.gp), lat %in% lat(cb.gp)]

image(cb.gp[,,32])
image(k.gp)

all(cb.gp[,,32] == k.gp)

plot(cb.gp[,,32], k.gp, pch = 20)
# close enough that I'm happy I've got the right download!

range(cb.gp[,,32] - (k.gp * 100))
# error < 0.5 in 1000. Probably just rounding somewhere.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# ECMWF north-south midday temperatures                                                             ####

# load original data
load("../02_papers/01-winter-temps/code/mmeBayes/data/ecmwf.rda")

# filter to select obly January 2009 (according to current labelling)
load("../02_papers/01-winter-temps/code/mmeBayes/data/timestamps.rda")
ecmwf <- ecmwf[1:2,16:105,"08",,-1][,timestamps[,2] %in% dimnames(cb.dat)$date,,]

cb.dat <- nc.array("./01-raw-data/zz_scratch/ecmwf/ec01.nc")[,,,,1:15,] - 273.14

# try aggregating without land-sea mask
{
    ns.all <- abind("temp.n" = apply(cb.dat[,lat(cb.dat) > 54.4,,,], 5:3, mean),
                    "temp.s" = apply(cb.dat[,lat(cb.dat) < 54.4,,,], 5:3, mean), along = 0)
}


########################################################################################################
# ERA-interim 24h average temps                                                                     ####

# tried to download using parameter 'mean2t24'/'55'/'128.55' but none of these worked.
# only 'mean2t24' returned any data, and that gave vertical integral of water vapour.

# try averaging over midday and midnight temps
{
    ei.mtmp <- apply(readRDS("./02-objects/step0-ERAint.rds")["t2m",,,1:8,31:120,], 1:4, mean)
    
    nsm <- ns.mask()
    
    ei.ns.tmp <- abind("temp.n" = apply(sweep(ei.mtmp, 1:2, nsm$n, "*"), 3:4, mean, na.rm = T),
                       "temp.s" = apply(sweep(ei.mtmp, 1:2, nsm$s, "*"), 3:4, mean, na.rm = T), along = 0)
}

load("./99-org-data/kf-era-int/ERAint_temp2m_36yr_north24hrave.rda")
# presumably 90 days x 4 timesteps?



# ECMWF 24h average temps                                                                           ####

# load new data
{
    grib.convert("./99-org-data/cb-era-int/ecmwf-2010-cf-mean2t24.grib")
    
    nc.inspect("./99-org-data/cb-era-int/ecmwf-2010-cf-mean2t24.nc")
    mtmp <- nc.array("./99-org-data/cb-era-int/ecmwf-2010-cf-mean2t24.nc")
    
    ncin <- nc_open("./99-org-data/cb-era-int/ecmwf-2010-cf-mean2t24.nc")
    
    names(ncin$dim); names(ncin$var)
    
    sapply(names(ncin$var), function(varb) ncatt_get(ncin, varb))
    
    unlist(ncatt_get(ncin, "msl"))
    nc.inspect("./99-org-data/cb-era-int/ecmwf-mean2t24.nc")
}


# load data from Kate
{
    load("./99-org-data/kf-era-int/ERAint_mslp_36yr.rda")
    load("./99-org-data/kf-era-int/ERAint_europe_lon.rda")
    load("./99-org-data/kf-era-int/ERAint_europe_lat.rda")
    load("./99-org-data/kf-era-int/ERAint_europe_starttime.rda")
    
    kf.dates <- apply(model_time/24, 1:2, as.Date, origin = "1800-01-01")
    kf.dates[1:5,1:5]
    # these seem to be taken from 12:00 forecast, not 00:00
    
    image(lon, lat, model_var[,,1,1])
    map("world", add = T)
    abline(h = .5:80.5, v = -50.5:50.5, lty = 2, col = adjustcolor("black", alpha = .4))
    # definitely 1-degree resolution
}