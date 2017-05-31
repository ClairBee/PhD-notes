
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
# ECMWF MSLP : MATCHED                                                                              ####

# load data from Kate
{
    load("./99-org-data/kf-ecmwf/ECMWFctrl_mslp_8yr.rda")
    load("./99-org-data/kf-ecmwf/ECMWF_europe_lat.rda")
    load("./99-org-data/kf-ecmwf/ECMWF_europe_lon.rda")
    load("./99-org-data/kf-ecmwf/ECMWF_europe_starttime.rda")
    load("./99-org-data/kf-ecmwf/ECMWF_europe_leadtime.rda")
}

nc.inspect("./99-org-data/cb-era-int/ecmwf-cf-temp-mslp.nc")

cb.msl <- nc.array("./99-org-data/cb-era-int/ecmwf-cf-temp-mslp.nc")["msl",,,,,]

dt <- "20090101"
k.ind <- which(apply(model_time, 1:2, function(mt) gsub("-","",as.Date(mt/24, origin = "1800-01-01"))) == dt, arr.ind = T)

cb.sl <- cb.msl[lon(cb.msl) %in% lon, lat(cb.msl) %in% lat,,"12",dt] 
kf.sl <- apply(model_var[lon %in% lon(cb.sl), lat %in% lat(cb.sl), , ,], 1:3, "[", k.ind)

image(lon(cb.sl), lat(cb.sl), cb.sl[,,1])
image(lon(cb.sl), lat(cb.sl), kf.sl[,,1])

plot(cb.sl, kf.sl, pch = 20)
# yup. Rounding/unpacking again.

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

########################################################################################################
# ERA-int north-south temperatures (step 0)                                                         ####

# load data from Kate
{
    load("./99-org-data/kf-era-int/ERAint_temp2m_36yr_north24hrave.rda")
    load("./99-org-data/kf-era-int/ERAint_small_lon.rda")
    load("./99-org-data/kf-era-int/ERAint_small_lat.rda")
    load("./99-org-data/kf-era-int/ERAint_small_starttime.rda")
}

# load new download
{
    ncin <- nc_open("./99-org-data/cb-era-int/ei-temps-all-times.nc")
    # names(ncin$dim); names(ncin$var)
    
    t2m <- ncvar_get(ncin, "t2m")
    dimnames(t2m) <- sapply(names(ncin$dim), function(dd) ncvar_get(ncin, dd))
    
    nc_close(ncin); remove(ncin)
    # nothing filtered or reordered yet
    
    t2m <- t2m[,21:1,]
    
    times <- as.numeric(dimnames(t2m)$time) %% 24
}

# looking at dims of model_time and model_var, suspect that each 'mean' is average of t+[0:3]
mean.t2m <- abind(invisible(sapply(1:718, function(dd) {
   apply(t2m[,,dd+(0:3)], 1:2, mean)
}, simplify = F)), rev.along = 0)
dimnames(mean.t2m)[[3]] <- sapply(as.numeric(dimnames(t2m)$time), function(dt) gsub("-", "", as.Date(dt/24, origin = "1900-01-01")))[1:718]
names(dimnames(mean.t2m)) <- c("longitude", "latitude", "date")
#mean.t2m <- mean.t2m[,,times[1:718] == 12]  # single time only

# apply latitude weights to n/s mask
lat.wt <- cos(lat * pi/180)
nsm <- ns.mask()
nsm.n <- sweep(nsm$n, 2, cos(lat(nsm$n)/90 * pi/2), "*")
nsm.s <- sweep(nsm$s, 2, cos(lat(nsm$s)/90 * pi/2), "*")

mt.n <- sweep(mean.t2m[lon(mean.t2m) %in% lon(nsm$n), lat(mean.t2m) %in% lat(nsm$n),],
              1:2, nsm$n, "*")
mt.n <- apply(mt.n, 3, weighted.mean, w = nsm.n, na.rm = T)
mt.s <- apply(sweep(mean.t2m[lon(mean.t2m) %in% lon(nsm$s), lat(mean.t2m) %in% lat(nsm$s),],
                    1:2, nsm$s, "*"), 3, mean, na.rm = T)
mt.s <- apply(mt.s, 3, weighted.mean, w = nsm.s, na.rm = T)

plot(mt.n[361:720], type = "l")
lines(model_var[,31], col = "red")
# so close...

load("./99-org-data/kf-era-int/ERAint_temp2m_36yr_south24hrave.rda")

plot(mt.s[1:360], type = "l")
lines(model_var[,31], col = "red")

names(mt.n)

as.Date(model_time[1,31]/24, origin = "1800-01-01")

t.ext <- sweep(t2m[lon(t2m) %in% lon(nsm$n),lat(t2m) %in% lat(nsm$n),1:4], 1:2, nsm$n, "*")
as.Date(as.numeric(dimnames(t.ext)$time[1])/24, origin="1900-01-01")


image(t.ext[,,1])

# double-check model dates & times align
model_time[2,31] %% 24
as.numeric(dimnames(t.ext)$time[2]) %% 24

plot(mt.s[1:360], model_var[,31], pch = 20)

load("./99-org-data/kf-era-int/ERAint_temp2m_36yr_north24hrave.rda")
plot(mt.n[1:360], model_var[,31], pch = 20)

# what about averaging without first applying the land-sea mask?
range(mt.s[1:360] - model_var[,31])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# ERA-int north-south temperatures (step 6 & 0)                                                     ####

# possible that instead of averaging over all reanalyses at step 0,
# we should average over step 0 and step 6 from midnight and midday?

# load new data
{
    grib.convert("./99-org-data/cb-era-int/ei-06-temps")
    ncin <- nc_open("./99-org-data/cb-era-int/ei-06-temps.nc")
    t6 <- ncvar_get(ncin, "t2m")
    dimnames(t6) <- sapply(names(ncin$dim), function(dd) ncvar_get(ncin, dd))
    nc_close(ncin); remove(ncin)
}

# load data from Kate
{
    load("./99-org-data/kf-era-int/ERAint_temp2m_36yr_north24hrave.rda"); kf.n <- model_var
    load("./99-org-data/kf-era-int/ERAint_temp2m_36yr_south24hrave.rda"); kf.s <- model_var
    
    load("./99-org-data/kf-era-int/ERAint_small_lon.rda")
    load("./99-org-data/kf-era-int/ERAint_small_lat.rda")
    load("./99-org-data/kf-era-int/ERAint_small_starttime.rda")
}

nsm <- ns.mask()

t6.m <- apply(t6, c(1:2,5), mean)           # 24h mean at each gridpoint
t6.m <- t6.m[lon(t6.m) %in% lon(nsm$n), lat(t6.m) %in% lat(nsm$n),]
t6m.n <- apply(sweep(t6.m, 1:2, nsm$n, "*"), 3, mean, na.rm = T)

as.Date(model_time[1,31]/24, origin = "1800-01-01")
as.Date(as.integer(names(t6m.n)[1]), origin = "1900-01-01")
kfnn <- kf.n[1,1]

# NOPE

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
    grib.convert("./99-org-data/cb-era-int/ECMWF-cf-alltemps.grib")
    
    nc.inspect("./99-org-data/cb-era-int/ECMWF-cf-alltemps.nc")
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