#!/usr/bin/env python

from ecmwfapi import ECMWFDataServer
server = ECMWFDataServer()

server.retrieve({
    "class"   : "ti",
    "dataset" : "tigge",
    "type"    : "cf", 
    "date"    : "2017-05-09",
    "expver"  : "prod",
    "grid"    : "1/1",
    "area"    : "58/-6/51/1",
    "levtype" : "sfc",
    "origin"  : "ecmf",
    "param"   : "167",
    "step"    : "0/24",
    "time"    : "00:00:00",
    "target"  : "ECMWF.grib",
})

# download 2m temperature forecast for all ECMWF perturbed forecast members at 1-degree resolution over the whole UK (one date, one forecast time, one forecast step)
