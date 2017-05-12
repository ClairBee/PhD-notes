#!/usr/bin/env python

from ecmwfapi import ECMWFDataServer
server = ECMWFDataServer()

server.retrieve({
    "class"   : "ti",
    "dataset" : "tigge",
    "type"    : "pf", 
    "date"    : "2017-05-09",
    "expver"  : "prod",
    "grid"    : "1/1",
    "area"    : "58/-6/51/1",
    "levtype" : "sfc",
    "origin"  : "ecmf",
    "param"   : "172/228002",
    "step"    : "0/24",
    "time"    : "00:00:00",
    "target"  : "../TIGGE/temp.grib",
})

# ECMWF orography & land-sea mask from May 2017 (check if same as downloaded for Feb)
# Also testing whether relative filepaths are acceptable 
