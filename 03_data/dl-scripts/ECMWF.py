#!/usr/bin/env python

from ecmwfapi import ECMWFDataServer
server = ECMWFDataServer()

server.retrieve({
    "class": "ti",
    "dataset": "tigge",
    "date": "2017-02-01/to/2017-02-28",
    "expver": "prod",
    "grid": "1/1",
    "area"    : "60/-6/50/2",
    "levtype": "sfc",
    "origin": "ecmf",
    "param": "151/165/166/167/172/228002/228164/228228",
    "step": "0/24/48/72/96/120/144/168/192/216/240/264/288/312/336/360",
    "time": "00:00:00",
    "type": "pf",
    "target": "ECMWF-2017-02.grib",
})

# download various forecast quantities for all ECMWF perturbed forecast members at 1-degree resolution over the whole UK; also includes orography & land-sea mask fields (which hopefully only need to be downloaded once)


