#!/usr/bin/env python
from ecmwfapi import ECMWFDataServer
server = ECMWFDataServer()

server.retrieve({
    "class"	: "ei",
    "dataset"	: "interim",
    "date"	: "2010-11-01/to/2011-02-28",
    "expver"	: "1",
    "grid"	: "1/1",
    "area"    	: "60/-6/50/2",
    "levtype"	: "sfc",
    "param"	: "151.128/165.128/166.128/167.128",
    "step"	: "0",
    "stream"	: "oper",
    "time"	: "00:00:00/12:00:00",
    "type"	: "an",
    "format"	: "netcdf",
    "target"	: "../ei-2010-step0.nc",
})
