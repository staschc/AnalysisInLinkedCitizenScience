?missing

library(spacetime)
library(maptools)
fname = system.file("shapes/sids.shp", package="maptools")[1]
nc = readShapePoly(fname, proj4string=CRS("+proj=longlat +datum=NAD27"))
summary(nc)
time = as.POSIXct(c("1974-07-01", "1979-07-01"), tz = "GMT")
endTime = as.POSIXct(c("1978-06-30", "1984-06-30"), tz = "GMT")
data = data.frame(BIR = c(nc$BIR74, nc$BIR79),NWBIR = c(nc$NWBIR74, nc$NWBIR79),SID = c(nc$SID74, nc$SID79))
nct = STFDF(sp = as(nc, "SpatialPolygons"), time, data, endTime)
summary(nct)
