# estuary_calc_shore_dist.R#####
#
# Author: Mark Myer
#
# Adapted from code written by: John Darling
#
# Purpose: To calculate the maximum Euclidean distance to shore and maximum pixel window width accomodated by estuary waterbodies of the U.S.
# Revision: October 30, 2018
# R Version 3.5.1 Feather Spray

library(raster)
library(rgdal)
library(scales) # only needed for percentage formatting

setwd("[your working drive]/Estuaries")

CalcShoreDist <- function(index, wbs, out.dir){
  # Intended to be called via lapply(seq(1, nrow(wbs)), ...). The iterator is left
  # outside the function to allow the user to resume processing after interuption.  
  #
  # Iterates through a shapefile by row:
  #  1. Rasterizes a single feature 
  #  2. Calculates the maximum euclidian distance to shore
  #  3. Appends COMID and max distance (m) to csv
  # index = integer, row of the waterbody shapefile to subset and process
  # wbs = shapefile of all waterbodies
  #
  wb <- wbs[index, ]
  print(paste0("Processing Estuary ", wb$OBJECTID))
  # create a blank raster for rasterizing and masking
  blank.raster <- raster(extent(wb)+c(-15,15,-15,15), crs = "+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83 +units=m +no_defs", resolution =15, vals = 1)
  print("rasterizing")
  wb.raster <- rasterize(wb, blank.raster, 'OBJECTID')
  print("masking")
  landmask <- raster::mask(blank.raster, mask = wb.raster, inverse = TRUE)
  print("calculating distance")
  shore.dist <- distance(landmask, doEdge=TRUE)
  print("finding max distance")
  max.shore.dist <- raster::zonal(shore.dist, wb.raster, fun='max')
  print(paste0(percent(index/nrow(wbs)), " of waterbodies processed."))
  # scientific = FALSE is critical to prevent innapropriate rounding (eg comid 21000001, 21000003 recorded as 2.1+e07)
  write(format(max.shore.dist, scientific = FALSE), paste0(out.dir, "/wb_shore_dist.txt"), append = T, ncolumns = 2)
}

# define a function to calculate the maximum window size a water body can accomodate based on its maximum shore distance
maxWindow <- function(shore.dist) (shore.dist / (sqrt(2)/2))

# NOAA estuary dataset for the Atlantic coast ------------
# set an output directory.
out.dir <- "[your working drive]/Estuaries/Atlantic"
dir.create(out.dir)

noaa.wb.atlantic<- shapefile("[your working drive]/Estuaries/Atlantic/ATL_estuaries.shp")

crs = "+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
#reproject shapefile to match the raster

est.wbs.atl = spTransform(noaa.wb.atlantic, crs)

# Loop through polygons 
est.seq.atl <- seq(1, nrow(est.wbs.atl))
est.wbs.shoredist.atl <- lapply(est.seq.atl, CalcShoreDist, wbs = est.wbs.atl, out.dir = out.dir)
# to resume after interruption, something like: est.seq <- seq(nrow(shore.dist.tbl.atl) + 1, nrow(est.wbs))

#### 
shore.dist.tbl.atl <- read.table(paste0(out.dir, "/wb_shore_dist.txt"), col.names = c('objectid', 'shore_dist'))
# cleanup, need to discard some trailing zeros
shore.dist.tb.atl$objectid <- as.integer(shore.dist.tbl.atl$objectid)
# check if all comids are replicated in the table
shore.dist.check <- shore.dist.tbl.atl[!(shore.dist.tbl.atl$estcode %in% est.wbs.atl$ESTCODE), ]
range(shore.dist.tbl.atl$shore_dist)
# list the maximum window width that can be accomodated by each water body
shore.dist.tbl.atl$maxwindow = maxWindow(shore.dist.tbl.atl$shore_dist)
# merge with shapefile by objectid
est.wbs.shore.dist.spdf.atl <- merge(est.wbs.atl, shore.dist.tbl.atl, by.x = 'OBJECTID', by.y = 'objectid')

writeOGR(est.wbs.shore.dist.spdf.atl, dsn = out.dir, layer = "Atlantic_estuary_shore_dist", driver = "ESRI Shapefile")


#Atlantic Subestuaries-----
# set an output directory.  recommend isolating from other outputs.
out.dir <- "[your working drive]/Estuaries/Atlantic/Subestuaries"
dir.create(out.dir)

noaa.wb.atlantic.sub<- shapefile("[your working drive]/Estuaries/Atlantic/Subestuaries/ATL_subestuaries.shp")

crs = "+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
#reproject shapefile to match the raster

est.wbs.atl.sub = spTransform(noaa.wb.atlantic.sub, crs)

# Loop through polygons 
est.seq.atl <- seq(1, nrow(est.wbs.atl.sub))
est.wbs.shoredist.atl.sub <- lapply(est.seq.atl.sub, CalcShoreDist, wbs = est.wbs.atl.sub, out.dir = out.dir)
# to resume after interruption, something like: est.seq <- seq(nrow(shore.dist.tbl.atl.sub) + 1, nrow(est.wbs))

#### 
shore.dist.tbl.atl.sub <- read.table(paste0(out.dir, "/wb_shore_dist.txt"), col.names = c('objectid', 'shore_dist'))
# cleanup, need to discard some trailing zeros
shore.dist.tb.atl.sub$objectid <- as.integer(shore.dist.tbl.atl.sub$objectid)
# check if all comids are replicated in the table
shore.dist.check <- shore.dist.tbl.atl.sub[!(shore.dist.tbl.atl.sub$estcode %in% est.wbs.atl.sub$ESTCODE), ]
range(shore.dist.tbl.atl.sub$shore_dist)
# list the maximum window width that can be accomodated by each water body
shore.dist.tbl.atl.sub$maxwindow = maxWindow(shore.dist.tbl.atl.sub$shore_dist)
# merge with shapefile by objectid
est.wbs.shore.dist.spdf.atl.sub <- merge(est.wbs.atl.sub, shore.dist.tbl.atl.sub, by.x = 'OBJECTID', by.y = 'objectid')

writeOGR(est.wbs.shore.dist.spdf.atl.sub, dsn = out.dir, layer = "Atlantic_subestuary_shore_dist", driver = "ESRI Shapefile")

# NOAA estuary dataset for Gulf of Mexico------------
out.dir <- "[your working drive]/Estuaries/Gulf"
dir.create(out.dir)

noaa.wb.gom<- shapefile("[your working drive]/Estuaries/Gulf/GOM_estuaries.shp")
noaa.wb.gom@data$OBJECTID <- 1:nrow(noaa.wb.gom@data)
crs = "+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
#reproject shapefile to match the raster

est.wbs.gom = spTransform(noaa.wb.gom, crs)
#est.wbs.gom = est.wbs.gom[-1,] Uncomment this if the first raster, a shoreline, causes function to choke

# Loop through polygons 
est.seq <- seq(1, nrow(est.wbs.gom)) 
est.wbs.gom.shoredist <- lapply(est.seq, CalcShoreDist, wbs = est.wbs.gom, out.dir = out.dir)
# to resume after interruption, something like: est.seq <- seq(nrow(shore.dist.tbl.gom) + 1, nrow(est.wbs.gom))

#### 
shore.dist.tbl.gom <- read.table(paste0(out.dir, "/wb_shore_dist.txt"), col.names = c('objectid', 'shore_dist'))
# cleanup, need to discard some trailing zeros
shore.dist.tbl.gom$objectid <- as.integer(shore.dist.tbl.gom$objectid)
# check if all comids are replicated in the table
shore.dist.check <- shore.dist.tbl.gom[!(shore.dist.tbl.gom$estcode %in% est.wbs.gom$ESTCODE), ]
range(shore.dist.tbl.gom$shore_dist)
# list the maximum window width that can be accomodated by each water body
shore.dist.tbl.gom$maxwindow = maxWindow(shore.dist.tbl.gom$shore_dist)
# merge with shapefile by objectid
est.wbs.gom.shore.dist.spdf.gom <- merge(est.wbs.gom, shore.dist.tbl.gom, by.x = 'OBJECTID', by.y = 'objectid')

writeOGR(est.wbs.gom.shore.dist.spdf.gom, dsn = out.dir, layer = "Gulf_estuary_shore_dist", driver = "ESRI Shapefile")

#Gulf Subestuaries-----
# set an output directory.  recommend isolating from other outputs.
out.dir <- "[your working drive]/Estuaries/Gulf/Subestuaries"
dir.create(out.dir)

noaa.wb.gulf.sub<- shapefile("[your working drive]/Estuaries/Gulf/Subestuaries/GOM_subestuaries.shp")

crs = "+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
#reproject shapefile to match the raster

est.wbs.gulf.sub = spTransform(noaa.wb.gulf.sub, crs)

# Loop through polygons 
est.seq.gulf <- seq(1, nrow(est.wbs.gulf.sub))
est.wbs.shoredist.gulf.sub <- lapply(est.seq.gulf.sub, CalcShoreDist, wbs = est.wbs.gulf.sub, out.dir = out.dir)
# to resume after interruption, something like: est.seq <- seq(nrow(shore.dist.tbl.gulf.sub) + 1, nrow(est.wbs))

#### 
shore.dist.tbl.gulf.sub <- read.table(paste0(out.dir, "/wb_shore_dist.txt"), col.names = c('objectid', 'shore_dist'))
# cleanup, need to discard some trailing zeros
shore.dist.tb.gulf.sub$objectid <- as.integer(shore.dist.tbl.gulf.sub$objectid)
# check if all comids are replicated in the table
shore.dist.check <- shore.dist.tbl.gulf.sub[!(shore.dist.tbl.gulf.sub$estcode %in% est.wbs.gulf.sub$ESTCODE), ]
range(shore.dist.tbl.gulf.sub$shore_dist)
# list the maximum window width that can be accomodated by each water body
shore.dist.tbl.gulf.sub$maxwindow = maxWindow(shore.dist.tbl.gulf.sub$shore_dist)
# merge with shapefile by objectid
est.wbs.shore.dist.spdf.gulf.sub <- merge(est.wbs.gulf.sub, shore.dist.tbl.gulf.sub, by.x = 'OBJECTID', by.y = 'objectid')

writeOGR(est.wbs.shore.dist.spdf.gulf.sub, dsn = out.dir, layer = "Gulf_subestuary_shore_dist", driver = "ESRI Shapefile")

# NOAA estuary dataset for Pacific Coast-----------------
out.dir <- "[your working drive]/Estuaries/Pacific"
dir.create(out.dir)

noaa.wb.pac<- shapefile("[your working drive]/Estuaries/Pacific/PAC_estuaries.shp")
noaa.wb.pac@data$OBJECTID <- 1:nrow(noaa.wb.pac@data)
crs = "+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
#reproject shapefile to match the raster

est.wbs.pac = spTransform(noaa.wb.pac, crs)

# Loop through polygons
est.seq <- seq(1, nrow(est.wbs.pac))
est.wbs.pac.shoredist.pac <- lapply(est.seq, CalcShoreDist, wbs = est.wbs.pac, out.dir = out.dir)
# to resume after interruption, something like: est.seq <- seq(nrow(shore.dist.tbl.pac) + 1, nrow(est.wbs.pac))

#### 
shore.dist.tbl.pac <- read.table(paste0(out.dir, "/wb_shore_dist.txt"), col.names = c('objectid', 'shore_dist'))
# cleanup, need to discard some trailing zeros
shore.dist.tbl.pac$objectid <- as.integer(shore.dist.tbl.pac$objectid)
# check if all comids are replicated in the table
shore.dist.check <- shore.dist.tbl.pac[!(shore.dist.tbl.pac$estcode %in% est.wbs.pac$ESTCODE), ]
range(shore.dist.tbl.pac$shore_dist)
# list the maximum window width that can be accomodated by each water body
shore.dist.tbl.pac$maxwindow = maxWindow(shore.dist.tbl.pac$shore_dist)
# merge with shapefile by objectid
est.wbs.pac.shore.dist.spdf.pac <- merge(est.wbs.pac, shore.dist.tbl.pac, by.x = 'OBJECTID', by.y = 'objectid')

writeOGR(est.wbs.pac.shore.dist.spdf.pac, dsn = out.dir, layer = "Pacific_estuary_shore_dist", driver = "ESRI Shapefile")

#Pacific Subestuaries-----
# set an output directory.  recommend isolating from other outputs.
out.dir <- "[your working drive]/Estuaries/Pacific/Subestuaries"
dir.create(out.dir)

noaa.wb.pacific.sub<- shapefile("[your working drive]/Estuaries/Pacific/Subestuaries/PAC_subestuaries.shp")

crs = "+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
#reproject shapefile to match the raster

est.wbs.pac.sub = spTransform(noaa.wb.pacific.sub, crs)

# Loop through polygons 
est.seq.pac <- seq(1, nrow(est.wbs.pac.sub))
est.wbs.shoredist.pac.sub <- lapply(est.seq.pac.sub, CalcShoreDist, wbs = est.wbs.pac.sub, out.dir = out.dir)
# to resume after interruption, something like: est.seq <- seq(nrow(shore.dist.tbl.pac.sub) + 1, nrow(est.wbs))

#### 
shore.dist.tbl.pac.sub <- read.table(paste0(out.dir, "/wb_shore_dist.txt"), col.names = c('objectid', 'shore_dist'))
# cleanup, need to discard some trailing zeros
shore.dist.tb.pac.sub$objectid <- as.integer(shore.dist.tbl.pac.sub$objectid)
# check if all comids are replicated in the table
shore.dist.check <- shore.dist.tbl.pac.sub[!(shore.dist.tbl.pac.sub$estcode %in% est.wbs.pac.sub$ESTCODE), ]
range(shore.dist.tbl.pac.sub$shore_dist)
# list the maximum window width that can be accomodated by each water body
shore.dist.tbl.pac.sub$maxwindow = maxWindow(shore.dist.tbl.pac.sub$shore_dist)
# merge with shapefile by objectid
est.wbs.shore.dist.spdf.pac.sub <- merge(est.wbs.pac.sub, shore.dist.tbl.pac.sub, by.x = 'OBJECTID', by.y = 'objectid')

writeOGR(est.wbs.shore.dist.spdf.pac.sub, dsn = out.dir, layer = "Pacific_subestuary_shore_dist", driver = "ESRI Shapefile")


