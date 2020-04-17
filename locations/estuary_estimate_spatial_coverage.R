# estimate_spatial_coverage.R#####
#
# Author: Mark Myer
#
# Adapted from code written by: John Darling
#
# Purpose: To tabulate the number of coastal estuarine and subestuarine waterbodies of the U.S. that can be resolved by various satellite imagery image resolutions.
# Revision: March 20, 2019
# R Version 3.5.1 Feather Spray

library(raster)
library(tables)
library(splitstackshape) # for expandRows()

out.dir <- "[Your Working Directory]"
setwd(out.dir)

# Calculates the radius of the circumcircle for a square of length window.width
minDist <- function(window.width) (sqrt(2*window.width^2))/2

#Determine minimum window sizes for each estuary coastline
#Atlantic -----
nhd.wb.df.ATL <- shapefile(paste0(out.dir,"Atlantic/Atlantic_estuary_shore_dist.shp"))
nhd.wb.df.ATL$w30 <- nhd.wb.df.ATL$shore_dist >= minDist(30) 
nhd.wb.df.ATL$w90 <- nhd.wb.df.ATL$shore_dist >= minDist(90)
nhd.wb.df.ATL$w300 <- nhd.wb.df.ATL$shore_dist >= minDist(300)
nhd.wb.df.ATL$w900 <- nhd.wb.df.ATL$shore_dist >= minDist(900)
nhd.wb.df.ATL$w1k <- nhd.wb.df.ATL$shore_dist >= minDist(1000) 
nhd.wb.df.ATL$w3k <- nhd.wb.df.ATL$shore_dist >= minDist(3000) 
atlantic.df<-nhd.wb.df.ATL@data
atlantic.df$Coast <- rep("Atlantic",nrow(atlantic.df))
rm(nhd.wb.df.ATL)

#Gulf of Mexico-----
nhd.wb.df.GOM <- shapefile(paste0(out.dir,"Gulf/Gulf_estuary_shore_dist.shp"))
nhd.wb.df.GOM$w30 <- nhd.wb.df.GOM$shore_dist >= minDist(30) 
nhd.wb.df.GOM$w90 <- nhd.wb.df.GOM$shore_dist >= minDist(90)
nhd.wb.df.GOM$w300 <- nhd.wb.df.GOM$shore_dist >= minDist(300)
nhd.wb.df.GOM$w900 <- nhd.wb.df.GOM$shore_dist >= minDist(900) 
nhd.wb.df.GOM$w1k <- nhd.wb.df.GOM$shore_dist >= minDist(1000) 
nhd.wb.df.GOM$w3k <- nhd.wb.df.GOM$shore_dist >= minDist(3000) 
gulf.df<-nhd.wb.df.GOM@data
gulf.df$Coast <- rep("Gulf of Mexico",nrow(gulf.df))
rm(nhd.wb.df.GOM)

#Pacific-----
nhd.wb.df.PAC <- shapefile(paste0(out.dir,"Pacific/Pacific_estuary_shore_dist.shp"))
nhd.wb.df.PAC$w30 <- nhd.wb.df.PAC$shore_dist >= minDist(30) 
nhd.wb.df.PAC$w90 <- nhd.wb.df.PAC$shore_dist >= minDist(90)
nhd.wb.df.PAC$w300 <- nhd.wb.df.PAC$shore_dist >= minDist(300)
nhd.wb.df.PAC$w900 <- nhd.wb.df.PAC$shore_dist >= minDist(900) 
nhd.wb.df.PAC$w1k<- nhd.wb.df.PAC$shore_dist >= minDist(1000) 
nhd.wb.df.PAC$w3k<- nhd.wb.df.PAC$shore_dist >= minDist(3000) 
pacific.df<-nhd.wb.df.PAC@data
pacific.df$Coast <- rep("Pacific",nrow(pacific.df))
rm(nhd.wb.df.PAC)

#Create a table of percent coverages at each window size----
estuaries.df<-rbind(atlantic.df,gulf.df,pacific.df)
rm(atlantic.df,gulf.df,pacific.df)

#Create the table using tabular()
est.coverage.table <- tabular((Heading("Coast") * as.factor(Coast) + 1) 
                              ~ Heading("Window Size") * 
                                (Heading("30x30") * w30 + Heading("90x90") * w90 + Heading("300x300") * w300 + Heading("900x900") * w900 + Heading("1 Sq.Km") * w1k + Heading("3x3 Km") * w3k) * 
                                (Heading("n")*1 +Heading("%")*
                                Format(digits = 2)*Percent("row")), data=estuaries.df)
#table as a latex expression
latex(est.coverage.table)

write.csv.tabular(est.coverage.table, paste0(out.dir,"estuary_estimated_coverage.csv"))

#Determine minimum window sizes for each subestuary coastline-----
#Atlantic subestuaries -----
nhd.wb.df.sub.ATL <- shapefile(paste0(out.dir,"Atlantic/Subestuaries/Atlantic_subestuary_shore_dist.shp"))
nhd.wb.df.sub.ATL$w30 <- nhd.wb.df.sub.ATL$shore_dist >= minDist(30) 
nhd.wb.df.sub.ATL$w90 <- nhd.wb.df.sub.ATL$shore_dist >= minDist(90)
nhd.wb.df.sub.ATL$w300 <- nhd.wb.df.sub.ATL$shore_dist >= minDist(300)
nhd.wb.df.sub.ATL$w900 <- nhd.wb.df.sub.ATL$shore_dist >= minDist(900)
nhd.wb.df.sub.ATL$w1k <- nhd.wb.df.sub.ATL$shore_dist >= minDist(1000) 
nhd.wb.df.sub.ATL$w3k <- nhd.wb.df.sub.ATL$shore_dist >= minDist(3000) 
atlantic.sub.df<-nhd.wb.df.sub.ATL@data
atlantic.sub.df$Coast <- rep("Atlantic Subestuaries",nrow(atlantic.sub.df))


#Gulf of Mexico Subestuaries-----
nhd.wb.df.sub.GOM <- shapefile(paste0(out.dir,"Gulf/Subestuaries/Gulf_subestuary_shore_dist.shp"))
nhd.wb.df.sub.GOM$w30 <- nhd.wb.df.sub.GOM$shore_dist >= minDist(30) 
nhd.wb.df.sub.GOM$w90 <- nhd.wb.df.sub.GOM$shore_dist >= minDist(90)
nhd.wb.df.sub.GOM$w300 <- nhd.wb.df.sub.GOM$shore_dist >= minDist(300)
nhd.wb.df.sub.GOM$w900 <- nhd.wb.df.sub.GOM$shore_dist >= minDist(900) 
nhd.wb.df.sub.GOM$w1k <- nhd.wb.df.sub.GOM$shore_dist >= minDist(1000) 
nhd.wb.df.sub.GOM$w3k <- nhd.wb.df.sub.GOM$shore_dist >= minDist(3000) 
gulf.sub.df<-nhd.wb.df.sub.GOM@data
gulf.sub.df$Coast <- rep("Gulf of Mexico Subestuaries",nrow(gulf.sub.df))


#Pacific Subestuaries-----
nhd.wb.df.sub.PAC <- shapefile(paste0(out.dir,"Pacific/Subestuaries/Pacific_subestuary_shore_dist.shp"))
nhd.wb.df.sub.PAC$w30 <- nhd.wb.df.sub.PAC$shore_dist >= minDist(30) 
nhd.wb.df.sub.PAC$w90 <- nhd.wb.df.sub.PAC$shore_dist >= minDist(90)
nhd.wb.df.sub.PAC$w300 <- nhd.wb.df.sub.PAC$shore_dist >= minDist(300)
nhd.wb.df.sub.PAC$w900 <- nhd.wb.df.sub.PAC$shore_dist >= minDist(900) 
nhd.wb.df.sub.PAC$w1k<- nhd.wb.df.sub.PAC$shore_dist >= minDist(1000) 
nhd.wb.df.sub.PAC$w3k<- nhd.wb.df.sub.PAC$shore_dist >= minDist(3000) 
pacific.sub.df<-nhd.wb.df.sub.PAC@data
pacific.sub.df$Coast <- rep("Pacific Subestuaries",nrow(pacific.sub.df))

#Create a table of percent coverages at each window size----
subestuaries.df<-rbind(atlantic.sub.df,gulf.sub.df,pacific.sub.df)
rm(atlantic.sub.df,gulf.sub.df,pacific.sub.df)

#Create the table using tabular()
subest.coverage.table <- tabular((Heading("Coast") * as.factor(Coast) + 1) 
                              ~ Heading("Window Size") * 
                                (Heading("30x30") * w30 + Heading("90x90") * w90 + Heading("300x300") * w300 + Heading("900x900") * w900 + Heading("1 Sq.Km") * w1k + Heading("3x3 Km") * w3k) * 
                                (Heading("n")*1 +Heading("%")*
                                   Format(digits = 2)*Percent("row")), data=subestuaries.df)
#table as a latex expression
latex(subest.coverage.table)

write.csv.tabular(subest.coverage.table, paste0(out.dir,"subestuary_estimated_coverage.csv"))



