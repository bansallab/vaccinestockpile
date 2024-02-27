
library(raster)
library(rgdal)
library(rgeos)
library(sp)
library(velox)

human <- velox('C:/Users/cjcar/Dropbox/CEPI Vaccine Stockpiling/Nipah/Code for Spillover Layer/human/humanfine.grd')

gadm <- readOGR(dsn='C:/Users/cjcar/Dropbox/CEPI Vaccine Stockpiling/MERS/Country_layers',
                layer='middle_east')

human.pop <- human$extract(sp = gadm, fun = function(x) sum(x, na.rm = TRUE))
