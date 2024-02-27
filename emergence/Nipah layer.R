
library(rgdal)
library(raster)
library(scales)

base <- readOGR('C:/Users/cjcar/Dropbox/CEPI Vaccine Stockpiling/Nipah/Clean_shp/clean_ind_bgd_shp.shp')
mammals <- readOGR('C:/Users/cjcar/Dropbox/HowManyHelminths2019/TERRESTRIAL_MAMMALS.shp')

hostlist <- c('Scotophilus kuhlii',
              'Rhinolophus sinicus',
              'Rhinolophus affinis',
              'Hipposideros pomona',
              'Hipposideros armiger',
              'Hipposideros larvatus',
              'Cynopterus sphinx',
              'Cynopterus brachyotis',
              'Eonycteris spelaea',
              'Rousettus leschenaultii',
              'Pteropus medius')

mammals.sub <- mammals[mammals$binomial %in% hostlist,]

require("rgdal")
require("rgeos")
require("dplyr")

hosts <- maptools::unionSpatialPolygons(mammals.sub, mammals.sub@data$binomial)

base@proj4string <- mammals.sub@proj4string

l= rlist::list.cbind(lapply(c(1:10), function(i) {over(base,hosts[i,])}))
l[is.na(l)] <- 0

base@data$BATS <- rowSums(l)
base$BATS <- rescale(base$BATS, to = c(0, 1)) 

library(sp)
# spplot(base, c('BATS'), lwd=0.5)


######## BAT ABUNDANCE

gbif.bat <- read.delim(file='C:/Users/cjcar/Dropbox/CEPI Vaccine Stockpiling/Nipah/Code for Spillover Layer/Chiroptera.csv',
           sep='\t') 

gbif.mam <- read.delim(file='C:/Users/cjcar/Dropbox/CEPI Vaccine Stockpiling/Nipah/Code for Spillover Layer/Mammalia.csv',
                       sep='\t')

gbif.bat <- gbif.bat[complete.cases(gbif.bat$decimalLatitude,gbif.bat$decimalLongitude),]
bat.df <- SpatialPoints(coords=gbif.bat[,c('decimalLongitude','decimalLatitude')])
bat.df@proj4string <- base@proj4string

o <- over(base,bat.df)
base$BATPTS <- o
# spplot(base, c('BATPTS'), lwd=0.5)


gbif.mam <- gbif.mam[complete.cases(gbif.mam$decimalLatitude,gbif.mam$decimalLongitude),]
mam.df <- SpatialPoints(coords=gbif.mam[,c('decimalLongitude','decimalLatitude')])
mam.df@proj4string <- base@proj4string

o <- over(base,mam.df)
base$MAMPTS <- o
# spplot(base, c('MAMPTS'), lwd=0.5)

base$MAMPTS[is.na(base$MAMPTS)] <- 0
base$BATPTS[is.na(base$BATPTS)] <- 0

base$GBIFRATIO <- rescale(base$BATPTS, to=c(0,1)) -
                  rescale(base$MAMPTS, to=c(0,1))

base$GBIFRATIO <- rescale(base$GBIFRATIO, to=c(0,1))
# spplot(base, c('GBIFRATIO'), lwd=0.5)

############

#https://science.sciencemag.org/content/361/6407/1108 
#Deforestation for forestry which here includes the fruit stuff as I udnerstand

forest <- raster('C:/Users/cjcar/Dropbox/CEPI Vaccine Stockpiling/Nipah/Code for Spillover Layer/FixedDeforest.tif')
deforest <- (forest>0) # 3 is deforestation for forestry 

meana <- function(x, na.rm=TRUE) na.omit(mean(x))
r.vals <- extract(deforest, base, fun=meana)
r.vals[is.na(r.vals)] <- 0
base$DEFOREST <- rescale(r.vals, to=c(0,1))

#spplot(base, c('DEFOREST'), lwd=0.5)

############

#install.packages('gfcanalysis')
#library(gfcanalysis)
#tiles <- calc_gfc_tiles(base)
#download_tiles(tiles, getwd(), images = c("treecover2000"))

setwd('C:/Users/cjcar/Dropbox/CEPI Vaccine Stockpiling/Nipah/Code for Spillover Layer')
#rlist <- list.files(pattern='treecover')
#rlist <- rlist[grep('.tif.aux.xml', rlist, invert=TRUE)]
#rlist$fun = mean
#tc <- do.call(mosaic, rlist)

#library(velox)
#veloxbagel <- function(filename) {
#  vx <- velox(filename)
#  vx$aggregate(factor=c(100,100), aggtype='mean')
#  print('one down')
#  return(vx$as.RasterLayer())
#}

#upscaled <- lapply(rlist, veloxbagel)
#upscaled$fun = mean
#tc <- do.call(mosaic, upscaled)
#writeRaster(tc,'treecover.tif')

tc2 <- raster('treecover.tif')
r.vals <- extract(tc2, base, fun=meana)
r.vals[is.na(r.vals)] <- 0
base$TREECOVER <- rescale(r.vals, to=c(0,1))

############

bangfruit <- read.csv('date_palm_fruit_adm2only.csv')
bangfruit$Division_name[!(bangfruit$Division_name %in% base$NAME_2)]

name_key <- c(Barguna = 'Borgona',
              Barishal = 'Barisal',
              Jhallokati = 'Jhalakati',
              Bandarban = 'Bandarbon',
              Chittagang = 'Chittagong',
              Cumilla = 'Comilla',
              Khagrachhari = 'Khagrachari',
              Laksmipur = 'Lakshmipur',
              Kishorganj = 'Kishoreganj',
              Manikganj = 'Manikgonj',
              Munshiganj = 'Munshigonj',
              Narayanganj = 'Naray Angonj',
              Narsingdi = 'Narshingdi',
              Chuadanga = 'Choua Danga',
              Jashore = 'Jessore', # this one is weird?
              Kushtia = 'Kustia',
              Satkhira = 'Shatkhira',
              Mymensing = 'Mymensingh',
              Bogura = 'Bogra',
              Joypurhat = 'Jaipurhat',
              'Chapai Nawabganj' = 'Nawabganj',
              Sirajganj = 'Sirajgonj', 
              Gaibandha = 'Gaibanda',
              Rangpur = 'Rongpur',
              Habiganj = 'Hobiganj',
              Maulvibazar = 'Moulvibazar',
              Sunamganj = 'Sun Amgonj',
              Gopalganj = 'Gopalgonj')

bangfruit %>% mutate(Division_name=recode(Division_name, !!!name_key)) -> bangfruit2

bangfruit2 <- bangfruit2[,c('Division_name','Total_production_2015.1')]
colnames(bangfruit2)[2] <- 'FruitProdBang' #metric tonnes
base@data <- left_join(base@data, bangfruit2, 
                       by=c('NAME_2'='Division_name'))

bangfruit2$Division_name[!(bangfruit2$Division_name %in% base$NAME_2)] # Reality check

spplot(base, c('FruitProd'), lwd=0.5)


bangoil <- read.csv('date_palm_juice_adm2only.csv')
bangoil$Division_name[!(bangoil$Division_name %in% base$NAME_2)]
bangoil %>% mutate(Division_name=recode(Division_name, !!!name_key)) -> bangoil2
bangoil2$Division_name[!(bangoil2$Division_name %in% base$NAME_2)]

bangoil2 <- bangoil2[,c('Division_name','Area_under_garden_2016')]
colnames(bangoil2)[2] <- 'OilAreaBang' # this one is acres 

bangoil2$OilAreaBang <- bangoil2$OilAreaBang * 0.00404686 # convert to km2

base@data <- left_join(base@data, bangoil2, 
                       by=c('NAME_2'='Division_name'))

spplot(base, c('OilAreaBang'), lwd=0.5)

base@data$FruitProdBPct <- (base@data$FruitProd)/(base@data$Area) #tonnes per square km
base@data$OilAreaBPct <- (base@data$OilAreaBang)/(base@data$Area)
spplot(base, c('OilAreaBPct'), lwd=0.5)
spplot(base, c('FruitProdBPct'), lwd=0.5)

############ 

india <- read.csv('IndiaStatePalm.csv')

base@data %>% filter(NAME_0 == 'India') %>% group_by(NAME_1) %>% 
  summarize(bigarea = sum(Area, na.rm=TRUE)) -> areadf

india <- left_join(india, areadf, by=c('State'='NAME_1'))
india$OilAreaInd <- india$OilHectares*0.01
india

base@data <- left_join(base@data, india[,-2], by=c('NAME_1'='State'))

base@data$FruitProdIPct <- (base@data$FruitTonnes)/(base@data$bigarea) #tonnes per square km
base@data$OilAreaIPct <- (base@data$OilAreaInd)/(base@data$bigarea)
spplot(base, c('FruitProdIPct'), lwd=0.5)


### PUT THEM TOGETHER

base@data$TOTALFRUIT <- pmin(base@data$FruitProdBPct, base@data$FruitProdIPct,
                            na.rm=TRUE)
base@data$TOTALFRUIT[is.na(base@data$TOTALFRUIT)] <- 0
spplot(base, c('TOTALFRUIT'), lwd=0.5)

base@data$TOTALOIL <- pmin(base@data$OilAreaIPct, base@data$OilAreaBPct,
                           na.rm=TRUE)
base@data$TOTALOIL[is.na(base@data$TOTALOIL)] <- 0
spplot(base, c('TOTALOIL'), lwd=0.5)

base$TOTALFRUIT <- rescale(base$TOTALFRUIT, to=c(0,1))
base$TOTALOIL <- rescale(base$TOTALOIL, to=c(0,1))

spplot(base, c('TOTALFRUIT','TOTALOIL'))
#############

base$SPILLOVER <- base$BATS + 
                  base$GBIFRATIO + 
                  base$DEFOREST + 
                  base$TREECOVER + 
                  base$TOTALFRUIT + 
                  base$TOTALOIL

base$SPILLOVER <- rescale(base$SPILLOVER, to=c(0,1))
spplot(base, c('SPILLOVER'), lwd=0.5)

spplot(base, c('BATS','GBIFRATIO','TREECOVER','DEFOREST', 'TOTALFRUIT','TOTALOIL'),
       names.attr = c('Host richness', 'Bat abundance', 'Tree Cover', 'Deforestation',
                      'Palm fruit prod.','Palm oil area'),
       lwd=0.2)
 

base$BATAB <- rescale(((base$GBIFRATIO)/base$Area), to=c(0,1))

base$BATVAR <- (base$BATS + base$GBIFRATIO)
base$BATVAR <- rescale(base$BATVAR, to=c(0,1))

base$TREEVAR <- (base$TREECOVER + base$DEFOREST)
base$TREEVAR <- rescale(base$TREECOVER, to=c(0,1))

base$PALMVAR <- (base$TOTALFRUIT + base$TOTALOIL)
base$PALMVAR <- rescale(base$PALMVAR, to=c(0,1))

base$SPILL2 <- (base$BATVAR + base$TREEVAR + base$PALMVAR)
base$SPILL2 <- rescale(base$SPILL2, to=c(0,1))


spplot(base, c('BATVAR','TREEVAR','PALMVAR','SPILL2'),
       names.attr = c('Bat risk','Forest risk','Palm risk','Spillover'),
       lwd=0.2)
