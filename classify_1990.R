library(raster)
library(rgdal)
library(RStoolbox)
library(maptools)
library(randomForest)
library(e1071)
library(caret)
library(ggplot2)
library(LSRS)
library(fitdistrplus)
library(aRn)
library(purrr)
library(rasterMapR)

### This code produces the classifications for the 1990 map by using the 2018 manually produced mosaic
### which is a mosaic of Landsat images from single dates (not ARD), without Palsar or Canopy Height data.
### It creates superclass Objects named ...for90.RData which are the random forest models, produced using the 2018
### calibration data over the 2018 manually produced mosaic, to be applied to the corresponding are in the
### 1990 mosaic.

crossvalidsp=function(dataset=indata, validprop=0.3){
  uniqueclass=unique(dataset$CLASS_ID)
  for (i in 1:length(uniqueclass)){
    classrows=which(dataset$CLASS_ID==uniqueclass[i],arr.ind=TRUE)
    classpoly=dataset[classrows,]
    validrows=sample(c(1:nrow(classpoly)), round(nrow(classpoly)*validprop))
    validpoly=classpoly[validrows,]
    calibpoly=classpoly[-validrows,]
    if (i==1){
      validtot=validpoly
      calibtot=calibpoly}
    else {
      validtot=spRbind(validtot, validpoly)
      calibtot=spRbind(calibtot, calibpoly)
    }
  }
  return(list(validtot, calibtot))
}

setwd('Z:/Peru_Palm_Swamp/ARD')
dir.create('tempfiledir')
tempdir=paste(getwd(),'tempfiledir', sep="/")
rasterOptions(tmpdir=tempdir)

inimage90 = stack('Z:/Peru_Palm_Swamp/Mosaic/inimage_90_RPJ.tif')
##### use old (pre-ARD) mosaic from 2018 to train a RF to classify 1990
inimage18 = stack('Z:/Peru_Palm_Swamp/Mosaic/inimage_FULL_2018_2.tif')
names(inimage18) <- c("Green", "Red", "NIR", "SWIR1", "SWIR2", "HH", "HV", "DEM", "NDVI","NDWI", "MSAVI", "Canopy")
mos90 = stack('Z:/ARD_data/1990_palmSwamp_mosaic.tif')
# reproject inimage18 to crs(mos90)
inimage18 = dropLayer(inimage18, c(6,7,12))
inimage18 = projectRaster(inimage18, mos90)
writeRaster(inimage18, filename = 'Z:/Peru_Palm_Swamp/Mosaic/inimage18_noPalnoCan_RPJtoARD.tif')

#### Start with Pastaza Maranon zone ###################################

inimage90 = stack('Z:/Peru_Palm_Swamp/Mosaic/inimage_90.tif')
zone <- readOGR('Z:/Peru_Palm_Swamp/Shapefiles', 'PMFB_zone')
zone = spTransform(zone, crs(mos90))
inimage90 = projectRaster(inimage90, mos90)
inimage90 <- mask(inimage90, zone) %>% crop(extent(zone))
pmfb18 = mask(inimage18, zone) %>% crop(extent(zone))
calibdata = readOGR(dsn = 'Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data', layer = 'calibdata_ARD_PMFB')
load("Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_pasmar_ard_18_4.RData")

calibdata = calibdata - superClassObj$validation$validationGeometry
val = superClassObj$validation$validationGeometry
val$CLASS_ID <- as.character(val$CLASS_ID)

superClassObj_for90 <- superClass(pmfb18, calibdata, valData=val, responseCol = "CLASS_ID", trainPartition = NULL, model="rf", mode= 'classification',
                                  verbose=TRUE)
save(superClassObj_for90, file = 'Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_for90_pmfb.RData')


names(inimage90) <- c("Green", "Red", "NIR", "SWIR1", "SWIR2", "DEM", "NDVI","NDWI", "MSAVI")
pmfb_90 <- predict(superClassObj_for90, inimage90)
writeRaster(pmfb_90, filename = 'Z:/Peru_Palm_Swamp/ARD/Classifications_1990_2/pmfb90.tif', datatype='INT2S')


############# maranon ucayali ###########################################
rm(calibdata, superClassObj, superClassObj_for90, val, zone)

inimage90 = stack('Z:/Peru_Palm_Swamp/Mosaic/inimage_90.tif')
zone <- readOGR('Z:/Peru_Palm_Swamp/Shapefiles', 'PacayaSamaria_Zone')
zone = spTransform(zone, crs(inimage18))
inimage90 = projectRaster(inimage90, inimage18)
writeRaster(inimage90, filename = 'Z:/Peru_Palm_Swamp/Mosaic/inimage_90_RPJ.tif')
inimage90 <- mask(inimage90, zone) %>% crop(extent(zone))
im18 = mask(inimage18, zone) %>% crop(extent(zone))
calibdata = readOGR(dsn = 'Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data', layer = 'calibdata_ARD_pacsam')
load("Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_PacSam_ARD_2018_1.RData")

calibdata = calibdata - superClassObj$validation$validationGeometry
val = superClassObj$validation$validationGeometry
val$CLASS_ID <- as.character(val$CLASS_ID)

superClassObj_for90 <- superClass(im18, calibdata, valData=val, responseCol = "CLASS_ID", trainPartition = NULL, model="rf", mode= 'classification',
                                  verbose=TRUE)
save(superClassObj_for90, file = 'Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_for90_pacsam.RData')

names(inimage90) <- c("Green", "Red", "NIR", "SWIR1", "SWIR2", "DEM", "NDVI","NDWI", "MSAVI")
class_90 <- predict(superClassObj_for90, inimage90)
writeRaster(class_90, filename = 'Z:/Peru_Palm_Swamp/ARD/Classifications_1990_2/pacsam90.tif', datatype='INT2S')


##################   amazonas #####################################
rm(calibdata, superClassObj, superClassObj_for90, val, zone)

inimage90 = stack('Z:/Peru_Palm_Swamp/Mosaic/inimage_90_RPJ.tif')
zone <- readOGR('Z:/Peru_Palm_Swamp/Shapefiles', 'East_StudyArea_2')
zone = spTransform(zone, crs(inimage18))
inimage90 <- mask(inimage90, zone) %>% crop(extent(zone))
im18 = mask(inimage18, zone) %>% crop(extent(zone))
calibdata = readOGR(dsn = 'Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data', layer = 'calibdata_ARD_East')
load("Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_east_ard_18_2.RData")

calibdata = calibdata - superClassObj$validation$validationGeometry
val = superClassObj$validation$validationGeometry
calibdata = intersect(calibdata, zone)
val = intersect(val, zone)
val$CLASS_ID <- as.character(val$CLASS_ID)

superClassObj_for90 <- superClass(im18, calibdata, valData=val, responseCol = "CLASS_ID", trainPartition = NULL, model="rf", mode= 'classification',
                                  verbose=TRUE)
save(superClassObj_for90, file = 'Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_for90_east.RData')

names(inimage90) <- c("Green", "Red", "NIR", "SWIR1", "SWIR2", "DEM", "NDVI","NDWI", "MSAVI")
class_90 <- predict(superClassObj_for90, inimage90)
writeRaster(class_90, filename = 'Z:/Peru_Palm_Swamp/ARD/Classifications_1990_2/east90.tif', datatype='INT2S')


##################  napo tigre #####################################
rm(calibdata, superClassObj, superClassObj_for90, val, zone)

inimage90 = stack('Z:/Peru_Palm_Swamp/Mosaic/inimage_90_RPJ.tif')
zone <- readOGR('Z:/Peru_Palm_Swamp/Shapefiles', 'East_StudyArea_2')
zone = spTransform(zone, crs(inimage18))
inimage90 <- mask(inimage90, zone) %>% crop(extent(zone))
im18 = mask(inimage18, zone) %>% crop(extent(zone))
calibdata = readOGR(dsn = 'Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data', layer = 'calibdata_ARD_Napo')
load("Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_ARD_Napo_2018_2.RData")

calibdata = calibdata - superClassObj$validation$validationGeometry
val = superClassObj$validation$validationGeometry
val$CLASS_ID <- as.character(val$CLASS_ID)

superClassObj_for90 <- superClass(im18, calibdata, valData=val, responseCol = "CLASS_ID", trainPartition = NULL, model="rf", mode= 'classification',
                                  verbose=TRUE)
save(superClassObj_for90, file = 'Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_for90_napo.RData')

names(inimage90) <- c("Green", "Red", "NIR", "SWIR1", "SWIR2", "DEM", "NDVI","NDWI", "MSAVI")
class_90 <- predict(superClassObj_for90, inimage90)
writeRaster(class_90, filename = 'Z:/Peru_Palm_Swamp/ARD/Classifications_1990_2/napo90.tif', datatype='INT2S')


##################   putumayo #####################################
rm(calibdata, superClassObj, superClassObj_for90, val, zone, class_90, im18)

inimage90 = stack('Z:/Peru_Palm_Swamp/Mosaic/inimage_90_RPJ.tif')
zone <- readOGR('Z:/Peru_Palm_Swamp/Shapefiles', 'NorthEast_StudyArea')
zone = spTransform(zone, crs(inimage18))
inimage90 <- mask(inimage90, zone) %>% crop(extent(zone))
im18 = mask(inimage18, zone) %>% crop(extent(zone))
calibdata = readOGR(dsn = 'Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data', layer = 'calibdata_ARD_East')
load("Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_putumayo_ard_18_1.RData")

calibdata = calibdata - superClassObj$validation$validationGeometry
val = superClassObj$validation$validationGeometry
val$CLASS_ID <- as.character(val$CLASS_ID)
calibdata = raster::intersect(calibdata, extent(zone))
val = raster::intersect(val, extent(zone))
names(im18) <- c("Green", "Red", "NIR", "SWIR1", "SWIR2", "DEM", "NDVI","NDWI", "MSAVI")

superClassObj_for90 <- superClass(im18, calibdata, valData=val, responseCol = "CLASS_ID", trainPartition = NULL, model="rf", mode= 'classification',
                                  verbose=TRUE)
save(superClassObj_for90, file = 'Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_for90_napo.RData')

names(inimage90) <- c("Green", "Red", "NIR", "SWIR1", "SWIR2", "DEM", "NDVI","NDWI", "MSAVI")
class_90 <- predict(superClassObj_for90, inimage90)
writeRaster(class_90, filename = 'Z:/Peru_Palm_Swamp/ARD/Classifications_1990_2/putu90.tif', datatype='INT2S')
