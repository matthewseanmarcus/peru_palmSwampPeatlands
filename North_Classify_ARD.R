library(raster)
library(rgdal)
library(RStoolbox)
library(maptools)
library(randomForest)
library(e1071)
library(caret)
library(ggplot2)
library(LSRS)
library(tidyverse)

### This code classifies the Putumayo and Napo tigre zones for 2018 and 2007

setwd('Z:/Peru_Palm_Swamp/ARD')

dir.create('tempfiledir3')
tempdir=paste(getwd(),'tempfiledir3', sep="/")
rasterOptions(tmpdir=tempdir)

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
########### North East (Amazon up to Putumayo River) ####################

inimage <- stack('Z:/ARD_data/inimage_Loreto_full.tif')
zone <- readOGR('Z:/Peru_Palm_Swamp/Shapefiles','NorthEast_StudyArea')
zone = spTransform(zone, crs(inimage))
inimage <- mask(inimage, zone) %>% crop(extent(zone))

calibdata <- readOGR('Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data','calibdata_ARD_East')
calibdata <- raster::intersect(calibdata, zone)

names(inimage) <- c("Green", "Red", "NIR", "SWIR1", "SWIR2", "HH", "HV", "DEM", "NDVI","NDWI", "MSAVI", "Canopy")

splitted=crossvalidsp(dataset=calibdata)
superClassObj <- superClass(inimage, splitted[[2]], valData=splitted[[1]], responseCol = "CLASS_ID", trainPartition = NULL, model="rf", mode= 'classification',
                            verbose=TRUE)
writeRaster(superClassObj$map, filename = "Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data/putumayo_ard_18_2.tif", datatype="INT2S")
save(superClassObj, file = "Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_putumayo_ard_18_2.RData")
varImp(superClassObj$model, scale = FALSE)

inimage07 = dropLayer(inimage, 12)
superClassObj_for07 <- superClass(inimage07, splitted[[2]], valData=splitted[[1]], responseCol = "CLASS_ID", trainPartition = NULL, model="rf", mode= 'classification',
                            verbose=TRUE)
save(superClassObj_for07, file = "Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_putumayo_ard_for07_18_2.RData")
# predict 2007
inimage = stack('Z:/Peru_Palm_Swamp/Mosaic/inimage_ARD_FULL_2007_2.tif')
names(inimage) <- c("Green", "Red", "NIR", "SWIR1", "SWIR2", "HH", "HV", "DEM", "NDVI","NDWI", "MSAVI")
zone <- readOGR('Z:/Peru_Palm_Swamp/Shapefiles','NorthEast_StudyArea')
zone = spTransform(zone, crs(inimage))
inimage <- mask(inimage, zone) %>% crop(extent(zone))
load("Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_putumayo_ard_for07_18_2.RData")
im7 = predict(superClassObj_for07, inimage)
writeRaster(im7, filename = 'Z:/Peru_Palm_Swamp/ARD/Classifications_2007/putumayo07_2.tif', datatype='INT2S')



######################################################### Napo region ###############################################################

inimage <- stack('Z:/ARD_data/inimage_Loreto_full.tif')

#writeOGR(zone,  dsn = 'Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data', layer = 'Napo_zone', driver = 'ESRI Shapefile')
names(inimage) <- c("Green", "Red", "NIR", "SWIR1", "SWIR2", "HH", "HV", "DEM", "NDVI","NDWI", "MSAVI", "Canopy")


calibdata = readOGR(dsn = 'Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data', layer = 'calibdata_ARD_PMFB')
calibdata2 = readOGR(dsn = 'Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data', layer = 'calibdata_ARD_East')
calibdata = bind(calibdata, calibdata2)
rm(calibdata2)
calibdata = intersect(calibdata, zone)
writeOGR(calibdata, dsn = 'Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data', layer = 'calibdata_ARD_Napo', driver = 'ESRI Shapefile')

calibdata = readOGR(dsn = 'Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data', layer = 'calibdata_ARD_Napo')
# add improved zone
## also, bring in 2 new DEM files and fix DEM layer
setwd('Z:/Peru_Palm_Swamp/Elevation/northPeruDems')
demlist = list.files(path = 'Z:/Peru_Palm_Swamp/Elevation/northPeruDems')
dems = lapply(demlist, raster)
merged = merge(dems[[1]],dems[[2]])
merged = projectRaster(merged, inimage)
inimage$DEM = merge(inimage$DEM, merged)


zone = readOGR('Z:/Peru_Palm_Swamp/Shapefiles', 'Napo_zone')
zone = spTransform(zone, crs(inimage))
inimage <- mask(inimage, zone) %>% crop(extent(zone))

calibdata = readOGR(dsn = 'Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data', layer = 'calibdata_ARD_Napo')
splitted=crossvalidsp(dataset=calibdata)
superClassObj <- superClass(inimage, splitted[[2]], valData=splitted[[1]], responseCol = "CLASS_ID", trainPartition = NULL, model="rf", mode= 'classification',
                            verbose=TRUE)
writeRaster(superClassObj$map, filename = "Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data/napo_ard_18_3.tif", datatype="INT2S")  ##!! change _2_MAj
save(superClassObj, file = "Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_ARD_Napo_2018_3.RData")
varImp(superClassObj$model, scale = FALSE)

# drop canopy for 2007
inimage = dropLayer(inimage,12)
superClassObj_for07 <- superClass(inimage, splitted[[2]], valData=splitted[[1]], responseCol = "CLASS_ID", trainPartition = NULL, model="rf", mode= 'classification',
                            verbose=TRUE)
save(superClassObj_for07, file = "Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_napo_ard_for07_2.RData")
inimage07 = stack('Z:/Peru_Palm_Swamp/Mosaic/inimage_ARD_FULL_2007_1.tif')
inimage07 = mask(inimage07, zone)
inimage07 = crop(inimage07, extent(zone))

inimage = stack('Z:/Peru_Palm_Swamp/Mosaic/inimage_ARD_FULL_2007_2.tif')
zone <- readOGR('Z:/Peru_Palm_Swamp/Shapefiles','Napo_zone')
zone = spTransform(zone, crs(inimage))
inimage <- mask(inimage, zone) %>% crop(extent(zone))
load("Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_napo_ard_for07_2.RData")

names(inimage) <- c("Green", "Red", "NIR", "SWIR1", "SWIR2", "HH", "HV", "DEM", "NDVI","NDWI", "MSAVI")
x_07 <- predict(superClassObj_for07, inimage)
writeRaster(x_07, filename = 'Z:/Peru_Palm_Swamp/ARD/Classifications_2007/napo07_2.tif', datatype = 'INT2S')