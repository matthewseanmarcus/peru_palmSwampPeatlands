library(raster)
library(rgdal)
library(RStoolbox)
library(maptools)
library(randomForest)
library(e1071)
library(caret)
library(ggplot2)
library(LSRS)

### This code classifies the Amazonas zone for 2018 and 2007


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
inimage = stack('Z:/Peru_Palm_Swamp/Mosaic/inimage_ARD_FULL_2018_1.tif')
calibdata=readOGR("Z:/Peru_Palm_Swamp/Shapefiles", "Calibration_East_Updated_4")
calibdata <- spTransform(calibdata, crs(inimage))
writeOGR(calibdata, dsn = 'Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data', layer = 'calibdata_ARD_East', driver = 'ESRI Shapefile')


###################    Classification Amazonas ###################################
zone <- readOGR("Z:/Peru_Palm_Swamp/Shapefiles", "East_StudyArea_2")
zone <- spTransform(zone, crs(inimage))
writeOGR(zone, dsn = 'Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data', layer = 'East_StudyArea_2_ARD', driver = 'ESRI Shapefile')

calibdata=readOGR(dsn = 'Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data', layer = 'calibdata_ARD_East')
inimage = mask(inimage, zone)
inimage = crop(inimage, extent(zone))
calibdata <- raster::intersect(calibdata, zone)

splitted=crossvalidsp(dataset=calibdata)

superClassObj <- superClass(inimage, splitted[[2]], valData=splitted[[1]], responseCol = "CLASS_ID", trainPartition = NULL, model="rf", mode= 'classification', verbose=TRUE)

writeRaster(superClassObj$map, filename = "Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data/east_ard_18_2.tif", datatype="INT2S")
save(superClassObj, file = "Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_east_ard_18_2.RData")
varImp(superClassObj$model, scale = FALSE)
save(superClassObj2, file = "Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_east_ard_18_3.RData") 
# superClassObj2 had higher accuracy. So _18_3 will be 2018 east map used
# run again without canopy for 2007
im2 = dropLayer(inimage, 12)
names(im2) <- c("Green", "Red", "NIR", "SWIR1", "SWIR2", "HH", "HV", "DEM", "NDVI","NDWI", "MSAVI")

superClassObj07 <- superClass(im2, splitted[[2]], valData=splitted[[1]], responseCol = "CLASS_ID", trainPartition = NULL, model="rf", mode= 'classification', verbose=TRUE)

save(superClassObj07, file = "Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_east_ard_NoCan_18_1.RData")
#writeRaster(superClassObj07$map, filename = "Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data/east_ard_NoCan_18_1.tif", datatype="INT2S")

###### apply nocan to 2007 east
inimage07 = stack('Z:/Peru_Palm_Swamp/Mosaic/inimage_ARD_FULL_2007_1.tif')
inimage07 = mask(inimage07, zone)
inimage07 = crop(inimage07, extent(zone))
names(inimage07) <- c("Green", "Red", "NIR", "SWIR1", "SWIR2", "HH", "HV", "DEM", "NDVI","NDWI", "MSAVI")
east_07 <- predict(superClassObj07, inimage07)

writeRaster(east_07, filename = 'Z:/Peru_Palm_Swamp/ARD/Classifications_2007/east07.tif', datatype = 'INT2S')