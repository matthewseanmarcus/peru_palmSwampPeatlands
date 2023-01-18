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

### This code produces the classifications of the Ucayali zone for 2018, 2007, and 1990

crossvalidsp=function(dataset=indata, validprop=0.3){
  uniqueclass=unique(dataset$CLASS_NAME)
  for (i in 1:length(uniqueclass)){
    classrows=which(dataset$CLASS_NAME==uniqueclass[i],arr.ind=TRUE)
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
names(inimage) <- c("Green", "Red", "NIR", "SWIR1", "SWIR2", "HH", "HV", "DEM", "NDVI","NDWI", "MSAVI", "Canopy")
# south DEM must be reprojected and added in to inimage

zone <- readOGR("Z:/Peru_Palm_Swamp/Shapefiles", "South_studyArea")
zone = spTransform(zone, crs(inimage))
calibdata=readOGR("Z:/Peru_Palm_Swamp/Shapefiles", "ROIs_CAL_mosaicSOUTH_2")
calibdata = spTransform(calibdata, crs(inimage))
calibdata = intersect(calibdata, zone)
writeOGR(calibdata, dsn = 'Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data', layer = 'calibdata_ARD_south', driver = 'ESRI Shapefile')
dem <- raster('Z:/Peru_Palm_Swamp/Elevation/dem_HolesPatched_South.tif')

inimage = dropLayer(inimage, 8)
inimage = mask(inimage, zone)
inimage = crop(inimage, extent(zone))
dem = projectRaster(dem, inimage)
inimage = stack(inimage, dem)
names(inimage) <- c("Green", "Red", "NIR", "SWIR1", "SWIR2", "HH", "HV",  "NDVI","NDWI", "MSAVI", "Canopy","DEM")
calibdata = readOGR(dsn = 'Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data', layer = 'calibdata_ARD_south')
splitted=crossvalidsp(dataset=calibdata)
superClassObj <- superClass(inimage, splitted[[2]], valData=splitted[[1]], responseCol = "CLASS_NAME", trainPartition = NULL, model="rf", mode= 'classification',
                            verbose=TRUE)
writeRaster(superClassObj$map, filename = "Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data/south_ard_18_2.tif", datatype="INT2S")  ##!! change _2_MAj
save(superClassObj, file = "Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_ARD_South_2018_2.RData")
# drop canopy for 2007
inimage = dropLayer(inimage, 11)
superClassObj <- superClass(inimage, splitted[[2]], valData=splitted[[1]], responseCol = "CLASS_NAME", trainPartition = NULL, model="rf", mode= 'classification',
                            verbose=TRUE)
save(superClassObj, file = "Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_ARD_South_2018_noCan.RData")
inimage07 = stack('Z:/Peru_Palm_Swamp/Mosaic/inimage_ARD_FULL_2007_1.tif')
names(inimage07) <- c("Green", "Red", "NIR", "SWIR1", "SWIR2", "HH", "HV", "DEM", "NDVI","NDWI", "MSAVI")
inimage07$DEM = merge(inimage07$DEM, dem)
inimage07 = mask(inimage07, zone)
inimage07 = crop(inimage07, extent(zone))
x_07 <- predict(superClassObj, inimage07)
writeRaster(x_07, filename = 'Z:/Peru_Palm_Swamp/ARD/Classifications_2007/south07.tif', datatype = 'INT2S')
# drop palsar for 1990
inimage = dropLayer(inimage, c(6,7))
superClassObj <- superClass(inimage, splitted[[2]], valData=splitted[[1]], responseCol = "CLASS_NAME", trainPartition = NULL, model="rf", mode= 'classification',
                            verbose=TRUE)
save(superClassObj, file = "Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_ARD_South_2018_for1990.RData")

load("Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_ARD_South_2018_for1990.RData")
inimage = stack('Z:/Peru_Palm_Swamp/Mosaic/South_1990_normToArd.tif')

NDVI <- ((inimage[[3]]-inimage[[2]])/(inimage[[3]]+inimage[[2]]))
m <- c(-Inf, -1.01, 0, 1.01,Inf, 0)
rclmat <- matrix(m, ncol = 3, byrow = TRUE)
NDVI <- reclassify(NDVI, rclmat)
ndwi <- ((inimage[[3]]-inimage[[4]])/(inimage[[3]]+inimage[[4]]))
ndwi=reclassify(ndwi, rclmat)
msavi <- MSAVI(a = inimage[[3]], b = inimage[[2]], Pixel.Depth = 1)
m_msavi <- c(NA,6.5)
rclmat <- matrix(m_msavi, ncol = 2, byrow = TRUE)
msavi <- reclassify(msavi, rclmat)

dem <- raster('Z:/Peru_Palm_Swamp/Elevation/dem_HolesPatched_South.tif')
dem = projectRaster(dem, inimage)
inimage90 = stack(inimage, dem, NDVI, ndwi, msavi)
names(inimage90) <- c("Green", "Red", "NIR", "SWIR1", "SWIR2", "DEM", "NDVI","NDWI", "MSAVI")

im_90 <- predict(superClassObj, inimage90)
writeRaster(im_90, filename = 'Z:/Peru_Palm_Swamp/ARD/Classifications_1990/south90.tif', datatype = 'INT2S')
