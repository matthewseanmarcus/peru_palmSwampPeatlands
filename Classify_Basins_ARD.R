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

### This code classifies the Pastaza Maranon, and Maranon Ucayali zones for 2018 and 2007

setwd('Z:/Peru_Palm_Swamp/ARD')

dir.create('tempfiledir2')
tempdir=paste(getwd(),'tempfiledir2', sep="/")
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


### Begin by fully merging 2018 ARD mosaic together

mos = stack('Z:/ARD_data/2018_palmSwamp_mosaic_test1.tif')
mos = dropLayer(mos, c(6:7))
mos2 = stack('Z:/ARD_data/2018_palmSwamp_mosaic_additional.tif')
mos2 =dropLayer(mos2, c(6:7)) 
hh = raster('Z:/Peru_Palm_Swamp/PalsarImages/palsar_dB_HH_ARD_2018.tif')
hv = raster('Z:/Peru_Palm_Swamp/PalsarImages/palsar_dB_HV_ARD_2018.tif')
mos = merge(mos2,mos)
writeRaster(mos, filename = 'Z:/ARD_data/2018_palmSwamp_mosaic_LoretoFull.tif')
plot(mos[[1]])
inimage = crop(mos, extent(hh))
inimage = stack(inimage, hh, hv)

###################### PMFB Zone (North of Maranon)  ##############

zone <- readOGR('Z:/Peru_Palm_Swamp/Shapefiles', 'PMFB_zone')
zone = spTransform(zone, crs(inimage))

inimage = mask(inimage, zone) %>% crop(extent(zone))

old = stack("Z:/Peru_Palm_Swamp/Mosaic/inimage_ARD_FULL_2018_1.tif")
names(old) <- c("Green", "Red", "NIR", "SWIR1", "SWIR2", "HH", "HV", "DEM", "NDVI","NDWI", "MSAVI", "Canopy")
loreto = readOGR(dsn = 'Z:/Peru_Palm_Swamp', layer = 'Loreto') %>% spTransform(crs(inimage))
dem <- raster('Z:/Peru_Palm_Swamp/Elevation/dem_ARD.tif')
dem = crop(dem, inimage)
heights = old$Canopy
heights = crop(heights, inimage)

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

inimage <- stack(inimage, dem, NDVI, ndwi, msavi, heights)
names(inimage) <- c("Green", "Red", "NIR", "SWIR1", "SWIR2", "HH", "HV", "DEM", "NDVI","NDWI", "MSAVI", "Canopy")
writeRaster(inimage, filename = 'Z:/Peru_Palm_Swamp/Mosaic/inimage_ARD_FULL_2018_1.tif')



# reproject calibdata to ARD projection WGS 84; also zones

# calibdata=readOGR("Z:/Peru_Palm_Swamp/Classifications", "Calibration_PalmSwamp_LoMedHigh")
# calibdata <- spTransform(calibdata, crs(inimage))
# 
# 
# 
# calibdata <- raster::intersect(calibdata, zone)
# writeOGR(calibdata, dsn = 'Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data', layer = 'calibdata_ARD_PMFB',
#          driver = 'ESRI Shapefile')
# 
# inimage <- mask(inimage, zone)
# inimage <- crop(inimage, extent(zone))
# bring revised cal data back in 
calibdata = readOGR(dsn = 'Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data', layer = 'calibdata_ARD_PMFB')

load("Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_pasmar_ard_18_4.RData")
calibdata = calibdata - superClassObj$validation$validationGeometry
val = superClassObj$validation$validationGeometry
val$CLASS_ID <- as.character(val$CLASS_ID)
rm(superClassObj)
superClassObj <- superClass(inimage, calibdata, valData=val, responseCol = "CLASS_ID", trainPartition = NULL, model="rf", mode= 'classification',
                            verbose=TRUE)



writeRaster(superClassObj$map, filename = "Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data/pasmar_ard_18_5.tif", datatype="INT2S")
save(superClassObj, file = "Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_pasmar_ard_18_5.RData")

# drop canopy and run again for 2007
inimage = dropLayer(inimage, 12)
superClassObj_for07 <- superClass(inimage, calibdata, valData=val, responseCol = "CLASS_ID", trainPartition = NULL, model="rf", mode= 'classification',
                            verbose=TRUE)
save(superClassObj_for07, file = "Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_pasmar_ard_for07_1.RData")

############################## pasmar 2007 ####################################

mosadd = stack('Z:/ARD_data/2007_palmSwamp_mosaic_additional.tif')
mosadd = dropLayer(mosadd, c(6,7))
mos = stack('Z:/ARD_data/2007_palmSwamp_mosaic_07.tif')
mos = dropLayer(mos, c(6:7))
hh = raster('Z:/Peru_Palm_Swamp/PalsarImages/palsar_dB_HH_ARD_2007.tif')
hv = raster('Z:/Peru_Palm_Swamp/PalsarImages/palsar_dB_HV_ARD_2007.tif')

mos = merge(mosadd, mos)
inimage = crop(mos, hh)
inimage = stack(inimage, hh, hv)
writeRaster(mos, filename = 'Z:/ARD_data/2007_palmSwamp_mosaic_07_2.tif')

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
dem <- raster('Z:/Peru_Palm_Swamp/Elevation/dem_ARD_FULL.tif') %>% crop(inimage)

inimage <- stack(inimage, dem, NDVI, ndwi, msavi)
names(inimage) <- c("Green", "Red", "NIR", "SWIR1", "SWIR2", "HH", "HV", "DEM", "NDVI","NDWI", "MSAVI")
writeRaster(inimage, filename = 'Z:/Peru_Palm_Swamp/Mosaic/inimage_ARD_FULL_2007_2.tif')


zone <- readOGR('Z:/Peru_Palm_Swamp/Shapefiles', 'PMFB_zone')
zone = spTransform(zone, crs(inimage))
inimage = mask(inimage, zone) %>% crop(extent(zone))
load("Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_pasmar_ard_for07_1.RData")

pmfb_07 <- predict(superClassObj_for07, inimage)
writeRaster(pmfb_07, filename = 'Z:/Peru_Palm_Swamp/ARD/Classifications_2007/pasmar07_2.tif', datatype = 'INT2S')

####### Pacaya Samaria Zone ########################################################################################

inimage1 <- stack('Z:/ARD_data/2018_palmSwamp_mosaic_LoretoFull.tif')
hh = raster('Z:/Peru_Palm_Swamp/PalsarImages/palsar_dB_HH_ARD_2018.tif')
hv = raster('Z:/Peru_Palm_Swamp/PalsarImages/palsar_dB_HV_ARD_2018.tif')
zone <- readOGR('Z:/Peru_Palm_Swamp/Shapefiles', 'PacayaSamaria_Zone')
zone = spTransform(zone, crs(inimage1))
inimage1 = crop(inimage1, extent(hh))
heights = raster('Z:/Peru_Palm_Swamp/CanopyHeight/canopyHeights_fullLoreto.tif') %>% crop(extent(hh))
inimage = inimage1
rm(inimage1)
dem = raster('Z:/Peru_Palm_Swamp/Elevation/dem_ARD.tif')
dems = list.files('Z:/ARD_data/srtm_add', recursive = TRUE, pattern = 'dem.tif')
setwd('Z:/ARD_data/srtm_add')
dems = lapply(dems, raster)
dem = merge(dem, dems[[1]], dems[[2]])
plot(dem)
writeRaster(dem, filename = 'Z:/Peru_Palm_Swamp/Elevation/dem_ARD_FULL.tif')
dem = crop(dem, extent(hh))


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


inimage = stack(inimage, hh, hv, dem, NDVI, ndwi, msavi, heights)
names(inimage) <- c("Green", "Red", "NIR", "SWIR1", "SWIR2", "HH", "HV", "DEM", "NDVI","NDWI", "MSAVI", "Canopy")
writeRaster(inimage, filename = 'Z:/ARD_data/inimage_Loreto_full.tif')
inimage <- mask(inimage, zone) %>% crop(extent(zone))


# calibdata=readOGR("Z:/Peru_Palm_Swamp/Classifications", "Calibration_PalmSwamp_LoMedHigh")
# calibdata <- spTransform(calibdata, crs(inimage1))
# calibdata <- raster::intersect(calibdata, zone)
# 
# writeOGR(calibdata, dsn = 'Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data', layer = 'calibdata_ARD_pacsam',
#          driver = 'ESRI Shapefile')

#### bring in data here
calibdata = readOGR('Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data', layer = 'calibdata_ARD_pacsam')
# inimage = stack('Z:/Peru_Palm_Swamp/Mosaic/inimage_Pacsam_ARD_18.tif')
# names(inimage) <- c("Green", "Red", "NIR", "SWIR1", "SWIR2", "HH", "HV", "DEM", "NDVI","NDWI", "MSAVI", "Canopy")


splitted=crossvalidsp(dataset=calibdata)
superClassObj <- superClass(inimage, splitted[[2]], valData=splitted[[1]], responseCol = "CLASS_ID", trainPartition = NULL, model="rf", mode= 'classification',
                            verbose=TRUE)
writeRaster(superClassObj$map, filename = "Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data/PacSam_ARD_2018_2.tif", datatype="INT2S")
save(superClassObj, file = "superClassObj_PacSam_ARD_2018_2.RData")
varImp(superClassObj$model, scale = FALSE)

# repeat, but without canopy to apply to 2007
im2 = dropLayer(inimage, 12)
names(im2) <- c("Green", "Red", "NIR", "SWIR1", "SWIR2", "HH", "HV", "DEM", "NDVI","NDWI", "MSAVI")
superClassObj2 <- superClass(im2, splitted[[2]], valData=splitted[[1]], responseCol = "CLASS_ID", trainPartition = NULL, model="rf", mode= 'classification',
                            verbose=TRUE)
save(superClassObj2, file = "Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_ARD_PacSam_for07.RData")
writeRaster(superClassObj2$map, filename = "Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data/PacSam_ARD_2018_NoCan_1.tif", datatype="INT2S")

#predict 2007

inimage = stack('Z:/Peru_Palm_Swamp/Mosaic/inimage_ARD_FULL_2007_2.tif')
names(inimage) <- c("Green", "Red", "NIR", "SWIR1", "SWIR2", "HH", "HV", "DEM", "NDVI","NDWI", "MSAVI")
zone <- readOGR('Z:/Peru_Palm_Swamp/Shapefiles', 'PacayaSamaria_Zone')
zone = spTransform(zone, crs(inimage))
inimage = mask(inimage, zone) %>% crop(extent(zone))
load("Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_ARD_PacSam_for07.RData")

pmfb_07 <- predict(superClassObj2, inimage)
writeRaster(pmfb_07, filename = 'Z:/Peru_Palm_Swamp/ARD/Classifications_2007/pacsam07_2.tif', datatype = 'INT2S')
