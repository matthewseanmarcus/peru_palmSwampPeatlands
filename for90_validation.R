library(tidyverse)
library(raster)
library(rgdal)
library(RStoolbox)
library(maptools)
library(randomForest)
library(e1071)
library(caret)
library(LSRS)
library(fitdistrplus)

### this code validates the 1990 classification, by producing the 2018 manually produced classification
### and then validating it using the validation polygons for the 2018 ARD produced classification
### The 2018 manually produced classification is a classification of the landscape in 2018, without
### palsar and canopy height data which were not available in 1990. Therefore, classifying the 2018 map
### without palsar and canopy height data and then validating with the 2018 validaiton polygons is the only way
### to validate the 1990 maps.


setwd('Z:/Peru_Palm_Swamp/ARD')
dir.create('tempfiledir')
tempdir=paste(getwd(),'tempfiledir', sep="/")
rasterOptions(tmpdir=tempdir)
# to validate the 1990 maps, bring in all for90_superclassobj's used
# merge the maps together, maj filter, then validate with 2018 val polygons
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

inimage18 = stack('Z:/Peru_Palm_Swamp/Mosaic/inimage18_noPalnoCan_RPJtoARD.tif')
names(inimage18) <- c("Green", "Red", "NIR", "SWIR1", "SWIR2", "DEM", "NDVI","NDWI", "MSAVI")

############## pastaza - maranon ########################

load("Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_for90_pmfb.RData")
pmfb = superClassObj_for90
zone <- readOGR('Z:/Peru_Palm_Swamp/Shapefiles', 'PMFB_zone') %>% spTransform(crs(inimage18))
im18 = mask(inimage18, zone) %>% crop(extent(zone))
writeRaster(im18, filename = 'Z:/Peru_Palm_Swamp/ARD/Classifications_1990_2/for90_inimages/im18_pmfb_for90.tif')
im18 = stack('Z:/Peru_Palm_Swamp/ARD/Classifications_1990_2/for90_inimages/im18_pmfb_for90.tif')
names(im18) <- c("Green", "Red", "NIR", "SWIR1", "SWIR2", "DEM", "NDVI","NDWI", "MSAVI")
calibdata = readOGR(dsn = 'Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data', layer = 'calibdata_ARD_PMFB')
splitted = crossvalidsp(calibdata)

calibdata = calibdata - pmfb$validation$validationGeometry
val = pmfb$validation$validationGeometry
val$CLASS_ID <- as.character(val$CLASS_ID)
imx = superClass(im18, calibdata, valData=val, responseCol = "CLASS_ID", trainPartition = NULL, model="rf", mode= 'classification',
                 verbose=TRUE)
writeRaster(imx$map, filename = 'Z:/Peru_Palm_Swamp/ARD/Classifications_1990_2/Expanded/for90_val_maps/pmfb.tif',
            datatype='INT2S')

############### maranon ucayali ################

load("Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_for90_pacsam.RData")
zone <- readOGR('Z:/Peru_Palm_Swamp/Shapefiles', 'PacayaSamaria_Zone') %>% spTransform(crs(inimage18))
im18 = mask(inimage18, zone) %>% crop(extent(zone))
calibdata = readOGR(dsn = 'Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data', layer = 'calibdata_ARD_pacsam')
calibdata = calibdata - superClassObj_for90$validation$validationGeometry
val = superClassObj_for90$validation$validationGeometry
val$CLASS_ID <- as.character(val$CLASS_ID)
imx = superClass(im18, calibdata, valData=val, responseCol = "CLASS_ID", trainPartition = NULL, model="rf", mode= 'classification',
                 verbose=TRUE)
writeRaster(imx$map, filename = 'Z:/Peru_Palm_Swamp/ARD/Classifications_1990_2/Expanded/for90_val_maps/pacsam.tif',
            datatype='INT2S')

################ napo tigre ###################

load("Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_for90_napo.RData")
zone <- readOGR('Z:/Peru_Palm_Swamp/Shapefiles', 'Napo_zone') %>% spTransform(crs(inimage18))
im18 = mask(inimage18, zone) %>% crop(extent(zone))
calibdata = readOGR(dsn = 'Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data', layer = 'calibdata_ARD_Napo')
calibdata = calibdata - superClassObj_for90$validation$validationGeometry
val = superClassObj_for90$validation$validationGeometry
val$CLASS_ID <- as.character(val$CLASS_ID)
imx = superClass(im18, calibdata, valData=val, responseCol = "CLASS_ID", trainPartition = NULL, model="rf", mode= 'classification',
                 verbose=TRUE)
writeRaster(imx$map, filename = 'Z:/Peru_Palm_Swamp/ARD/Classifications_1990_2/Expanded/for90_val_maps/napo.tif',
            datatype='INT2S')

############## amazonas #################

load("Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_for90_east.RData")
zone <- readOGR('Z:/Peru_Palm_Swamp/Shapefiles', 'East_StudyArea_2') %>% spTransform(crs(inimage18))
im18 = mask(inimage18, zone) %>% crop(extent(zone))
calibdata = readOGR(dsn = 'Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data', layer = 'calibdata_ARD_East') %>% intersect(zone)
calibdata = calibdata - superClassObj_for90$validation$validationGeometry
val = superClassObj_for90$validation$validationGeometry
val$CLASS_ID <- as.character(val$CLASS_ID)
imx = superClass(im18, calibdata, valData=val, responseCol = "CLASS_ID", trainPartition = NULL, model="rf", mode= 'classification',
                 verbose=TRUE)
writeRaster(imx$map, filename = 'Z:/Peru_Palm_Swamp/ARD/Classifications_1990_2/Expanded/for90_val_maps/east.tif',
            datatype='INT2S')

rm(calibdata,im18,imx,superClassObj_for90,val,zone)
############### putumayo; need to redo the zone ###############

zone <- readOGR('Z:/Peru_Palm_Swamp/Shapefiles', 'NorthEast_StudyArea') %>% spTransform(crs(inimage18))
im18 = mask(inimage18, zone) %>% crop(extent(zone))
calibdata = readOGR(dsn = 'Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data', layer = 'calibdata_ARD_East') %>% intersect(zone)
splitted = crossvalidsp(calibdata)
imx = superClass(im18, splitted[[2]], valData=splitted[[1]], responseCol = "CLASS_ID", trainPartition = NULL, model="rf", mode= 'classification',
                 verbose=TRUE)
writeRaster(imx$map, filename = 'Z:/Peru_Palm_Swamp/ARD/Classifications_1990_2/Expanded/for90_val_maps/putu.tif',
            datatype='INT2S')
save(imx, file = 'Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_for90_putu.RData')

im1 = stack("Z:/Peru_Palm_Swamp/LandsatImages/1990_Images/Additional/im4_62_NORMALIZED.tif")
inimage = stack('Z:/Peru_Palm_Swamp/Mosaic/mosaic1990_longlat_resampled.tif')
inimage = merge(inimage, im1)
inimage = mask(inimage, zone) %>% crop(extent(zone))
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

mos18 = stack('Z:/ARD_data/inimage_Loreto_full.tif')
names(mos18) <- c("Green", "Red", "NIR", "SWIR1", "SWIR2", "HH", "HV", "DEM", "NDVI","NDWI", "MSAVI", "Canopy")
dem = mos18$DEM
dem = crop(dem, extent(zone))
rm(mos18)
dem = extend(dem, inimage)


inimage <- stack(inimage, dem, NDVI, ndwi, msavi)
names(inimage) <- c("Green", "Red", "NIR", "SWIR1", "SWIR2",  "DEM", "NDVI","NDWI", "MSAVI")

# the ndvi has -Inf values, not sure how
m = c(-Inf,0)
rclmat = matrix(m, ncol = 2, byrow = TRUE)
inimage$NDVI = reclassify(inimage$NDVI, rclmat)
cls = predict(imx, inimage)
writeRaster(cls, filename = 'Z:/Peru_Palm_Swamp/ARD/Classifications_1990_2/Expanded/putu90_2.tif', datatype='INT2S')
writeRaster(im18, filename = 'Z:/Peru_Palm_Swamp/ARD/Classifications_1990_2/for90_inimages/im18_putu_for90.tif')
writeRaster(inimage, filename = 'Z:/Peru_Palm_Swamp/ARD/Classifications_1990_2/for90_inimages/inimage90_putu.tif')

######### ucayali ###########

load("Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_ARD_South_2018_for1990.RData")
zone <- readOGR('Z:/Peru_Palm_Swamp/Shapefiles', 'South_studyArea') %>% spTransform(crs(inimage18))
im18 = mask(inimage18, zone) %>% crop(extent(zone))

dem = raster('Z:/Peru_Palm_Swamp/Elevation/dem_ARD_FULL.tif') %>% crop(zone)
im18$DEM <- dem
calibdata = readOGR(dsn = 'Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data', layer = 'calibdata_ARD_south')  %>% intersect(zone)
calibdata = calibdata - superClassObj$validation$validationGeometry
val = superClassObj$validation$validationGeometry
val$CLASS_NAME <- as.character(val$CLASS_NAME)
imx = superClass(im18, calibdata, valData=val, responseCol = "CLASS_NAME", trainPartition = NULL, model="rf", mode= 'classification',
                 verbose=TRUE)
writeRaster(imx$map, filename = 'Z:/Peru_Palm_Swamp/ARD/Classifications_1990_2/Expanded/for90_val_maps/south.tif',
            datatype='INT2S')

############ merge all, reclass, then majority
setwd('Z:/Peru_Palm_Swamp/ARD/Classifications_1990_2/Expanded/for90_val_maps')

pmfb = raster('pmfb.tif')
east = raster('east.tif')
pacsam = raster('pacsam.tif')
napo = raster('napo.tif')
ne = raster('putu.tif')
south = raster('south.tif')

m <- c(5,6, 6,7, 7,8, 8,9, 9,10,10,11)
rcl_new <- matrix(m, ncol = 2, byrow = TRUE)
napo <- reclassify(napo, rcl_new)

m_south = c(6,7, 7,8, 8,12)
rcl_sou <- matrix(m_south, ncol = 2, byrow = TRUE)
south <- reclassify(south, rcl_sou)

# mask east
msk = readOGR('Z:/Peru_Palm_Swamp/Shapefiles', 'East_crop')
east = mask(east, msk)

im <- merge(pmfb, east, pacsam, napo, ne, south) 
writeRaster(im, filename = 'for_90_merged.tif', datatype='INT2S')

## perform 3x3 majority filter in arcgis (too slow to use R for this)

im = raster('for_90_merged_MAJ.tif')
rm(pmfb, east, pacsam, napo, ne, south)
### validate im

library(ggplot2)
library(raster)
library(RStoolbox)
library(ggpubr)
library(rgdal)
library(maptools)
library(gridExtra)
library(caret)


#### pull out all validation polygons from each of 6 superclassobjects, then apply to full classification

#### load in updated .RData files

load("Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_for90_pmfb.RData") 
z_pasmar = superClassObj_for90
rm(superClassObj_for90)
load("Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_for90_east.RData")
z_east = superClassObj_for90
rm(superClassObj_for90)
load("Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_for90_pacsam.RData") 
z_pacsam = superClassObj_for90
rm(superClassObj_for90)
load('Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_for90_napo.RData')
z_napo = superClassObj_for90
rm(superClassObj_for90)
load('Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_putumayo_ard_18_2.RData')
z_putu = superClassObj
rm(superClassObj)
load('Z:/Peru_Palm_Swamp/ARD/RData_Files/superClassObj_ARD_South_2018_for1990.RData')
z_south = superClassObj
rm(superClassObj)
z_south$validation$validationGeometry$CLASS_ID=z_south$validation$validationGeometry$CLASS_NAME


zlist = bind(z_pasmar$validation$validationGeometry, z_east$validation$validationGeometry, z_pacsam$validation$validationGeometry,
             z_napo$validation$validationGeometry, z_south$validation$validationGeometry)

writeOGR(zlist, dsn = 'Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data', layer = 'validation_2018_for90', driver = 'ESRI Shapefile')

val_z = readOGR(dsn = 'Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data', layer = 'validation_2018_for90')


val_z$CLASS_ID_INT[val_z$CLASS_ID == "Open Peat"] <- 1
val_z$CLASS_ID_INT[val_z$CLASS_ID == "Palm Swamp High Density"] <- 2
val_z$CLASS_ID_INT[val_z$CLASS_ID == "Palm Swamp Low Density"] <- 3
val_z$CLASS_ID_INT[val_z$CLASS_ID == "Palm Swamp Medium Density"] <- 4
val_z$CLASS_ID_INT[val_z$CLASS_ID == "Pasture"] <- 5
val_z$CLASS_ID_INT[val_z$CLASS_ID == "Pole Forest"] <- 6
val_z$CLASS_ID_INT[val_z$CLASS_ID == "Sand Bank"] <- 7
val_z$CLASS_ID_INT[val_z$CLASS_ID == "Seasonally Flooded Forest"] <- 8
val_z$CLASS_ID_INT[val_z$CLASS_ID == "Terra Firme Forest"] <-9
val_z$CLASS_ID_INT[val_z$CLASS_ID == "Urban"] <-10
val_z$CLASS_ID_INT[val_z$CLASS_ID == "Water"] <-11
val_z$CLASS_ID_INT[val_z$CLASS_ID == "Secondary Forest"] <-12



valmap = validateMap(im, valData = val_z, responseCol = 'CLASS_ID_INT', nSamples = 1000)
save(valmap, file = 'Z:/Peru_Palm_Swamp/ARD/RData_Files/ARD_for90_Dataconmatrix_FULL.RData')

#load('Z:/Peru_Palm_Swamp/ARD/RData_Files/ARD_for90_Dataconmatrix_FULL.RData')

write.csv(valmap$performance$table, file = 'Z:/Peru_Palm_Swamp/ARD/Accuracy_ARD/acc_matrix_for90.csv')

#### olofsson corrections for 1990 ########################################################################

setwd("Z:/Peru_Palm_Swamp/ARD/Accuracy_ARD")
load('Z:/Peru_Palm_Swamp/ARD/RData_Files/ARD_2018_Dataconmatrix_FULL_18_2.RData')
x <- as.data.frame(valmap$performance$byClass)

y = as.data.frame(x$Sensitivity) # Sensitivity is Producer's Acc
y$Accuracy_Type <- 0
classes = c("Herbaceous Swamp", "Palm Swamp High Density", "Palm Swamp Low Density", "Palm Swamp Medium Density", "Pasture", "Pole Forest", "Sand Bank",
            "Seasonally Flooded Forest", "Terra Firme Forest", "Urban", "Water",'Secondary Forest')
class_order = c(4,1,3,2,8,5,11,6,7,9,10,12)
y$Class <- classes
y$Order <- class_order
names(y) <- c("Accuracy", "Type","Class","Order")
y1 <- as.data.frame(x$`Pos Pred Value`) # Pos.Pred.Value is User's Acc
y1$Accuracy_Type <- 1
y1$Class <- classes
y1$Order <- class_order
names(y1) <- c("Accuracy", "Type","Class","Order")
df <- rbind(y,y1) # makes DF where each observation (class and acc) is a row, so each class
# appears twice, once for Producers, type=0, once for Users=1
df$Accuracy <- df$Accuracy*100
val = c("#00CC00", "#377EB8") # green and blue codes
lab = c("Producer's Accuracy","User's Accuracy" )

graphed <- ggplot(df, aes(x = reorder(factor(Class), Order), y = Accuracy, fill = factor(Type))) +
  geom_bar(stat = 'identity', position = "dodge", width = 0.7) +
  scale_x_discrete("Classes") + 
  scale_y_continuous("Accuracy (%)")  +
  scale_fill_manual("Legend", 
                    values = val,
                    labels = lab) +
  ggtitle(paste("Accuracy - Overall:", paste((round(valmap$performance$overall[[1]], digits=2)*100), "%", sep = ''))) + theme(text = element_text(size=20),axis.text.x = element_text(angle = 65, hjust = 1, size = 20), axis.line = element_line(colour = "black"), panel.background = element_blank())

## save conmatrix

t = valmap$performance$table
rownames(t) <- classes
colnames(t) <- classes

ord = c("Palm Swamp High Density","Palm Swamp Medium Density","Palm Swamp Low Density","Herbaceous Swamp","Pole Forest","Seasonally Flooded Forest", "Terra Firme Forest",
        "Sand Bank","Pasture","Urban", "Water",'Secondary Forest')

t = t[ord,ord]
write.csv(t, file = 'confusionMatrix_full_ARD_for90.csv')


#### Area estimation as per Olofsson et al, 2014

# first, convert raster into planar Peru Central for area est.
im18 <- raster('Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data/Full_Classification_18_RPJ.tif')
im = projectRaster(im, im18, method = 'ngb')

f = freq(im, useNA='no')
f = as.data.frame(f)
f$Area <- f$count*0.09
f$value <- classes
ordDF = as.data.frame(ord)
ff = f[order(match(f[,1], ordDF[,1])),]
farea = ff$Area

conmat = cbind(t, farea)
prop = farea/sum(farea)
conmat = cbind(conmat, prop)

# get total number of ref pixels per class. right total column from conmatrix
total_rows = rowSums(t)

conmat = cbind(conmat, total_rows)
conmat_prop = conmat[1:12,1:12]
conmat_prop = (prop*(conmat_prop/total_rows))

column_totals = colSums(conmat_prop)
conmat_prop = rbind(conmat_prop, column_totals)
area_tot = as.data.frame(farea)
area_tot = rbind(area_tot, sum(farea))

conmat_prop <- cbind(conmat_prop, area_tot)
estimated_area <- conmat_prop[13,1:12]*(sum(f$Area))

estimated_area[,13] <- NA
names(estimated_area) <- names(conmat_prop)
conmat_prop <- rbind(conmat_prop, estimated_area)

est = c(estimated_area[1:12])
est = as.data.frame(est)
est= t(est)
areas_oldnew = cbind(ff, est)
areas_oldnew = areas_oldnew[,c(1,3,4)]
names(areas_oldnew) <- c('Classes', "Mapped Areas", "Area Estimates")

x1 = areas_oldnew[,c(1,2)]
x2= areas_oldnew[,c(1,3)]
names(x1) <- c('Classes', 'Areas')
names(x2) <- c('Classes', 'Areas')
x1$Mapped_Estimation <- "Mapped"
x2$Mapped_Estimation <- "Estimation"
df = rbind(x1,x2)


val = c("#00CC00", "#377EB8") # green and blue codes
lab = c("Area Estimates" , "Mapped Areas")

graphed <- ggplot(df, aes(x = factor(Classes), y = Areas, fill = factor(Mapped_Estimation))) +
  geom_bar(stat = 'identity', position = "dodge", width = 0.7) +
  scale_x_discrete("Classes") + 
  scale_y_continuous("Area (Ha)")  +
  scale_fill_manual("Legend", 
                    values = val,
                    labels = lab)+
  ggtitle("Mapped Areas vs. Estimated Areas") + 
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 65, hjust = 1, size = 20), axis.line = element_line(colour = "black"), panel.background = element_blank())


write.csv(df, file = 'Z:/Peru_Palm_Swamp/ARD/Accuracy_ARD/area_estimation_3.csv')
rownames(conmat_prop) <- c("Palm Swamp High Density","Palm Swamp Medium Density","Palm Swamp Low Density","Herbaceous Swamp","Pole Forest","Seasonally Flooded Forest", "Terra Firme Forest",
                           "Sand Bank","Pasture","Urban", "Water",'Secondary Forest', 'Column Total', 'Estimated Areas')

# Confidence Intervals for areas
ni = total_rows
ni[c(13,14)] <- NA
conmat2 = cbind(conmat_prop, ni)
prop[c(13,14)] <- NA
conmat2 = cbind(conmat2, prop)
conmat2=conmat2[-c(13,14), ]

sp1 = function(df, col, prop, ni) (((df[col]*df[prop])-df[col]^2)/(df[ni]-1))

s_list = vector('list', 12)
for (i in 1:12){
  x = sp1(conmat2, col = i, prop = 15, ni = 14) %>% sum() %>% sqrt()
  s_list[i] <- x
}
standardError = lapply(s_list, '*', sum(conmat2$farea))
ci = lapply(standardError, '*', 2)
civ = as.numeric(ci)
x2$CI <- civ
area_est18 = x2[,c(1,2,4)]

write.csv(area_est18, file = 'Z:/Peru_Palm_Swamp/ARD/Accuracy_ARD/area_est_CI_for90.csv')
# add row proportions
row_total = rowSums(conmat_prop[1:12])
conmat_prop = cbind(conmat_prop, row_total)

conmat_prop$ua <- NA
#ua <- conmat_prop$ua
for (i in 1:12){
  conmat_prop$ua[i] <- conmat_prop[i,i]/conmat_prop[i,14]
}

for (i in 1:12){
  conmat_prop$pa[i] <- conmat_prop[i,i]/conmat_prop[13,i]
}

ua = conmat_prop[,15]
pa = conmat_prop[,16]
ua = as.data.frame(ua)
ua = ua[-c(13,14),]
pa = as.data.frame(pa)
pa = pa[-c(13,14),]
names(ua) <- c("Palm Swamp High Density","Palm Swamp Medium Density","Palm Swamp Low Density","Herbaceous Swamp","Pole Forest","Seasonally Flooded Forest", "Terra Firme Forest",
               "Sand Bank","Pasture","Urban", "Water",'Secondary Forest')
ua = as.data.frame(ua)
names(pa) <- c("Palm Swamp High Density","Palm Swamp Medium Density","Palm Swamp Low Density","Herbaceous Swamp","Pole Forest","Seasonally Flooded Forest", "Terra Firme Forest",
               "Sand Bank","Pasture","Urban", "Water",'Secondary Forest')
pa = as.data.frame(pa)
ua$Type <- "User's"
ua$Class <- c("Palm Swamp High Density","Palm Swamp Medium Density","Palm Swamp Low Density","Herbaceous Swamp","Pole Forest","Seasonally Flooded Forest", "Terra Firme Forest",
              "Sand Bank","Pasture","Urban", "Water",'Secondary Forest')
names(ua) <- c('Accuracy','Type','Class')

pa$Type <- "Producer's"
pa$Class <- c("Palm Swamp High Density","Palm Swamp Medium Density","Palm Swamp Low Density","Herbaceous Swamp","Pole Forest","Seasonally Flooded Forest", "Terra Firme Forest",
              "Sand Bank","Pasture","Urban", "Water",'Secondary Forest')
names(pa) <- c('Accuracy','Type','Class')

ua$Order <- c(1:12)
pa$Order <- c(1:12)
df1 <- rbind(ua, pa)
lab = c("Producer's Accuracy", "User's Accuracy")
df1$Accuracy = df1$Accuracy*100

graphed_acc <- ggplot(df1, aes(x = reorder(factor(Class), Order), y = Accuracy, fill = factor(Type))) +
  geom_bar(stat = 'identity', position = "dodge", width = 0.7) +
  scale_x_discrete("Classes") + 
  scale_y_continuous("Accuracy (%)")  +
  scale_fill_manual("Legend", 
                    values = val,
                    labels = lab) + 
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 65, hjust = 1, size = 20), axis.line = element_line(colour = "black"), panel.background = element_blank())

accmat <- as.matrix(conmat_prop[1:12,1:12])
d <- diag(accmat)
overall = Reduce('+', d)

write.csv(conmat_prop, file = 'Z:/Peru_Palm_Swamp/Figures/Acc_matrix_for90_AreaAdjusted.csv')

write.csv(df1, file = 'Z:/Peru_Palm_Swamp/ARD/Accuracy_ARD/df_accGraph_for1990.csv')

