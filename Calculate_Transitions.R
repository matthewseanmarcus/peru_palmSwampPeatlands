library(raster)
library(ggplot2)
library(ggpubr)
library(rgdal)
setwd('Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data')

dir.create('tempfiledir3')
tempdir=paste(getwd(),'tempfiledir3', sep="/")
rasterOptions(tmpdir=tempdir)

pmfb = raster('pasmar_ard_18_5.tif')
east = raster('east_ard_18_3_MAJ.tif')
pacsam = raster('PacSam_ARD_2018_2.tif')
napo = raster('napo_ard_18_3.tif')
ne = raster('putumayo_ard_18_2.tif')
south = raster('south_ard_18_2_MAJ.tif')

m <- c(5,6, 6,7, 7,8, 8,9, 9,10,10,11)
rcl_new <- matrix(m, ncol = 2, byrow = TRUE)
napo <- reclassify(napo, rcl_new)

m_south = c(6,7, 7,8, 8,12)
rcl_sou <- matrix(m_south, ncol = 2, byrow = TRUE)
south <- reclassify(south, rcl_sou)

# mask east
msk = readOGR('Z:/Peru_Palm_Swamp/Shapefiles', 'East_crop')
east = mask(east, msk)

im18 <- merge(pmfb, east, pacsam, napo, ne, south) 
writeRaster(im18, filename = 'Full_StudyArea_Classification_ARD_2018_2.tif', datatype='INT2S')
# then do majority filter in arc

# create im07
setwd('Z:/Peru_Palm_Swamp/ARD/Classifications_2007')

pmfb = raster('pasmar07_2.tif')
east = raster('east07_MAJ.tif')
pacsam = raster('pacsam07_2.tif')
napo = raster('napo07_2.tif')
ne = raster('putumayo07_2.tif')
south = raster('south07_MAJ.tif')

napo <- reclassify(napo, rcl_new)

south <- reclassify(south, rcl_sou)

# mask east
east = mask(east, msk)

im07 <- merge(pmfb, east, pacsam, napo, ne, south) 
plot(im07)
writeRaster(im07, filename = 'Full_StudyArea_Classification_ARD_2007_2.tif', datatype='INT2S')

# bring in majority filtered images:
im18 <- raster('Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data/Full_StudyArea_Classification_ARD_2018_2_MAJ.tif')
im07 <- raster('Z:/Peru_Palm_Swamp/ARD/Classifications_2007/Full_StudyArea_Classification_ARD_2007_2_MAJ.tif')

imStack <- stack(im07,im18) # stack two classifications
x <- im07
# Temporal Filter
x[Which(imStack[[2]]==2) & (imStack[[1]]==3 | imStack[[1]]==4)] <- 2
x[Which(imStack[[2]]==4 & imStack[[1]]==3)] <- 4
x[Which(imStack[[2]]==2 & (imStack[[1]]!=2 & imStack[[1]]!=3 & imStack[[1]]!=4))] <- 2
x[Which(imStack[[2]]==3 & (imStack[[1]]!=2 & imStack[[1]]!=3 & imStack[[1]]!=4))] <- 3
x[Which(imStack[[2]]==4 & (imStack[[1]]!=2 & imStack[[1]]!=3 & imStack[[1]]!=4))] <- 4
x[Which(imStack[[2]]==9 & (imStack[[1]]==2 | imStack[[1]]==3 | imStack[[1]]==4 | imStack[[1]]==6))] <- 9
writeRaster(x, filename = "Full_StudyArea_Classification_ARD_2007_2_TempFiltered.tif", datatype='INT2S')
##
setwd('Z:/Peru_Palm_Swamp/ARD/Classifications_1990_2/Expanded')

pmfb = raster('pmfb90.tif')
east = raster('east90.tif')
pacsam = raster('pacsam90.tif')
napo = raster('napo90.tif')
ne = raster('putu90.tif')
south = raster('south90.tif')

east <- reclassify(east, rcl_new)
east = mask(east, msk)
napo <- reclassify(napo, rcl_new)
south <- reclassify(south, rcl_sou)

im90 <- merge(pmfb, east, pacsam, napo, ne, south) 
writeRaster(im90, filename = 'Full_StudyArea_Classification_ARD_expanded_1990.tif', datatype='INT2S')


im07 <- raster('Z:/Peru_Palm_Swamp/ARD/Classifications_2007/Full_StudyArea_Classification_ARD_2007_2_TempFiltered.tif')
im90 = raster('Full_StudyArea_Classification_ARD_expanded_1990_MAJ.tif')
im90 <- crop(im90, im07)
imStack <- stack(im90, im07)

x <- im90
x[Which(imStack[[2]]==2) & (imStack[[1]]==3 | imStack[[1]]==4)] <- 2
x[Which(imStack[[2]]==4 & imStack[[1]]==3)] <- 4
x[Which(imStack[[2]]==2 & (imStack[[1]]!=2 & imStack[[1]]!=3 & imStack[[1]]!=4))] <- 2
x[Which(imStack[[2]]==3 & (imStack[[1]]!=2 & imStack[[1]]!=3 & imStack[[1]]!=4))] <- 3
x[Which(imStack[[2]]==4 & (imStack[[1]]!=2 & imStack[[1]]!=3 & imStack[[1]]!=4))] <- 4
x[Which(imStack[[2]]==9 & (imStack[[1]]==2 | imStack[[1]]==3 | imStack[[1]]==4 | imStack[[1]]==6))] <- 9
writeRaster(x, filename = "Full_Classification_TempFltered_1990.tif", datatype='INT2S')



### pix reference
# 1 = Open Peat, 2 = High Density, 3 = Low Density, 4 = Med Density, 5 = Pasture, 6 = Pole Forest, 7 = Sand Banks,
#  8 = SFF, 9 = TF, 10 = Urban, 11 = Water Bodies, 12 = Secondary Forest

im18 <- raster('Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data/Full_Classification_18_RPJ.tif')
writeRaster(im07, filename = 'Full_StudyArea_Classification_ARD_2007_2_TempFiltered_RPJ.tif')
im07 = raster('Z:/Peru_Palm_Swamp/ARD/Classifications_2007/Full_StudyArea_Classification_ARD_2007_2_TempFiltered_RPJ.tif')
imStack <- stack(im07,im18) # stack two classifications

# peat = raster('Z:/Peru_Palm_Swamp/Hastie/hastie_peatMask.tif')
# peat = projectRaster(peat, im18, method = 'ngb')
# writeRaster(peat, filename = 'Z:/Peru_Palm_Swamp/Hastie/hastie_peatMask_RPJ.tif', datatype='INT2S')
peat = raster('Z:/Peru_Palm_Swamp/Hastie/hastie_peatMask_RPJ.tif')
imStack = imStack*peat  # only includes pixels mapped by Hastie as peat, removes all else


# High to Low -> 100
# med to Low ->  101
# High to Med -> 102
# High dense Palm to open peat -> 103
# Med dense palm to open peat -> 104
# Low dense palm to open peat -> 105
# High Density Palm to woody -> 106
# Med Density Palm to woody -> 107

mskVal <- c(0,8.1,1, 8.9,9.1,NA, 9.2,11.1,1, 11.2, 12.1, NA) # removes TF and secondary
rclmsk <- matrix(mskVal, ncol = 3, byrow = TRUE)
msk <- reclassify(imStack[[2]], rclmsk)
imStack <- imStack*msk

x = imStack[[2]]
x[Which(imStack[[1]]==2 & imStack[[2]]==3)] <- 100
x[Which(imStack[[1]]==4 & imStack[[2]]==3)] <- 101
x[Which(imStack[[1]]==2 & imStack[[2]]==4)] <- 102

x[Which(imStack[[1]]==2 & imStack[[2]]==1)] <- 103
x[Which(imStack[[1]]==4 & imStack[[2]]==1)] <- 104
x[Which(imStack[[1]]==3 & imStack[[2]]==1)] <- 105

x[Which(imStack[[1]]==2 & imStack[[2]]==6)] <- 106
x[Which(imStack[[1]]==4 & imStack[[2]]==6)] <- 107

m=c(0,99,NA)
rclmat2 = matrix(m, ncol = 3, byrow = TRUE)
z2 = reclassify(x, rclmat2)

writeRaster(z2, filename = 'changeAnalysis_FullStudy_ARD_2018_5_RPJ.tif', datatype='INT2S')

z2 = raster('changeAnalysis_FullStudy_ARD_2018_1.tif')

### repeat for 2007 to 1990 ##################################################


im90 <- raster("Full_Classification_TempFltered_1990.tif")
im07 = raster('Z:/Peru_Palm_Swamp/ARD/Classifications_2007/Full_StudyArea_Classification_ARD_2007_2_TempFiltered_RPJ.tif')
im90 = projectRaster(im90, im07, method = 'ngb')
writeRaster(im90, filename = 'Full_Classification_TempFltered_1990_RPJ.tif', datatype='INT2S')
imStack <- stack(im90, im07)
mskVal <- c(0,8.1,1, 8.9,9.1,NA, 9.2,11.1,1, 11.2, 12.1, NA) # removes TF and secondary
rclmsk <- matrix(mskVal, ncol = 3, byrow = TRUE)
msk <- reclassify(imStack[[2]], rclmsk)
imStack <- imStack*msk

imStack = imStack*peat

x = imStack[[2]]
x[Which(imStack[[1]]==2 & imStack[[2]]==3)] <- 100
x[Which(imStack[[1]]==4 & imStack[[2]]==3)] <- 101
x[Which(imStack[[1]]==2 & imStack[[2]]==4)] <- 102

x[Which(imStack[[1]]==2 & imStack[[2]]==1)] <- 103
x[Which(imStack[[1]]==4 & imStack[[2]]==1)] <- 104
x[Which(imStack[[1]]==3 & imStack[[2]]==1)] <- 105

x[Which(imStack[[1]]==2 & imStack[[2]]==6)] <- 106
x[Which(imStack[[1]]==4 & imStack[[2]]==6)] <- 107

m=c(0,99,NA)
rclmat2 = matrix(m, ncol = 3, byrow = TRUE)
y = reclassify(x, rclmat2)

writeRaster(y, filename = 'Z:/Peru_Palm_Swamp/ARD/Classifications_1990_2/Expanded/changeAnalysis_FullStudy_ARD_2007_2.tif', datatype='INT2S')
y=raster('Z:/Peru_Palm_Swamp/ARD/Classifications_1990_2/Expanded/changeAnalysis_FullStudy_ARD_2007_2.tif')