library(raster)
library(ggplot2)
library(rgdal)

# Figure 6 #######
### The files created for Figure 6 are labeled "Figure_7" because this was formerly Figure 7

im18 <- raster('Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data/Full_Classification_18_RPJ.tif')
im07 = raster('Z:/Peru_Palm_Swamp/ARD/Classifications_2007/Full_StudyArea_Classification_ARD_2007_2_TempFiltered_RPJ.tif')
imStack <- stack(im07,im18) # stack two classifications
peat = raster('Z:/Peru_Palm_Swamp/Hastie/hastie_peatMask_RPJ.tif')
imStack = imStack*peat  # only includes pixels mapped by Hastie as peat, removes all else

m = c(0,13,4)
rclmat = matrix(m, ncol = 3, byrow = TRUE)
x = reclassify(im18, rclmat)
x[Which(imStack[[1]]==2 & imStack[[2]]==2)] <- 1
x[Which(imStack[[1]]==3 & imStack[[2]]==3)] <- 1
x[Which(imStack[[1]]==4 & imStack[[2]]==4)] <- 1

z2 = raster('Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data/changeAnalysis_FullStudy_ARD_2018_5_RPJ.tif')

m = c(99,102.1,2, 102.1,105.1,3, 105.1,108,NA)
rclmat = matrix(m, ncol = 3, byrow = TRUE)
z2 = reclassify(z2, rclmat)

merged = merge(z2, x)
writeRaster(merged, filename = 'Z:/Peru_Palm_Swamp/ARD/DegradationMaps/Figure7_3.tif', datatype='INT1U')

# repeat with 90
im90 = raster("Z:/Peru_Palm_Swamp/ARD/Classifications_1990_2/Expanded/Full_Classification_TempFltered_1990_RPJ.tif")
imStack <- stack(im90,im07) # stack two classifications
imStack = imStack*peat

m = c(0,13,4)
rclmat = matrix(m, ncol = 3, byrow = TRUE)
x = reclassify(im07, rclmat)
x[Which(imStack[[1]]==2 & imStack[[2]]==2)] <- 1
x[Which(imStack[[1]]==3 & imStack[[2]]==3)] <- 1
x[Which(imStack[[1]]==4 & imStack[[2]]==4)] <- 1

z2 = raster('Z:/Peru_Palm_Swamp/ARD/Classifications_1990_2/Expanded/changeAnalysis_FullStudy_ARD_2007_2.tif')

m = c(99,102.1,2, 102.1,105.1,3, 105.1,108,NA)
rclmat = matrix(m, ncol = 3, byrow = TRUE)
z2 = reclassify(z2, rclmat)

merged = merge(z2, x)
writeRaster(merged, filename = 'Z:/Peru_Palm_Swamp/ARD/DegradationMaps/Figure7_07_3.tif', datatype='INT1U')


## Figure 3, and SI 5 ######################################
#### Figure 3 was formerly Figure 4

### Make Figure 3, peat forming classes over peat
im18 = raster('Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data/Full_StudyArea_Classification_ARD_2018_2_MAJ.tif')
m = c(1,4,2,1, 4,2, 5,6, 6,5)
rclmat = matrix(m, ncol = 2, byrow = TRUE)
im18 = reclassify(im18, rclmat)
writeRaster(im18, filename = 'Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data/Full_Classification_ARD_2018_Fig4.tif', datatype='INT2S')
m = c(5.1,12.1,NA)
rclmat = matrix(m, ncol = 3, byrow = TRUE)
im18 = reclassify(im18, rclmat)
peat = raster('Z:/Peru_Palm_Swamp/Hastie/hastie_peatMask.tif')
im18 = im18*peat
writeRaster(im18, filename = 'Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data/Full_Classification_ARD_2018_Fig4_peatOnly.tif', datatype='INT2S')

# SI 5 ###
### SI 5 was formerly SI 6 ##
m = c(1,4,2,1, 4,2, 5,6, 6,5)
rclmat = matrix(m, ncol = 2, byrow = TRUE)
im7_si6 = reclassify(im07, rclmat)
writeRaster(im7_si6, filename = 'Z:/Peru_Palm_Swamp/ARD/Classifications_2007/im7_SI6.tif', datatype='INT1U')
# do with 90 when ready
im90 = raster('Z:/Peru_Palm_Swamp/ARD/Classifications_1990_2/Expanded/Full_Classification_TempFltered_1990_RPJ_2.tif')
im90_si6 = reclassify(im90, rclmat)
writeRaster(im90_si6, filename = 'Z:/Peru_Palm_Swamp/ARD/Classifications_1990_2/Expanded/im90_SI6.tif', datatype='INT1U')

### Table 1 ###########################
### Used to calculate ratio of peat_forming class area / peat-forming class area over peat

im18 <- raster('Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data/Full_Classification_18_RPJ.tif')
m = c(1,4,2,1, 4,2, 5,6, 6,5)
rclmat = matrix(m, ncol = 2, byrow = TRUE)
im18 = reclassify(im18, rclmat)
m = c(5.1,12.1,NA)
rclmat = matrix(m, ncol = 3, byrow = TRUE)
im18 = reclassify(im18, rclmat)
f = freq(im18, useNA='no')
f = as.data.frame(f)
f$Area = f$count*0.09
im18 = im18*peat
fp = freq(im18, useNA='no')
fp = as.data.frame(fp)
fp$Area = fp$count*0.09
nams = c('PS High Density', 'PS Medium Density', 'PS Low Density', 'HS','PF')
df = cbind(nams, f$Area, fp$Area)
df = as.data.frame(df)
names(df) <- c('Class','Mapped Area', 'Mapped Area over Peat')

df$`Mapped Area` = as.numeric(df$`Mapped Area`)
df$`Mapped Area over Peat` = as.numeric(df$`Mapped Area over Peat`)
df$Share_Over_Peat <- round((df$`Mapped Area over Peat`/df$`Mapped Area`)*100)
write.csv(df, file = 'Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data/area_over_peat.csv')



library(ggplot2)
library(raster)

####### Accuracy graph Figure 4; bring 3 figures together in one facet wrapped figure ######
setwd('Z:/Peru_Palm_Swamp/ARD/Accuracy_ARD')

df18=read.csv('df_accGraph_2018_3.csv')
df07=read.csv('df_accGraph_2007_3.csv')

df90=read.csv('df_accGraph_for1990.csv')
df1=df1[-4]

## use acronyms in Names
df90$Class <- c("PS High Density","PS Medium Density","PS Low Density","HS","PF","Seasonally Flooded Forest", "Terra Firme Forest",
               "Sand Bank","Pasture","Urban", "Water",'Secondary Forest', "PS High Density","PS Medium Density","PS Low Density","HS","PF","Seasonally Flooded Forest", "Terra Firme Forest",
               "Sand Bank","Pasture","Urban", "Water",'Secondary Forest')
df18$Class <- c("PS High Density","PS Medium Density","PS Low Density","HS","PF","Seasonally Flooded Forest", "Terra Firme Forest",
                "Sand Bank","Pasture","Urban", "Water",'Secondary Forest', "PS High Density","PS Medium Density","PS Low Density","HS","PF","Seasonally Flooded Forest", "Terra Firme Forest",
                "Sand Bank","Pasture","Urban", "Water",'Secondary Forest')
df07$Class <- c("PS High Density","PS Medium Density","PS Low Density","HS","PF","Seasonally Flooded Forest", "Terra Firme Forest",
                "Sand Bank","Pasture","Urban", "Water",'Secondary Forest', "PS High Density","PS Medium Density","PS Low Density","HS","PF","Seasonally Flooded Forest", "Terra Firme Forest",
                "Sand Bank","Pasture","Urban", "Water",'Secondary Forest')

### graphs were read in by rerunning above ggplot codes after df1 and df2 were fixed

# add years to dfs, then bring dfs together

df90$Year <- 1990
df18$Year <- 2018
df07$Year <- 2007

df18=df18[,2:6]
df07=df07[,2:6]
df90=df90[,2:6]
dfs = rbind(df18,df07,df90)

val = c("#00CC00", "#377EB8") # green and blue codes
lab = c("Producer's Accuracy","User's Accuracy" )

# subpanels vertical
facetGraph <-  ggplot(dfs, aes(x = reorder(factor(Class), Order), y = Accuracy, fill = factor(Type))) +
  geom_bar(stat = 'identity', position = "dodge", width = 0.7) +
  scale_x_discrete("Classes") + 
  scale_y_continuous("Accuracy (%)")  +
  scale_fill_manual("Legend", 
                    values = val,
                    labels = lab) + 
  facet_grid(Year~.) +
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 65, hjust = 1, size = 20), axis.line = element_line(colour = "black"), panel.background = element_blank())


## overall accuracies
conmat_prop =read.csv('Z:/Peru_Palm_Swamp/Figures/Acc_matrix_2018_AreaAdjusted_2.csv')
conmat_prop = conmat_prop[-1]

accmat <- as.matrix(conmat_prop[1:12,1:12])
d <- diag(accmat)
overall = Reduce('+', d)
# 93

conmat_prop =read.csv('Z:/Peru_Palm_Swamp/Figures/Acc_matrix_2007_AreaAdjusted_2.csv')
conmat_prop = conmat_prop[-1]

accmat <- as.matrix(conmat_prop[1:12,1:12])
d <- diag(accmat)
overall = # 84

conmat_prop=read.csv(file = 'Z:/Peru_Palm_Swamp/Figures/Acc_matrix_for90_AreaAdjusted.csv')
conmat_prop = conmat_prop[-1]

accmat <- as.matrix(conmat_prop[1:12,1:12])
d <- diag(accmat)
overall = Reduce('+', d)


# SI 4: 2018 classification peat filtered all classes
## formerly called SI12

im18 <- raster('Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data/Full_Classification_18_RPJ.tif')
peat = raster('Z:/Peru_Palm_Swamp/Hastie/hastie_peatMask_RPJ.tif')
im18 = im18*peat
m = c(1,4,2,1, 4,2, 5,6, 6,5)
rclmat = matrix(m, ncol = 2, byrow = TRUE)
im18 = reclassify(im18, rclmat)
writeRaster(im18, filename = 'Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data/Full_Classification_18_RPJ_SI12.tif', datatype='INT1U')
im18 = raster('Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data/Full_Classification_18_RPJ_SI12.tif')
f = freq(im18, useNA='no')
f[8,]*0.09
f[9,]*0.09
f = as.data.frame(f)
f$Proportion <- (f$count/sum(f$count))*100
f$Area = f$count*0.09


# Figure 5; accuracy of transitions

setwd('Z:/Peru_Palm_Swamp/ARD/Accuracy_ARD')
tacc18 = read.csv('transition_Acc_18.csv')
tacc07 = read.csv('transition_Acc_07.csv')
tacc18$Period = '2007 - 2018'
tacc07$Period = '1990 - 2007'

# remove PS to PF; rows 7, 8 & 15, 16
tacc07 = tacc07[-c(7,8,15,16),]
tacc18 = tacc18[-c(7,8,15,16),]


df_tacc = rbind(tacc18,tacc07)
val = c("#00CC00", "#377EB8") # green and blue codes
lab = c("Producer's Accuracy","User's Accuracy" )

facetGraph <-  ggplot(df_tacc, aes(x = reorder(factor(Transitions), Order), y = Accuracy, fill = factor(Type))) +
  geom_bar(stat = 'identity', position = "dodge", width = 0.7) +
  scale_x_discrete("Transitions") + 
  scale_y_continuous(limits = c(0,100))  +
  scale_fill_manual("Legend", 
                    values = val,
                    labels = lab) + 
  facet_grid(Period~.) +
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 65, hjust = 1, size = 20), axis.line = element_line(colour = "black"), panel.background = element_blank())


### degradation within the pacaya samaria

pacsam = readOGR('Z:/Peru_Palm_Swamp/Shapefiles/Pacaya_Samaria', 'Pacaya_Samaria')
change07 = raster('Z:/Peru_Palm_Swamp/ARD/Classifications_1990_2/Expanded/changeAnalysis_FullStudy_ARD_2007_3.tif')
change18 = raster('Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data/changeAnalysis_FullStudy_ARD_2018_5_RPJ.tif')
pacsam = spTransform(pacsam, crs(change07))
change07 = mask(change07, pacsam)
f07 = freq(change07, useNA='no')

change18 = mask(change18, pacsam)
f18 = freq(change18, useNA='no')
sum(f07[c(1:3),2])/sum(f18[c(1:3),2])
sum(f07[c(1:3),2])/(2007-1990)
sum(f18[c(1:3),2])/(2018-2007)

deg07=sum(f07[c(1:3),2])*0.09
deg18=sum(f18[c(1:3),2])*0.09

deg07/(2007-1990)
deg18/(2018-2007)
### area of peatland
fpeat = 70676068
fpeat*0.09

### degradation within Pastaza-Maranon zone
pm = readOGR('Z:/Peru_Palm_Swamp/Shapefiles', 'PMFB_zone')
change07 = raster('Z:/Peru_Palm_Swamp/ARD/Classifications_1990_2/Expanded/changeAnalysis_FullStudy_ARD_2007_3.tif')
change18 = raster('Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data/changeAnalysis_FullStudy_ARD_2018_5_RPJ.tif')
pm = spTransform(pm, crs(change07))
change07 = mask(change07, pm)
f07 = freq(change07, useNA='no')

change18 = mask(change18, pm)
f18 = freq(change18, useNA='no')

sum(f07[c(1:3),2])/sum(f18[c(1:3),2])
sum(f07[c(1:3),2])/(2007-1990)
sum(f18[c(1:3),2])/(2018-2007)

deg07=sum(f07[c(1:3),2])*0.09
deg18=sum(f18[c(1:3),2])*0.09

x=deg07/(2007-1990)
y=deg18/(2018-2007)

perc = ((y-x)/x)*100

# defor pas mar
def07=sum(f07[c(4:6),2])*0.09
def18=sum(f18[c(4:6),2])*0.09

xx=def07/(2007-1990)
yy=def18/(2018-2007)

perc = ((yy-xx)/xx)*100

# degradation within the Ucayali zone

zone = readOGR('Z:/Peru_Palm_Swamp/Shapefiles','South_studyArea')
change07 = raster('Z:/Peru_Palm_Swamp/ARD/Classifications_1990_2/Expanded/changeAnalysis_FullStudy_ARD_2007_3.tif')
change18 = raster('Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data/changeAnalysis_FullStudy_ARD_2018_5_RPJ.tif')

change07 = mask(change07, zone)
f07 = freq(change07, useNA='no')

change18 = mask(change18, zone)
f18 = freq(change18, useNA='no')

deg07=sum(f07[c(1:3),2])*0.09
deg18=sum(f18[c(1:3),2])*0.09

deg07/(2007-1990)
deg18/(2018-2007)

#### Results section; Percentage of degraded peatlands in 18, 07; proportion relative to peat forming classes over peat
peat = raster('Z:/Peru_Palm_Swamp/Hastie/hastie_peatMask_RPJ.tif')
fpeat = freq(peat)
areaPeat = fpeat[1,2]*0.09
# areaPeat is 6360846 ha
