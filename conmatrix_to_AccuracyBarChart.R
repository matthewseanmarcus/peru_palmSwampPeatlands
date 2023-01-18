library(rgdal)
library(raster)
library(tidyverse)

# This code takes a mapValidation object, made from validateMap() in RSToolbox,
# and then adjusts the accuracy matrix using Pontus et al 2014 methods

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

t = valmap$performance$table # cofusion matrix
rownames(t) <- classes
colnames(t) <- classes

ord = c("Palm Swamp High Density","Palm Swamp Medium Density","Palm Swamp Low Density","Herbaceous Swamp","Pole Forest","Seasonally Flooded Forest", "Terra Firme Forest",
        "Sand Bank","Pasture","Urban", "Water",'Secondary Forest')

t = t[ord,ord]
write.csv(t, file = 'confusionMatrix_full_ARD_2018_2.csv')


#### Area estimation as per Pontus

im18 <- raster('Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data/Full_Classification_18_RPJ.tif')
#t = read.csv(file = 'Z:/Peru_Palm_Swamp/ARD/Accuracy_ARD/confusionMatrix_full_ARD_2018_2.csv')
f = freq(im18, useNA='no')
f = as.data.frame(f)
f$Area <- f$count*0.09 # converts pixel resolution of 30m to hectares
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

s_list = vector('list', 12) # number of classes
for (i in 1:12){
  x = sp1(conmat2, col = i, prop = 15, ni = 14) %>% sum() %>% sqrt()
  s_list[i] <- x
}
standardError = lapply(s_list, '*', sum(conmat2$farea))
ci = lapply(standardError, '*', 2) # 1.96 
civ = as.numeric(ci)
x2$CI <- civ
area_est18 = x2[,c(1,2,4)]

write.csv(area_est18, file = 'Z:/Peru_Palm_Swamp/ARD/Accuracy_ARD/area_est_CI_18.csv')
# add row proportions
row_total = rowSums(conmat_prop[1:12]) # number of classes
conmat_prop = cbind(conmat_prop, row_total)

#calculate user's and producer's accuracies
conmat_prop$ua <- NA
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

write.csv(conmat_prop, file = 'Z:/Peru_Palm_Swamp/Figures/Acc_matrix_2018_AreaAdjusted_2.csv')
