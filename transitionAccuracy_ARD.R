library(raster)
library(rgdal)
library(ggplot2)
library(caret)

# This code calculates the accuracy of the degradation transitions: 'PS high to low', 'PS Med to Low', 'PS High to Med', 
# 'PS high to HS', 'PS Med to HS','PS Low to HS', 'PS High to PF', 'PS Med to PF'. (PS = palm swamp)

setwd('Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data')

################### read in df1 and df2 from conmatrix_to_AccuracyBarChart.R in order to create df below

df18 <- read.csv('Z:/Peru_Palm_Swamp/ARD/Accuracy_ARD/df_accGraph_2018_3.csv')
df07 <- read.csv('Z:/Peru_Palm_Swamp/ARD/Accuracy_ARD/df_accGraph_2007_3.csv')

#2018
ua18 = df18[1:12,c(4,2)]
ua18$Accuracy = ua18$Accuracy/100

pa18 = df18[13:24, c(4,2)]
pa18$Accuracy = pa18$Accuracy/100

#2007
ua07 = df07[1:12,c(4,2)]
ua07$Accuracy = ua07$Accuracy/100

pa07 = df07[13:24, c(4,2)]
pa07$Accuracy = pa07$Accuracy/100

df = cbind(ua18,ua07,pa18,pa07)
class_order = c(4,1,3,2,8,5,11,6,7,9,10,12)
df=df[class_order,]
xxx <- df[c(1,2,4,6,8)]
names(xxx) <- c('Class','UA18','UA07','PA18','PA07')

## transitions df

transitions = c('PS High to Low Density', 'PS Med to Low Density', 'PS High to Med Density',  'PS High Density to HS',
                 'PS Med Density to HS', 'PS Low Density to HS', 'PS High Density Palm to PF',
                'PS Med Density Palm to PF')

df <- xxx[c(1,3,2)]

ua_trans <- c((df[[2]][[2]]*df[[3]][[3]]), (df[[2]][[4]]*df[[3]][[3]]), (df[[2]][[2]]*df[[3]][[4]]), (df[[2]][[2]]*df[[3]][[1]]), 
              (df[[2]][[4]]*df[[3]][[1]]), (df[[2]][[3]]*df[[3]][[1]]), (df[[2]][[2]]*df[[3]][[6]]), (df[[2]][[4]]*df[[3]][[6]]))              #(df[[2]][[3]]*df[[3]][[6]]))

df <- xxx[c(1,5,4)]
pa_trans <- c((df[[2]][[2]]*df[[3]][[3]]), (df[[2]][[4]]*df[[3]][[3]]), (df[[2]][[2]]*df[[3]][[4]]), (df[[2]][[2]]*df[[3]][[1]]), 
              (df[[2]][[4]]*df[[3]][[1]]), (df[[2]][[3]]*df[[3]][[1]]), (df[[2]][[2]]*df[[3]][[6]]), (df[[2]][[4]]*df[[3]][[6]]))
              

df_fullTrans <- cbind(transitions,ua_trans,pa_trans)
df_fullTrans <- as.data.frame(df_fullTrans)

y = df_fullTrans[1:2]
names(y) <- c('Transitions','Accuracy')
y$Type <- "User's Accuracy"


y1 <- (df_fullTrans[c(1,3)])
names(y1) <- c('Transitions','Accuracy')
y1$Type <- "Producer's Accuracy"

df_acc <- rbind(y,y1)

df_acc$Accuracy <- as.numeric(df_acc$Accuracy)

df_acc$Accuracy <- df_acc$Accuracy*100

# put in same order as in paper
ord = c(2,3,1,6,7,8,4,5,2,3,1,6,7,8,4,5)
df_acc$Order <- ord

val = c( "#00CC00","#377EB8") # green and blue codes
lab = c("Producer's Accuracy","User's Accuracy")

graphed <- ggplot(df_acc, aes(x = reorder(factor(Transitions), Order), y = Accuracy, fill = factor(Type))) +
  geom_bar(stat = 'identity', position = "dodge", width = 0.7) +
  scale_x_discrete("Transitions") + 
  scale_y_continuous("Accuracy (%)", limits = c(0,100))  +
  scale_fill_manual("Legend", 
                    values = val,
                    labels = lab) +
 theme(text = element_text(size=20),axis.text.x = element_text(angle = 65, hjust = 1, size = 30), axis.line = element_line(colour = "black"), panel.background = element_blank())



write.csv(df_acc, file = 'Z:/Peru_Palm_Swamp/ARD/Accuracy_ARD/transition_Acc_18.csv')


df_acc_use18 = read.csv(file = 'Z:/Peru_Palm_Swamp/ARD/Accuracy_ARD/transition_Acc_18.csv')
df_acc_use18 = df_acc_use18[-1]
#### Area estimation as per Pontus' methods

im18 <- raster('changeAnalysis_FullStudy_ARD_2018_5_RPJ.tif')
f = freq(im18, useNA='no')
f = as.data.frame(f)

f$Area <- f$count*0.09
transitions = c('PS high to low', 'PS Med to Low', 'PS High to Med', 'PS high to HS', 'PS Med to HS',
                'PS Low to HS', 'PS High to PF', 'PS Med to PF')
f$value <- transitions
ordDF = as.data.frame(ord)
ff = f[order(match(f[,1], ordDF[,1])),]
farea = ff$Area

ua = df_acc_use18[c(1:8),2]/100
prop = farea/sum(farea)
areas = (ua*prop)*sum(farea) # multiplies the UA by its area proportion, then multiplies by full area of transitions mapped
areas = as.numeric(areas)
areasRound = round(areas, digits = 2)
t = cbind(transitions, areasRound)
t = as.data.frame(t)
write.csv(t, file = 'Z:/Peru_Palm_Swamp/ARD/Accuracy_ARD/transition_area_estimates_07to18.csv')

# calculate confidence intervals for areas
t = read.csv('Z:/Peru_Palm_Swamp/ARD/Accuracy_ARD/transition_area_estimates_07to18.csv')
t = t[-9,-1]
area_est07 =read.csv('Z:/Peru_Palm_Swamp/ARD/Accuracy_ARD/area_est_CI_07.csv')
area_est18 =read.csv('Z:/Peru_Palm_Swamp/ARD/Accuracy_ARD/area_est_CI_18.csv')
area_est18 = area_est18[-1]
area_est07 = area_est07[-1]
# convert CI to SE (by dividing CI_prop by 2), then reproduce Proportion, and recalculate SE below
area_est18$CI_Proportion = area_est18$CI_Proportion/2
area_est07$CI_Proportion = area_est07$CI_Proportion/2

# area of transition, X CI_Proportion for Before class, then same to after. Add for trans CI
h_l = (t[1,2]*area_est07[1,4]) + (t[1,2]*area_est18[3,4])
m_l = (t[2,2]*area_est07[2,4]) + (t[2,2]*area_est18[3,4])
h_m = (t[3,2]*area_est07[1,4]) + (t[3,2]*area_est18[2,4])

h_hs = (t[4,2]*area_est07[1,4]) + (t[4,2]*area_est18[4,4])
m_hs = (t[5,2]*area_est07[2,4]) + (t[5,2]*area_est18[4,4])
l_hs = (t[6,2]*area_est07[3,4]) + (t[6,2]*area_est18[4,4])

h_p = (t[7,2]*area_est07[1,4]) + (t[7,2]*area_est18[5,4])
m_p = (t[8,2]*area_est07[2,4]) + (t[8,2]*area_est18[5,4])

ciTrans = list(h_l, m_l, h_m, h_hs, m_hs, l_hs, h_p, m_p)
t$CI = as.numeric(ciTrans)
t$Order = 1:8
names(t) <- c('Transitions','Area (ha)', 'CI', 'Order')
write.csv(t, file = 'Z:/Peru_Palm_Swamp/ARD/Accuracy_ARD/trans_area_07_18_CI.csv')

graphed_trans <- ggplot(t, aes(x = reorder(transitions, Order), y = areasRound)) +
  geom_bar(stat = 'identity', width = 0.7, fill = 'Blue') +
  geom_errorbar(aes(ymin=areasRound-CI, ymax=areasRound+CI), width=.2,
                position=position_dodge(.9)) +
  scale_x_discrete('Classes') + 
  scale_y_continuous("Area (ha)") +  
  ggtitle("Transition Area Estimations 2007-2018") +
  theme(text = element_text(size=16), axis.text.x = element_text(angle = 75, hjust = 1, size = 20), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title=element_text(size=14,face="bold")) 



im07 <- raster('change07_SItrans_2.tif')
f = freq(im07, useNA='no')
f = as.data.frame(f)
f = f[-9,]

f$Area <- f$count*0.09
transitions = c('PS high to low', 'PS Med to Low', 'PS High to Med', 'PS high to HS', 'PS Med to HS',
                'PS Low to HS', 'PS High to PF', 'PS Med to PF')
f$value <- transitions
ordDF = as.data.frame(ord)
ff = f[order(match(f[,1], ordDF[,1])),]
farea = ff$Area

df_acc_use07 = df_acc_use[c(1:16),]
ua = df_acc_use07[c(1:8),3]/100
prop = farea/sum(farea)
areas = (ua*prop)*sum(farea) # multiplies the UA by its area proportion, then multiplies by full area of transitions mapped
areas = as.numeric(areas)
areasRound = round(areas, digits = 2)
t$Areas2007 <- areasRound
names(t) <- c('Transitions', 'Areas_18', 'Areas_07')
write.csv(t, file = 'Z:/Peru_Palm_Swamp/Figures/transition_area_estimates_BothPeriods.csv')
