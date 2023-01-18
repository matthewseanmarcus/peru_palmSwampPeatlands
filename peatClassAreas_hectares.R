library(raster)
library(rgdal)

pacsam = readOGR('Z:/Peru_Palm_Swamp/Shapefiles/Pacaya_Samaria', 'Pacaya_Samaria')

# get total mapped area of degradation within whole study area for both
# periods, then get ratio of that olofsson area / total area
# Then get degradation area within each Geographic Area, apply ratio
# Then apply peat filter, calculate areas

im18_full <- raster('Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data/Full_Classification_18_RPJ.tif')
m = c(1,4, 2,1, 4,2, 5,6, 6,5)
rclmat = matrix(m, ncol = 2, byrow = TRUE)
im18_full = reclassify(im18_full, rclmat)
# remove non peat forming for im18
m = c(5.1,13,NA)
rclmat1 = matrix(m, ncol = 3, byrow = TRUE)
im18 = reclassify(im18_full, rclmat1)

pacsam = spTransform(pacsam, crs(im18))
im18_ps = mask(im18, pacsam)
f_im18_ps = freq(im18_ps, useNA='no')

# area of mapped total of deg , and ratio to estimate, 18

f_im18_full = freq(im18_full, useNA='no')
# do for all 5 peat forming classes
total_high = f_im18_full[1,2]*0.09
total_med = f_im18_full[2,2]*0.09
total_low = f_im18_full[3,2]*0.09
total_hs = f_im18_full[4,2]*0.09
total_pf = f_im18_full[5,2]*0.09

high_est = 1597579
med_est = 1808226 
low_est = 2077066 
hs_est = 627307
pf_est = 793677

ratio_high = high_est/total_high
ratio_med = med_est/total_med
ratio_low = low_est/total_low
ratio_hs = hs_est/total_hs
ratio_pf = pf_est/total_pf

SE_high = 66382/high_est
SE_med = 80390/med_est
SE_low = 54076/low_est
SE_hs = 3909/hs_est
SE_pf = 20931/pf_est

## applly peat filter to im18, then mask geographic area
peat = raster('Z:/Peru_Palm_Swamp/Hastie/hastie_peatMask_RPJ.tif')

im18 = im18*peat

im18_ps = mask(im18, pacsam)
f_im18_ps = freq(im18_ps, useNA='no')

# apply ratios to mapped classes within pacsam
high_ps_18 = round((f_im18_ps[1,2]*0.09)*(ratio_high), digits = 2)
med_ps_18 = round((f_im18_ps[2,2]*0.09)*(ratio_med), digits = 2)
low_ps_18 = round((f_im18_ps[3,2]*0.09)*(ratio_low), digits = 2)
hs_ps_18 = round((f_im18_ps[4,2]*0.09)*(ratio_hs), digits = 2)
pf_ps_18 = round((f_im18_ps[5,2]*0.09)*(ratio_pf), digits = 2)

rows = c('PS high', 'PS med', 'PS low', 'HS', 'PF')
df = data.frame(row.names = rows)
vec = c(high_ps_18, med_ps_18, low_ps_18, hs_ps_18, pf_ps_18)
vec.se = c(round(high_ps_18*SE_high, digits = 2), round(med_ps_18*SE_med, digits = 2), 
           round(SE_low*low_ps_18, digits = 2), round(SE_hs*hs_ps_18, digits = 2), 
          round(pf_ps_18*SE_pf, digits = 2))
## change name of zone before running!
df = cbind(df, Pacaya_Samiria = vec, SE = vec.se)


############## repeat above with pmfb zone ##################

pm = readOGR('Z:/Peru_Palm_Swamp/Shapefiles', 'PMFB_zone')
pm = spTransform(pm, crs(im18))
im18_ps = mask(im18, pm)
f_im18_ps = freq(im18_ps, useNA='no')

# apply ratios to mapped classes within pmfb
high_ps_18 = round((f_im18_ps[1,2]*0.09)*(ratio_high), digits = 2)
med_ps_18 = round((f_im18_ps[2,2]*0.09)*(ratio_med), digits = 2)
low_ps_18 = round((f_im18_ps[3,2]*0.09)*(ratio_low), digits = 2)
hs_ps_18 = round((f_im18_ps[4,2]*0.09)*(ratio_hs), digits = 2)
pf_ps_18 = round((f_im18_ps[5,2]*0.09)*(ratio_pf), digits = 2)

rows = c('PS high', 'PS med', 'PS low', 'HS', 'PF')
vec = c(high_ps_18, med_ps_18, low_ps_18, hs_ps_18, pf_ps_18)
vec.se = c(round(high_ps_18*SE_high, digits = 2), round(med_ps_18*SE_med, digits = 2), 
           round(SE_low*low_ps_18, digits = 2), round(SE_hs*hs_ps_18, digits = 2), 
           round(pf_ps_18*SE_pf, digits = 2))
## change name of zone before running!
df = cbind(df, Pastaza_Maranon = vec, SE = vec.se)


########## repeat for napo_amazon_putu ################

north = readOGR('Z:/Peru_Palm_Swamp/Shapefiles','napo_amazon_putu')
north = spTransform(north, crs(im18))

im18_ps = mask(im18, north)
f_im18_ps = freq(im18_ps, useNA='no')

# apply ratios to mapped classes within napo_amazon_putu
high_ps_18 = round((f_im18_ps[1,2]*0.09)*(ratio_high), digits = 2)
med_ps_18 = round((f_im18_ps[2,2]*0.09)*(ratio_med), digits = 2)
low_ps_18 = round((f_im18_ps[3,2]*0.09)*(ratio_low), digits = 2)
hs_ps_18 = round((f_im18_ps[4,2]*0.09)*(ratio_hs), digits = 2)
pf_ps_18 = round((f_im18_ps[5,2]*0.09)*(ratio_pf), digits = 2)

vec = c(high_ps_18, med_ps_18, low_ps_18, hs_ps_18, pf_ps_18)
vec.se = c(round(high_ps_18*SE_high, digits = 2), round(med_ps_18*SE_med, digits = 2), 
           round(SE_low*low_ps_18, digits = 2), round(SE_hs*hs_ps_18, digits = 2), 
           round(pf_ps_18*SE_pf, digits = 2))
## change name of zone before running!
df = cbind(df, Napo_Amazon_Putumayo = vec, SE = vec.se)


####### repeat with ucayali ############

uca = readOGR('Z:/Peru_Palm_Swamp/Shapefiles','South_studyArea')
uca = spTransform(uca, crs(im18))

im18_ps = mask(im18, uca)
f_im18_ps = freq(im18_ps,useNA='no')

# apply ratios to mapped classes within ucayali
high_ps_18 = round((f_im18_ps[1,2]*0.09)*(ratio_high), digits = 2)
med_ps_18 = round((f_im18_ps[2,2]*0.09)*(ratio_med), digits = 2)
low_ps_18 = round((f_im18_ps[3,2]*0.09)*(ratio_low), digits = 2)
hs_ps_18 = round((f_im18_ps[4,2]*0.09)*(ratio_hs), digits = 2)
pf_ps_18 = round((f_im18_ps[5,2]*0.09)*(ratio_pf), digits = 2)

vec = c(high_ps_18, med_ps_18, low_ps_18, hs_ps_18, pf_ps_18)
vec.se = c(round(high_ps_18*SE_high, digits = 2), round(med_ps_18*SE_med, digits = 2), 
           round(SE_low*low_ps_18, digits = 2), round(SE_hs*hs_ps_18, digits = 2), 
           round(pf_ps_18*SE_pf, digits = 2))
## change name of zone before running!
df = cbind(df, Ucayali = vec, SE = vec.se)



####### repeat with ucayali southeast ############

uca = readOGR('Z:/Peru_Palm_Swamp/Shapefiles','ucayali_southeast')
uca = spTransform(uca, crs(im18))

im18_ps = mask(im18, uca)
f_im18_ps = freq(im18_ps,useNA='no')

# apply ratios to mapped classes within ucayali southeast
high_ps_18 = round((f_im18_ps[1,2]*0.09)*(ratio_high), digits = 2)
med_ps_18 = round((f_im18_ps[2,2]*0.09)*(ratio_med), digits = 2)
low_ps_18 = round((f_im18_ps[3,2]*0.09)*(ratio_low), digits = 2)
hs_ps_18 = round((f_im18_ps[4,2]*0.09)*(ratio_hs), digits = 2)
pf_ps_18 = round((f_im18_ps[5,2]*0.09)*(ratio_pf), digits = 2)

vec = c(high_ps_18, med_ps_18, low_ps_18, hs_ps_18, pf_ps_18)
vec.se = c(round(high_ps_18*SE_high, digits = 2), round(med_ps_18*SE_med, digits = 2), 
           round(SE_low*low_ps_18, digits = 2), round(SE_hs*hs_ps_18, digits = 2), 
           round(pf_ps_18*SE_pf, digits = 2))
## change name of zone before running!
df = cbind(df, Ucayali_southeast = vec, SE = vec.se)


###### repeat with maranon southwest

marsw = readOGR('Z:/Peru_Palm_Swamp/Shapefiles','maranon_southwest')
marsw = spTransform(marsw, crs(im18))

im18_ps = mask(im18, marsw)
f_im18_ps = freq(im18_ps,useNA='no')

# apply ratios to mapped classes within maranon southwest
high_ps_18 = round((f_im18_ps[1,2]*0.09)*(ratio_high), digits = 2)
med_ps_18 = round((f_im18_ps[2,2]*0.09)*(ratio_med), digits = 2)
low_ps_18 = round((f_im18_ps[3,2]*0.09)*(ratio_low), digits = 2)
hs_ps_18 = round((f_im18_ps[4,2]*0.09)*(ratio_hs), digits = 2)
pf_ps_18 = round((f_im18_ps[5,2]*0.09)*(ratio_pf), digits = 2)

vec = c(high_ps_18, med_ps_18, low_ps_18, hs_ps_18, pf_ps_18)
vec.se = c(round(high_ps_18*SE_high, digits = 2), round(med_ps_18*SE_med, digits = 2), 
           round(SE_low*low_ps_18, digits = 2), round(SE_hs*hs_ps_18, digits = 2), 
           round(pf_ps_18*SE_pf, digits = 2))
## change name of zone before running!
df = cbind(df, Maranon_southwest = vec, SE = vec.se)

write.csv(df, file = 'Z:/Peru_Palm_Swamp/Figures/peatFormingClassesAreas5zones_2.csv')


##### estimate of SFF over peat area. class = 8
est = 6139565
seratio = 67634.5/est
im18_full <- raster('Z:/Peru_Palm_Swamp/ARD/ARD_2018_Data/Full_Classification_18_RPJ.tif')
f = freq(im18_full, useNA='no')
sffmapped = f[8,2]*0.09
sff_ratio = est/sffmapped

im18peat = im18_full*peat
f2 = freq(im18peat, useNA='no')
sffmappedpeat = f2[8,2]*0.09

sffpeat = 849051
sff_peat_est = sffpeat*sff_ratio
sff_se = seratio*sff_peat_est

#Repeat for TF
est = 22399831
seratio = 103944/est
tfmapped = f[9,2]*0.09
tf_ratio = est/tfmapped
tfmappedpeat = f2[9,2]*0.09

tf_peat_est = tfmappedpeat*tf_ratio
tf_se = seratio*tf_peat_est
