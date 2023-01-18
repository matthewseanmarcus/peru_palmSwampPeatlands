library(raster)
library(rgdal)

pacsam = readOGR('Z:/Peru_Palm_Swamp/Shapefiles/Pacaya_Samaria', 'Pacaya_Samaria')

# get total mapped area of degradation within whole study area for both
# periods, then get ratio of that olofsson area / total area
# Then get degradation area within PS, apply ratio

deg18 = raster('Z:/Peru_Palm_Swamp/ARD/DegradationMaps/Figure7_3.tif')
pacsam = spTransform(pacsam, crs(deg18))
f18_total = freq(deg18)
deg18_ps = mask(deg18, pacsam)
f_deg18_ps = freq(deg18_ps)

deg07 = raster('Z:/Peru_Palm_Swamp/ARD/DegradationMaps/Figure7_07_3.tif')
f07_total = freq(deg07)
deg07_ps = mask(deg07, pacsam)
f_deg07_ps = freq(deg07_ps)

# area of mapped total of deg , and ratio to estimate, 18
total_deg18 = f18_total[2,2]*0.0009
total_defor18 = f18_total[3,2]*0.0009
total_deg18_est = 1941.82
ratio_deg18 = total_deg18_est/total_deg18
SEdeg18 = 76/1941.82
ratio_defor18 = 461.66/total_defor18
SEdef18 = 14.43/461.66

# area of mapped total of deg , and ratio to estimate, 07
total_deg07 = f07_total[2,2]*0.0009
total_defor07 = f07_total[3,2]*0.0009
total_deg07_est = 2622.13
ratio_deg07 = total_deg07_est/total_deg07
SEdeg07 = 109.64/2622.13
ratio_defor07 = 328.69/total_defor07
SEdef07 = 8.51/328.69

# apply ratios to mapped deg/defor within pacsam
deg_ps_18 = round((f_deg18_ps[2,2]*0.0009)*(ratio_deg18), digits = 2)
defor_ps_18 = round((f_deg18_ps[3,2]*0.0009)*(ratio_defor18), digits = 2)

deg_ps_07 = round((f_deg07_ps[2,2]*0.0009)*(ratio_deg07), digits = 2)
defor_ps_07 = round((f_deg07_ps[3,2]*0.0009)*(ratio_defor07), digits = 2)


rows = c('Degradation 1990-2007', 'Deforestation 2007-1990', 'Degradation 2007-2018', 'Deforestation 2007-2018')
df = data.frame(row.names = rows)
vec = c(deg_ps_07, defor_ps_07, deg_ps_18, defor_ps_18)

vec.se = c(round(deg_ps_07*SEdeg07, digits = 2), round(defor_ps_07*SEdef07, digits = 2), 
           round(deg_ps_18*SEdeg18, digits = 2), round(defor_ps_18*SEdef18, digits = 2))

## change name of zone before running!
df = cbind(df, Pacaya_Samiria = vec, SE = vec.se)

# rate of deg within PS
degRatePS18 = deg_ps_18/(2018-2007) # 1688.2 ha/yr = 16.88 sq km / yr
degRatePS07 = deg_ps_07/(2007-1990) # 2680.57 ha/yr = 26.8 sq km / yr
((degRatePS18-degRatePS07)/degRatePS07)*100 # = -37.02 %

deforRatePS18 = defor_ps_18/(2018-2007) # 129.3 ha.yr = 1.3 sq km / yr
deforRatePS07 = defor_ps_07/(2007-1990) # 84.7 ha/yr = 0.85 sq km / yr
((deforRatePS18-deforRatePS07)/deforRatePS07)*100 # = 52.7%

############## repeat above with pmfb zone ##################

pm = readOGR('Z:/Peru_Palm_Swamp/Shapefiles', 'PMFB_zone')
pm = spTransform(pm, crs(deg18))
deg18_ps = mask(deg18, pm)
f_deg18_ps = freq(deg18_ps)
deg07_ps = mask(deg07, pm)
f_deg07_ps = freq(deg07_ps)

# apply ratios to mapped deg/defor within pmfb
deg_ps_18 = round((f_deg18_ps[2,2]*0.0009)*(ratio_deg18), digits = 2)
defor_ps_18 = round((f_deg18_ps[3,2]*0.0009)*(ratio_defor18), digits = 2)

deg_ps_07 = round((f_deg07_ps[2,2]*0.0009)*(ratio_deg07), digits = 2)
defor_ps_07 = round((f_deg07_ps[3,2]*0.0009)*(ratio_defor07), digits = 2)


rows = c('Degradation 1990-2007', 'Deforestation 2007-1990', 'Degradation 2007-2018', 'Deforestation 2007-2018')
vec = c(deg_ps_07, defor_ps_07, deg_ps_18, defor_ps_18)

vec.se = c(round(deg_ps_07*SEdeg07, digits = 2), round(defor_ps_07*SEdef07, digits = 2), 
           round(deg_ps_18*SEdeg18, digits = 2), round(defor_ps_18*SEdef18, digits = 2))

## change name of zone before running!
df = cbind(df, Pastaza_Maranon = vec, SE = vec.se)

# # rate of deg within pmfb
degRatePS18 = deg_ps_18/(2018-2007) # 11166.54 ha/yr = 111.67 sq km / yr
degRatePS07 = deg_ps_07/(2007-1990) # 8463.52 ha/yr = 84.64 sq km / yr
((degRatePS18-degRatePS07)/degRatePS07)*100 # = 31.94%

deforRatePS18 = defor_ps_18/(2018-2007) # 3030.182 ha.yr = 30.30 sq km / yr
deforRatePS07 = defor_ps_07/(2007-1990) # 1260.176  ha/yr = 12.6 sq km / yr
((deforRatePS18-deforRatePS07)/deforRatePS07)*100 # = 140.5%


########## repeat for napo_amazon_putu ################

north = readOGR('Z:/Peru_Palm_Swamp/Shapefiles','napo_amazon_putu')
north = spTransform(north, crs(deg18))

deg18_ps = mask(deg18, north)
f_deg18_ps = freq(deg18_ps)
deg07_ps = mask(deg07, north)
f_deg07_ps = freq(deg07_ps)

# apply ratios to mapped deg/defor within nap
deg_ps_18 = round((f_deg18_ps[2,2]*0.0009)*(ratio_deg18), digits = 2)
defor_ps_18 = round((f_deg18_ps[3,2]*0.0009)*(ratio_defor18), digits = 2)

deg_ps_07 = round((f_deg07_ps[2,2]*0.0009)*(ratio_deg07), digits = 2)
defor_ps_07 = round((f_deg07_ps[3,2]*0.0009)*(ratio_defor07), digits = 2)


rows = c('Degradation 1990-2007', 'Deforestation 2007-1990', 'Degradation 2007-2018', 'Deforestation 2007-2018')
vec = c(deg_ps_07, defor_ps_07, deg_ps_18, defor_ps_18)

vec.se = c(round(deg_ps_07*SEdeg07, digits = 2), round(defor_ps_07*SEdef07, digits = 2), 
           round(deg_ps_18*SEdeg18, digits = 2), round(defor_ps_18*SEdef18, digits = 2))

## change name of zone before running!
df = cbind(df, Napo_Amazon_Putumayo = vec, SE = vec.se)

# # rate of deg within napo_amazon_putu
degRatePS18 = deg_ps_18/(2018-2007) # 2301.636ha/yr = 23.02 sq km / yr
degRatePS07 = deg_ps_07/(2007-1990) # 2814.35 ha/yr = 28.14 sq km / yr
((degRatePS18-degRatePS07)/degRatePS07)*100 # = -18.22%

deforRatePS18 = defor_ps_18/(2018-2007) # 261.909 ha.yr = 2.62 sq km / yr
deforRatePS07 = defor_ps_07/(2007-1990) # 116.4 ha/yr = 1.16 sq km / yr
((deforRatePS18-deforRatePS07)/deforRatePS07)*100 # = 124.93%



####### repeat with ucayali ############

uca = readOGR('Z:/Peru_Palm_Swamp/Shapefiles','South_studyArea')
uca = spTransform(uca, crs(deg18))

deg18_ps = mask(deg18, uca)
f_deg18_ps = freq(deg18_ps)
deg07_ps = mask(deg07, uca)
f_deg07_ps = freq(deg07_ps)

# apply ratios to mapped deg/defor within ucayali
deg_ps_18 = round((f_deg18_ps[2,2]*0.0009)*(ratio_deg18), digits = 2)
defor_ps_18 = round((f_deg18_ps[3,2]*0.0009)*(ratio_defor18), digits = 2)

deg_ps_07 = round((f_deg07_ps[2,2]*0.0009)*(ratio_deg07), digits = 2)
defor_ps_07 = round((f_deg07_ps[3,2]*0.0009)*(ratio_defor07), digits = 2)


rows = c('Degradation 1990-2007', 'Deforestation 2007-1990', 'Degradation 2007-2018', 'Deforestation 2007-2018')

vec = c(deg_ps_07, defor_ps_07, deg_ps_18, defor_ps_18)

vec.se = c(round(deg_ps_07*SEdeg07, digits = 2), round(defor_ps_07*SEdef07, digits = 2), 
           round(deg_ps_18*SEdeg18, digits = 2), round(defor_ps_18*SEdef18, digits = 2))

## change name of zone before running!
df = cbind(df, Ucayali = vec, SE = vec.se)

# # rate of deg within ucayali
degRatePS18 = deg_ps_18/(2018-2007) # 157.62 ha/yr = 1.58 sq km / yr
degRatePS07 = deg_ps_07/(2007-1990) # 149.88 ha/yr = 1.50 sq km / yr
((degRatePS18-degRatePS07)/degRatePS07)*100 # = 5.17%

deforRatePS18 = defor_ps_18/(2018-2007) # 69.72 ha.yr = 0.69 sq km / yr
deforRatePS07 = defor_ps_07/(2007-1990) # 129.47 ha/yr = 1.29 sq km / yr
((deforRatePS18-deforRatePS07)/deforRatePS07)*100 # = -46.2%



####### repeat with ucayali southeast ############

uca = readOGR('Z:/Peru_Palm_Swamp/Shapefiles','ucayali_southeast')
uca = spTransform(uca, crs(deg18))

deg18_ps = mask(deg18, uca)
f_deg18_ps = freq(deg18_ps)
deg07_ps = mask(deg07, uca)
f_deg07_ps = freq(deg07_ps)

# apply ratios to mapped deg/defor within ucayali
deg_ps_18 = round((f_deg18_ps[2,2]*0.0009)*(ratio_deg18), digits = 2)
defor_ps_18 = round((f_deg18_ps[3,2]*0.0009)*(ratio_defor18), digits = 2)

deg_ps_07 = round((f_deg07_ps[2,2]*0.0009)*(ratio_deg07), digits = 2)
defor_ps_07 = round((f_deg07_ps[3,2]*0.0009)*(ratio_defor07), digits = 2)


rows = c('Degradation 1990-2007', 'Deforestation 2007-1990', 'Degradation 2007-2018', 'Deforestation 2007-2018')
vec = c(deg_ps_07, defor_ps_07, deg_ps_18, defor_ps_18)

vec.se = c(round(deg_ps_07*SEdeg07, digits = 2), round(defor_ps_07*SEdef07, digits = 2), 
           round(deg_ps_18*SEdeg18, digits = 2), round(defor_ps_18*SEdef18, digits = 2))

## change name of zone before running!
df = cbind(df, Ucayali_southeast = vec, SE = vec.se)

# # rate of deg within ucayali se
degRatePS18 = deg_ps_18/(2018-2007) # 1275.6 ha/yr = 12.8 sq km / yr
degRatePS07 = deg_ps_07/(2007-1990) # 673.7 ha/yr = 6.74 sq km / yr
((degRatePS18-degRatePS07)/degRatePS07)*100 # = 89.3%

deforRatePS18 = defor_ps_18/(2018-2007) # 477.9 ha.yr = 4.78 sq km / yr
deforRatePS07 = defor_ps_07/(2007-1990) # 302.5 ha/yr = 3.03 sq km / yr
((deforRatePS18-deforRatePS07)/deforRatePS07)*100 # = 58%


###### repeat with maranon southwest

marsw = readOGR('Z:/Peru_Palm_Swamp/Shapefiles','maranon_southwest')
marsw = spTransform(marsw, crs(deg18))

deg18_ps = mask(deg18, marsw)
f_deg18_ps = freq(deg18_ps)
deg07_ps = mask(deg07, marsw)
f_deg07_ps = freq(deg07_ps)

# apply ratios to mapped deg/defor within pmfb
deg_ps_18 = round((f_deg18_ps[2,2]*0.0009)*(ratio_deg18), digits = 2)
defor_ps_18 = round((f_deg18_ps[3,2]*0.0009)*(ratio_defor18), digits = 2)

deg_ps_07 = round((f_deg07_ps[2,2]*0.0009)*(ratio_deg07), digits = 2)
defor_ps_07 = round((f_deg07_ps[3,2]*0.0009)*(ratio_defor07), digits = 2)


rows = c('Degradation 1990-2007', 'Deforestation 2007-1990', 'Degradation 2007-2018', 'Deforestation 2007-2018')
vec = c(deg_ps_07, defor_ps_07, deg_ps_18, defor_ps_18)

vec.se = c(round(deg_ps_07*SEdeg07, digits = 2), round(defor_ps_07*SEdef07, digits = 2), 
           round(deg_ps_18*SEdeg18, digits = 2), round(defor_ps_18*SEdef18, digits = 2))

## change name of zone before running!
df = cbind(df, Maranon_southwest = vec, SE = vec.se)

# # rate of deg within maranon southwest
degRatePS18 = deg_ps_18/(2018-2007) # 1062.95 ha/yr = 10.63 sq km / yr
degRatePS07 = deg_ps_07/(2007-1990) # 642.99 ha/yr = 6.43 sq km / yr
((degRatePS18-degRatePS07)/degRatePS07)*100 # = 65.3%

deforRatePS18 = defor_ps_18/(2018-2007) # 227.64 ha.yr = 2.28 sq km / yr
deforRatePS07 = defor_ps_07/(2007-1990) # 39.53 ha/yr = 0.396 sq km / yr
((deforRatePS18-deforRatePS07)/deforRatePS07)*100 # = 475.7%


write.csv(df, file = 'Z:/Peru_Palm_Swamp/Figures/degradationRatiosZones.csv')
