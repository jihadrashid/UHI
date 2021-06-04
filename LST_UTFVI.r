#install.packages("rgdal","raster","caret","RStoolbox","randomForest","e1071")
#Loading required libraries 
library(raster)
library(RStoolbox)

#importing study area boundary and sample data produced using QGIS
boundary = shapefile(".../SAbroad.shp")

#importing image for classification
metaData = readMeta(".../LC08_L1TP_137044_20200211_20200225_01_T1_MTL.txt")
rawImg = stackMeta(metaData)
#Atmospheric correction of satellite data to surface reflectance using Dark Object Subtraction (DOS) method.
rawImg = radCor(rawImg, metaData = metaData, method = "dos")

#replace names of the bands with B1,B2,...B5
names(rawImg) = paste0("B", c(1:7,9:11))
img = mask(crop(rawImg,boundary),boundary)
print(img)

#plotting false color composite (FCC) of mulspectral image of study area
plotRGB(img,r=5,g=4,b=3,stretch="lin")

#NDVI calculation
ndvi = spectralIndices(img, red = "B4", nir = "B5", indices = "NDVI")
plot(ndvi)

#calculating proportion of vegetation
pv = ((ndvi-ndvi@data@min)/(ndvi@data@max-ndvi@data@min))^2
plot(pv)

#Emissivity for TIRS bands
##c is the cavity effect of the surface (c=0 for flat surface).
c=(1-0.966)*(1-pv)*0.55*0.973

##e_mix is emissivity for ndvi values between 0.2 and 0.5 which is considered as mix pixels of vegetation and soil
e_mix=0.973*pv+0.966*(1-pv)

##classes for reclassification of ndvi. -1 to 0 is replaced with 0.991 as considering water, 
##0 to 0.2 is 0.966 considering soil, 0.5 to 1 is 0.973 considering vegetation surface .
classes=c(-1,0,0.991,
          0,0.2,0.966,
          0.2,0.5, NA,
          0.5,1,0.973)
mat=matrix(classes, ncol = 3, byrow = T)
## reclassification of ndvi layer
rec_ndvi=reclassify(ndvi,mat)
emissivity=cover(rec_ndvi,e_mix)

#Emissivity for TM band 06
#emissivity = 0.986+0.004*pv

plot(emissivity)

#Calculating land surface temperature for Landsat 08 Band 10
LST = img$B10/(1 + 10.9 * (img$B10/14388) * log10(emissivity))-273.15

#LST for landsat 05 band 06
#LST = img$B6/(1 + 11.5 * (img$B6/14388) * log10(emissivity))-273.15

plot(LST)

#Calculating urban thermal field variance 
Tm=cellStats(LST, stat='mean', na.rm=TRUE)
UTFVI=(LST-Tm)/Tm
plot(UTFVI)

#Exporting resulted files in the directory
writeRaster(LST, ".../LST2020.tif",overwrite=TRUE)
writeRaster(UTFVI, ".../UTFVI2020.tif",overwrite=TRUE)
