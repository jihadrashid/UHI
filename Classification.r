#install.packages("rgdal","raster","caret","RStoolbox","randomForest","e1071")
#Loading required libraries 
library(rgdal)
library(raster)
library(RStoolbox)
library(caret)

#importing study area boundary and sample data produced using QGIS
boundary = shapefile(".../SAbroad.shp")
samplePoly = shapefile(".../Train2020.shp")

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

#extracting sample data from image by labelled sample data
uniqueVal = "id"

sampleData = data.frame(matrix(vector(), nrow = 0, ncol = length(names(img)) + 1))   
for (i in 1:length(unique(samplePoly[[uniqueVal]]))){
  category = unique(samplePoly[[uniqueVal]])[i]
  categorymap = samplePoly[samplePoly[[uniqueVal]] == category,]
  dataSet = extract(img, categorymap)
  if(is(samplePoly, "SpatialPolygonsDataFrame")){
    dataSet = dataSet[!unlist(lapply(dataSet, is.null))]
    dataSet = lapply(dataSet, function(x){cbind(x, class = as.numeric(rep(category, nrow(x))))})
    df = do.call("rbind", dataSet)
    sampleData = rbind(sampleData, df)
  }
}

head(sampleData)
tail(sampleData)

#Subsetting sample data for model fitting
sampleDataMod = sampleData[sample(1:nrow(sampleData), 1000), ]

#Fitting random forest model
modelFit = train(as.factor(class) ~ B2 + B3 + B4 + B5 + B6 + B7 ,
                   method = "rf", 
                   data = sampleDataMod)
modelFit

#Predicting image classes
predictedFile <-   predict(img, modelFit)

#Plotting classifieid layer
clr= c('darkgreen','lightgreen','firebrick2','steelblue','yellow')
plot(predictedFile, col=clr, legend=F, axes=F)

#Exporting resulted prediction file in the directory
writeRaster(preds_rf, ".../Classified.tif",overwrite=TRUE)
