#install.packages("rgdal","raster","caret","RStoolbox","randomForest","e1071")
#Loading required libraries 
library(rgdal)
library(raster)
library(RStoolbox)
library(randomForest)
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
sampleData$class <- as.factor(sampleData$class)
str(sampleData)

#Subsetting sample data for model fitting
set.seed(123)
part <- sample(2, nrow(sampleData), replace = TRUE, prob = c(0.7, 0.3))
train <- sampleData[part==1,]
test <- sampleData[part==2,]

#Fitting random forest model
# Random Forest
set.seed(222)
rf <- randomForest(class~., data=train,
                   ntree = 250,
                   mtry = 2,
                   importance = TRUE,
                   proximity = TRUE)

print(rf)
attributes(rf)
plot(rf)

# # Prediction & Confusion Matrix - test data
p2 <- predict(rf, test)
confusionMatrix(p2, test$class)

# Tune mtry
t <- tuneRF(train[,-1], train[,1],
            stepFactor = 0.2,
            plot = TRUE,
            ntreeTry = 50,
            trace = TRUE,
            improve = 0.05)


#Final prediction
preds_rf <- predict(img, rf, type='class')

#Plotting classifieid layer
clr= c('darkgreen','lightgreen','firebrick2','steelblue','yellow')
plot(predictedFile, col=clr, legend=F, axes=F)

#Exporting resulted prediction file in the directory
writeRaster(preds_rf, ".../Classified.tif",overwrite=TRUE)
