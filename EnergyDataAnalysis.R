#####################################################################
# 1. Understanding data
#####################################################################
library(lubridate)
library(PerformanceAnalytics) 
library(Metrics)

# functions #########################

data_types <- function(frame) {
  res <- lapply(frame, class)
  res_frame <- data.frame(unlist(res))
  barplot(table(res_frame), main="Data Types", col="steelblue", ylab="Number of Features")
}

scaleFunc <- function(x) {
  result <- x
  result <- (result - min(result)) / (max(result) - min(result)) # scaling to unit interval
  return(result)
}

polyFunc <- function(x, p) {
  result <- x
  result <- result ^ p # polynomial transformation
  return(result)
}

# inverse scale function to see original values on the visualization
invScaleFunc <- function(x, minX, maxX) {
  result <- x
  result <- x * (maxX - minX) + minX # inverse min max scaling
  return(result)
}

# functions #########################

# read all data
my.data <- as.data.frame(read.csv("Datasets/Combined-Weather-Energy-201905.csv"))
# Columns=> Temp	Humidity	Barometer	DATETIME	TOTALDEMAND
# we name them as X1, X2, X3, X4 and Y

# convert DATETIME to unix epoch for it to become numeric
my.data[,4] <- as.numeric(as.POSIXct(my.data[,4],format="%d-%m-%Y %H:%M"))

# Scatter plots for TOTALDEMAND vs X1, X2, X3, X4
scatter.smooth(x = my.data[, 1],y = my.data[, 5],main = "Energy Demand vs Temp.",xlab = "Temp",ylab = "Energy Demand")
print(cor(my.data[, 1], my.data[, 5])) # calcluate correlation
scatter.smooth(x = my.data[, 2],y = my.data[, 5],main = "Energy Demand vs Humidity.",xlab = "Humidity",ylab = "Energy Demand")
print(cor(my.data[, 2], my.data[, 5])) # calcluate correlation
scatter.smooth(x = my.data[, 3],y = my.data[, 5],main = "Energy Demand vs Pressure",xlab = "Pressure",ylab = "Energy Demand")
print(cor(my.data[, 3], my.data[, 5])) # calcluate correlation
scatter.smooth(x = my.data[, 4],y = my.data[, 5],main = "Energy Demand vs DATETIME",xlab = "DATETIME",ylab = "Energy Demand")
print(cor(my.data[, 4], my.data[, 5])) # calcluate correlation

# Skewness for TOTALDEMAND, Temp	Humidity	Barometer	DATETIME	TOTALDEMAND
for (x in c(1:4)) {
  print(skewness(my.data[, x])) # calculate skewness
}
print(skewness(my.data[, 5])) # calculate skewness for Y

#####################################################################
# 2. Transform the data to unit scale and reduce skew
#####################################################################

# Transformation for Temp
tempTransformed <- scaleFunc(my.data[, 1])
skewness(my.data[, 1])
skewness(tempTransformed)

# Transformation for Humidity
humidityTransformed <- polyFunc(scaleFunc(my.data[, 2]), 1.6)
skewness(my.data[, 2])
skewness(humidityTransformed)

# Transformation for Pressure
pressTransformed <- polyFunc(scaleFunc(my.data[, 3]), 1.8)
skewness(my.data[, 3])
skewness(pressTransformed)

# Transformation for DATETIME
dateTimeTransformed <- scaleFunc(my.data[, 4])
skewness(my.data[, 4])
skewness(dateTimeTransformed)

# Transformation for Energy Demand
energyTransformed <- polyFunc(scaleFunc(my.data[, 5]), 0.82)
skewness(my.data[, 5])
skewness(energyTransformed)

transformed.my.data <-
  data.frame(tempTransformed, humidityTransformed, pressTransformed, dateTimeTransformed, energyTransformed)
colnames (transformed.my.data) <- c("Temp", "Humidity", "Pressure", "DateTime", "Energy")

transformed.my.data


#####################################################################
# 3. Build models and investigate the importance of each variable
#####################################################################
source("AggWaFit718.R")
PM5 <- function(x) {x^5}
invPM5 <-function(x) {x^(1/5)}

# fitting functions
fit.QAM(transformed.my.data[,c(1:4,5)],"WAMoutput.txt", "WAMstats.txt")
fit.QAM(transformed.my.data[,c(1:4,5)],"WPM05output.txt", "WPM05stats.txt", g=PM05, g.inv=invPM05)
fit.QAM(transformed.my.data[,c(1:4,5)],"WPM5output.txt", "WPM5stats.txt", g=PM5, g.inv=invPM5)
fit.QAM(transformed.my.data[,c(1:4,5)],"WPM2output.txt", "WPM2stats.txt", g=PM2, g.inv=invPM2)
fit.OWA(transformed.my.data[,c(1:4,5)],"OWAoutput.txt", "OWAstats.txt")
fit.choquet(transformed.my.data[,c(1:4,5)],"CQtoutput.txt", "CQstats.txt")

# Out of the stats for above, Chouqet Intergral has best performance : Least RMSE and Highest correlation
# Figures for Choquet Integral below
# RMSE 0.184653417385063
# Av. abs error 0.149778835021915
# Pearson Correlation 0.307080722879266
# Spearman Correlation 0.282786218596052
# Orness 0.461614594258179
# i Shapley i
# 1 0.328564476204844
# 2 0.184012455853421
# 3 0.163609206476802
# 4 0.323813861464933
# binary number fm.weights
# 1 0.464052582431926
# 2 0.154825390288042
# 3 0.553092056286385
# 4 0.26291779123594
# 5 0.511775529056708
# 6 0.26291779123594
# 7 0.553092056286385
# 8 0.511775529056708
# 9 0.511775529056708
# 10 0.511775529056708
# 11 0.660419414519626
# 12 0.511775529056708
# 13 0.511775529056708
# 14 0.511775529056708
# 15 0.999999999999999

#####################################################################
# 5. Predicting
#####################################################################

# fuzzy weights from CQstats.txt
fuzzyWeights <- c(0.464052582431926,0.154825390288042,0.553092056286385,0.26291779123594,0.511775529056708,
                  0.26291779123594,0.553092056286385,0.511775529056708,0.511775529056708,0.511775529056708,
                  0.660419414519626,0.511775529056708,0.511775529056708,0.511775529056708,0.999999999999999)

# Get summary of LM
limfunc <- lm(transformed.my.data[,5]~transformed.my.data[,1]+transformed.my.data[,2]+transformed.my.data[,3]+transformed.my.data[,4])
limfunc
summary(limfunc)

# Liner model for predicting all rows
lrmPrediction <- array(0,nrow(transformed.my.data))
for (i in 1:nrow(transformed.my.data)) {
  lrmPrediction[i] <- 0.46658+((-0.12515)*tempTransformed[i])+((-0.12592)*humidityTransformed[i])+(0.05720*pressTransformed[i])+(0.15972*dateTimeTransformed[i])
}
rmse(lrmPrediction, energyTransformed) # 0.1809947
mae(lrmPrediction, energyTransformed) # 0.1507193

# visualize for LRM to actual scale of Y
energyOriginal <- my.data[,5]
energylrmInv <- invScaleFunc(lrmPrediction, min(energyOriginal), max(energyOriginal))
energylrmInv
scatter.smooth(y= energylrmInv,x = energyOriginal, ylab = "LRM predicted Energy", xlab = "Actual Energy", main = "Linear Regression VS Actual values")

# Choquet model for predicting all rows
cqPrediction = array(0,nrow(transformed.my.data))
for (i in 1:nrow(transformed.my.data)) {
  r <- c(tempTransformed[i],humidityTransformed[i],pressTransformed[i],dateTimeTransformed[i])
  cqPrediction[i] <- choquet(r,fuzzyWeights)
}
rmse(cqPrediction, energyTransformed) # 0.1846534
mae(cqPrediction, energyTransformed) # 0.1497788

# visualize for Choquet model to actual scale of Y
energycqInv <- invScaleFunc(cqPrediction, min(energyOriginal), max(energyOriginal))
energycqInv
scatter.smooth(y= energycqInv,x = energyOriginal, ylab = "Choquet  predicted Energy", xlab = "Actual Energy", main = "Choquet model VS Actual values")

# Visualize between Linear and Choquet model for comparison/differences
scatter.smooth(y= energycqInv,x = energylrmInv, ylab = "Choquet  predicted Y", xlab = "Linear model predicted Y", main = "Choquet model VS Linear model")

# cor between them
cor(cqPrediction,lrmPrediction) # 0.651992



#####################################################################
# 6. Conclusion
#####################################################################


# Our best model Choquet model has an RMSE and Avg. Abs. Error of 0.1846534 and 0.1497788 resp. which are high
# The plots of actual vs predicted values indicate that this is not a suitable model for prediction.
# Similarly, Linear model proved to be even more ineffective.

