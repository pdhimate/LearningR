# read the hotel ratings data set into a matrix
# the first column is critics ratimg and the remaning 9 columns are user ratings
# we find user who can fill in the place of critic, ie who has similar rating like critic
kei.data <- as.matrix(read.table("Datasets/KeiHotels.txt"))
kei.data


############### functions ###################################
minkowski <- function(x, y, p = 1)
  (sum(abs(x - y) ^ p)) ^ (1 / p)
############### functions end ##############################

# calcutae euclidian distance between critic and user 2
minkowski(kei.data[, 1], kei.data[, 2], 2)

# Calculate manhattan distance between critic and user 3
minkowski(kei.data[, 1], kei.data[, 3]) # p =1

# correlation (pearson)
cor(kei.data[, 1], kei.data[, 3])

# correlation spearman
cor(kei.data[, 1], kei.data[, 3], method = "spearman")


# Calclate all teh above for all the columns
# note: lower distances means highly similar
#       higher correlation means highly similar
ed <- array(0, 9)
md <- array(0, 9)
cp <- array(0, 9)
cs <- array(0, 9)
for (i in 1:9) {
  ed[i] <-
    minkowski(kei.data[, 1], kei.data[, i + 1], 2) # 1. euclidean dist
  md[i] <-
    minkowski(kei.data[, 1], kei.data[, i + 1], 1) # 2. manhattan dist
  cp[i] <- cor(kei.data[, 1], kei.data[, i + 1]) # 3. Corr, pearson
  cs[i] <-
    cor(kei.data[, 1], kei.data[, i + 1], method = "spearman") # 4. corr, spearman
}
ed
md
cp
cs

# plot histograms for all users and critic
# to observe similarities with critic's histogram to know which user rates similar to critic
hist(kei.data[, 1])
for (i in 2:9) {
  hist(kei.data[, i])
}

# plot scatter plot for all users and critic
# to observe whether any chart forms a straight line indicating similarity
for (i in 2:9) {
  plot(x = kei.data[, 1], y = kei.data[, i])
}

# roughly user 5 is the most similar to critc from the above plots


# import lib, before doing that run the target script
source("AggWaFit718.R")

## Find Weighted A Mean between critics and remainng users
fit.QAM(kei.data[, c(2:10, 1)], output.1 = "Datasets/Outputs/output1.txt", stats.1 = "Datasets/Outputs/stats1.txt") # 2:10 reaming users With respect to 1 which is critic
# The stats txt above indicates that user 5 is the most similar to critic


## Find weighted power means for the below
fit.QAM(kei.data[,c(2:10,1)],g=PM05,g.inv=invPM05,output.1 = "Datasets/Outputs/output2.txt", stats.1 = "Datasets/Outputs/stats2.txt")
fit.QAM(kei.data[,c(2:10,1)],g=QM,g.inv=invQM,output.1 = "Datasets/Outputs/output2.txt", stats.1 = "Datasets/Outputs/stats2.txt")
fit.QAM(kei.data[,c(2:10,1)],g=PM2,g.inv=invPM2,output.1 = "Datasets/Outputs/output2.txt", stats.1 = "Datasets/Outputs/stats2.txt")
fit.QAM(kei.data[,c(2:10,1)],g=GMa,g.inv=invGMa,output.1 = "Datasets/Outputs/output2.txt", stats.1 = "Datasets/Outputs/stats2.txt")
fit.QAM(kei.data[,c(2:10,1)],g=OWA,output.1 = "Datasets/Outputs/output2.txt", stats.1 = "Datasets/Outputs/stats2.txt")

# if we observe the statts for all pf the above OWA has lowest RMSE 
# and user 6, 5, 10, 9 are most similar

## CAlculate weights for the above users
fit.QAM(kei.data[,c(6,5,10,9,1)],g=OWA,output.1 = "Datasets/Outputs/output2.txt", stats.1 = "Datasets/Outputs/stats2.txt")
fit.choquet(kei.data[,c(6,5,10,9,1)])


