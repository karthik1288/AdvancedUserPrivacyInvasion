rm(list=ls(all=TRUE))

#Load/install libraries
require("ggplot2")
require("gbm")
require("Metrics")

#Set Working Directory
workingDirectory <- 'C:\\Users\\Karthik\\Desktop\\infosec'
setwd(workingDirectory)

dataDirectory <- 'C:\\Users\\Karthik\\Desktop\\infosec\\data'

#Load functions
source(paste0(workingDirectory, 'Crval.R'))
source(paste0(workingDirectory, 'eBT.R'))

#############################
#Load Data
#Input Data
train <- read.csv(paste0('data3.csv'), header = TRUE, stringsAsFactors = FALSE, na.strings=c("", "NA", "NULL"))

#submissionTemplate <- read.csv(paste0(dataDirectory, 'sampleSubmission.csv'), header = TRUE, stringsAsFactors = FALSE, na.strings=c("", "NA", "NULL"))

train$date <- as.character(train$date)
train$time <- as.factor(train$time)
train$realp <- as.factor(train$realp)
train$reactivep <- as.factor(train$reactivep)
train$voltage <- as.factor(train$voltage)
train$gt <- as.factor(train$gt)
train$sm1 <- as.factor(train$sm1)
train$sm2 <- as.factor(train$sm2)
train$sm3 <- as.factor(train$sm3)
train$realpkwh <- as.factor(train$realpkwh)
train$reactivepkwh <- as.factor(train$reactivepkwh)
train$sm1kwh <- as.factor(train$sm1kwh)
train$sm2kwh <- as.factor(train$sm2kwh)
train$sm3kwh <- as.factor(train$sm3kwh)


library(tm)

mydata1 = as.matrix(read.table("app_table.txt", header = FALSE,fill = TRUE))
print (mydata1)
mydata1<-rbind(mydata1, c(1,1,1,0,0,0,1,1,1))
print (length(mydata1[,1]))


train$A <- as.factor(mydata1[,1])
train$B <- as.factor(mydata1[,2])
train$C <- as.factor(mydata1[,3])
train$D <- as.factor(mydata1[,4])
train$E <- as.factor(mydata1[,5])
train$F <- as.factor(mydata1[,6])
train$G <- as.factor(mydata1[,7])
train$H <- as.factor(mydata1[,8])
train$I <- as.factor(mydata1[,9])

#####################################
#EDA
#this needs to be automated as much as possible
set.seed(101)
sampleIndices <- sort(sample(1:nrow(train), 1836)) # these indices are good for the train features and features plots

#Product options density plots
A.spread <- ggplot(train[sampleIndices, ], aes(x = A)) + geom_density()
print(A.spread, height = 6, width = 8)
B.spread <- ggplot(train[sampleIndices, ], aes(x = B)) + geom_density()
print(B.spread, height = 6, width = 8)
C.spread <- ggplot(train[sampleIndices, ], aes(x = C)) + geom_density()
print(C.spread, height = 6, width = 8)
D.spread <- ggplot(train[sampleIndices, ], aes(x = D)) + geom_density()
print(D.spread, height = 6, width = 8)
E.spread <- ggplot(train[sampleIndices, ], aes(x = E)) + geom_density()
print(E.spread, height = 6, width = 8)
Ff.spread <- ggplot(train[sampleIndices, ], aes(x = F)) + geom_density()
print(Ff.spread, height = 6, width = 8)
G.spread <- ggplot(train[sampleIndices, ], aes(x = G)) + geom_density()
print(G.spread, height = 6, width = 8)
H.spread <- ggplot(train[sampleIndices, ], aes(x = H)) + geom_density()
print(H.spread, height = 6, width = 8)
I.spread <- ggplot(train[sampleIndices, ], aes(x = I)) + geom_density()
print(I.spread, height = 6, width = 8)


nonSmartRandSamples <- sapply(unique(train$realp), anonFun <- function(ID){
  randZeroIndex <- sample(train$realp,which(train$realp == ID), 1)
  return(randZeroIndex)
}
)
#save this!, the process takes about 2.5 hours
save(nonSmartRandSamples, file = 'randSamples.RData')

#last offer indices
lastOfferIndices <- sapply(unique(train$realp), function(ID){
  return(max(which(train$realp == ID)) - 1)
})
#Save this, it takes more or less an hour to compute
save(lastOfferIndices, file = 'lastOfferIndices.RData')

#Various indices to compare
SmartIndices <- train$realp > 0.05
pre_SmartIndices <- 1:nrow(train) %in% (which(train$realp == 1) - 1)
Smt.preSmt.Indices <- SmartIndices | pre_SmartIndices
SmartVector <- train$realp[SmartIndices | pre_SmartIndices]
randPlusSmart <- SmartIndices | 1:nrow(train) %in% nonSmartRandSamples

#create a "y" matrix and merge it with the train matrix#time vs. Appliance "A"
ggplot(train, aes(x = A, y = time, fill = realp)) +  geom_point() + facet_grid(realp ~ .)
ggplot(train[Smt.preSmt.Indices, ], aes(x = A, y = time, fill = realp)) +  geom_point() + facet_grid(realp ~ .)
ggplot(train[randPlusSmart, ], aes(x = A, y = time, fill = realp)) +  geom_point() + facet_grid(realp ~ .)

yMatrix <- train[SmartIndices, c('realp', 'A', 'B', 'C', 'D', 'E', 'F', 'G')]
names(yMatrix) <- c('realp', 'Ay', 'By', 'Cy', 'Dy', 'Ey', 'Fy', 'Gy')
train <- merge(train, yMatrix, all = TRUE)
rm(yMatrix)


#time vs. Appliance "A"
ggplot(train, aes(x = A, y = time, fill = realp)) +  geom_point() + facet_grid(realp ~ .)
ggplot(train[Smt.preSmt.Indices, ], aes(x = A, y = time, fill = realp)) +  geom_point() + facet_grid(realp ~ .)
ggplot(train[randPlusSmart, ], aes(x = A, y = time, fill = realp)) +  geom_point() + facet_grid(realp ~ .)

#time vs. Appliance "B"
ggplot(train, aes(x = B, y = time, fill = realp)) +  geom_point() + facet_grid(realp ~ .)
ggplot(train[Smt.preSmt.Indices, ], aes(x = B, y = time, fill = realp)) +  geom_point() + facet_grid(realp ~ .)
ggplot(train[randPlusSmart, ], aes(x = B, y = time, fill = realp)) +  geom_point() + facet_grid(realp ~ .)

#time vs. Appliance "C"
ggplot(train, aes(x = C, y = time, fill = realp)) +  geom_point() + facet_grid(realp ~ .)
ggplot(train[Smt.preSmt.Indices, ], aes(x = C, y = time, fill = realp)) +  geom_point() + facet_grid(realp ~ .)
ggplot(train[randPlusSmart, ], aes(x = C, y = time, fill = realp)) +  geom_point() + facet_grid(realp ~ .)


#time vs. Appliance "D"
ggplot(train, aes(x = D, y = time, fill = realp)) +  geom_point() + facet_grid(realp ~ .)
ggplot(train[Smt.preSmt.Indices, ], aes(x = D, y = time, fill = realp)) +  geom_point() + facet_grid(realp ~ .)
ggplot(train[randPlusSmart, ], aes(x = D, y = time, fill = realp)) +  geom_point() + facet_grid(realp ~ .)

#time vs. Appliance "E"
ggplot(train, aes(x = E, y = time, fill = realp)) +  geom_point() + facet_grid(realp ~ .)
ggplot(train[Smt.preSmt.Indices, ], aes(x = E, y = time, fill = realp)) +  geom_point() + facet_grid(realp ~ .)
ggplot(train[randPlusSmart, ], aes(x = E, y = time, fill = realp)) +  geom_point() + facet_grid(realp ~ .)

#time vs. Appliance "F"
ggplot(train, aes(x = F, y = time, fill = realp)) +  geom_point() + facet_grid(realp ~ .)
ggplot(train[Smt.preSmt.Indices, ], aes(x = F, y = time, fill = realp)) +  geom_point() + facet_grid(realp ~ .)
ggplot(train[randPlusSmart, ], aes(x = F, y = time, fill = realp)) +  geom_point() + facet_grid(realp ~ .)

#time vs. Appliance "G"
ggplot(train, aes(x = G, y = time, fill = realp)) +  geom_point() + facet_grid(realp ~ .)
ggplot(train[Smt.preSmt.Indices, ], aes(x = G, y = time, fill = realp)) +  geom_point() + facet_grid(realp ~ .)
ggplot(train[randPlusSmart, ], aes(x = G, y = time, fill = realp)) +  geom_point() + facet_grid(realp ~ .)

#time vs. Appliance "H"
ggplot(train, aes(x = H, y = time, fill = realp)) +  geom_point() + facet_grid(realp ~ .)
ggplot(train[Smt.preSmt.Indices, ], aes(x = H, y = time, fill = realp)) +  geom_point() + facet_grid(realp ~ .)
ggplot(train[randPlusSmart, ], aes(x = H, y = time, fill = realp)) +  geom_point() + facet_grid(realp ~ .)

#time vs. Appliance "I"
ggplot(train, aes(x = I, y = time, fill = realp)) +  geom_point() + facet_grid(realp ~ .)
ggplot(train[Smt.preSmt.Indices, ], aes(x = I, y = time, fill = realp)) +  geom_point() + facet_grid(realp ~ .)
ggplot(train[randPlusSmart, ], aes(x = I, y = time, fill = realp)) +  geom_point() + facet_grid(realp ~ .)

#back to factor for IDA/modeling
train$realp <- as.factor(train$realp)
print (train)
Ay <- c(Ay,na.rm=TRUE)
train[complete.cases(train),]
print (is.na (train))


####################################
#IDA (initial data analysis)
#Correlations between appliance consumption and time
#scatterplots of realp vs Appliance A
pairs(Ay ~ time + realp + B + C + D + E + F + G + H + I,train) 

pairs(B ~ time + realp + A + C + D + E + F + G + H + I, train) 

pairs(C ~ time + realp + A + B + D + E + F + G + H + I, train) 

pairs(D ~ time + realp + A + B + C + E + F + G + H + I, train) 

pairs(E ~ time + realp + A + B + C + D + F + G + H + I, train) 

pairs(F ~ time + realp + A + B + C + D + E + G + H + I, train) 


train2 <- c(train,na.rm=TRUE,options(scipen=999))
print (train2)
centroidsTrain <- sapply(unique(train2$realp), function(ID){
  oneCentroidKMeans <- kmeans(train[train2$realp == ID, seq(18, 25)],
                              centers = 1, algorithm = 'Hartigan-Wong')
  return(c(ID, oneCentroidKMeans$centers))
})
