
require("ggplot2")
require("gbm")
require("Metrics")

#Set Working Directory
workingDirectory <- 'C:\\Users\\Karthik\\Desktop\\infosec'
setwd(workingDirectory)

dataDirectory <- 'C:\\Users\\Karthik\\Desktop\\infosec\\data'



train <- read.csv(paste0('data3.csv'), header = TRUE, stringsAsFactors = FALSE, na.strings=c("", "NA", "NULL"))
plot(train$realp, type="o", col="blue", ylim=c(0,8))
lines(train$prediction,  type="o", pch=22, lty=2,col="red")
