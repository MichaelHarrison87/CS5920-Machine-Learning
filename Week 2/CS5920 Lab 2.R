library(class)

# setwd("Y:/01 MSc Machine Learning/01 Term 1/CS5920 Machine Learning/Week 2")

iris <- read.csv("iris.txt", header=FALSE)
ionosphere <- read.csv("ionosphere.txt", sep=",", header=FALSE)
USPS <- read.table("USPSsubset.txt", sep=" ", header=FALSE)

names(iris) <- c('sepal.Length','sepal.Width', 'petal.Length','petal.Width', 'Type')

train.X <- iris[c(1:70),c(1:4)] 
test.X <- iris[c(71:100),c(1:4)] 

train.Y <- iris[c(1:70),5] 
test.Y <- iris[c(71:100),5] 

set.seed(0)

predicted.Y <- knn(train.X, test.X, train.Y, k=1)

minimum <- min(USPS[1,1:256])
maximum <- max(USPS[1,1:256])
first.image <- (USPS[1,1:256]-minimum)/(maximum-minimum)