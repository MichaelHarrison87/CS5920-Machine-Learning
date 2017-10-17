USPS <- read.table("USPSsubset.txt", sep=" ", header=FALSE)

numTrain <- ceiling(nrow(USPS)/2)

set.seed(5)
sample.train <- sample(nrow(USPS), numTrain)

USPS.train <- USPS[sample.train, ]
USPS.test <- USPS[-sample.train, ]

USPS.train.X <- USPS.train[ ,1:256]
USPS.train.Y <- USPS.train[ ,257]
USPS.test.X <- USPS.test[ ,1:256]
USPS.test.Y <- USPS.test[ ,257]

USPS.Results <- data.frame(Distance = character()
                           ,K=integer()
                           ,Predictions_Total=integer()
                           ,Predictions_Correct=integer()
                           ,Predictions_Wrong=integer()
                           ,Error_Rate=double()
                           ,Percent_Correct=double()
                           , stringsAsFactors = FALSE)

i<-1
for (distanceType in c("Euclidean","Tangent")) {
 
   for (K in c(1:3)) {
  
      USPS.test.Predict <- NN(USPS.train.X, USPS.test.X, USPS.train.Y, K, distance = distanceType) 
      predictionsTotal <- length(USPS.test.Predict)
      predictionsCorrect <- sum(USPS.test.Y==USPS.test.Predict)
      predictionsWrong <- sum(USPS.test.Y!=USPS.test.Predict)
      errorRate <- predictionsWrong/predictionsTotal
      percentCorrect <- predictionsCorrect/predictionsTotal
      
      Results <- c(distanceType
                   , K
                   , predictionsTotal
                   , predictionsCorrect
                   , predictionsWrong
                   , errorRate
                   , percentCorrect)

                         
      USPS.Results[i,] <- Results
  
      i<-i+1
  }
  

}


# Normalise each row of the USPS data, and run the above again (although don't do tangent distance)

min.RowWise <- apply(USPS[ ,1:256],1,min)
max.RowWise <- apply(USPS[ ,1:256],1,max)

USPS.Normalise <- USPS
USPS.Normalise[ ,1:256] <- (USPS.Normalise[ ,1:256] - min.RowWise)/(max.RowWise-min.RowWise)

USPS.Normalise.train <- USPS.Normalise[sample.train, ]
USPS.Normalise.test <- USPS.Normalise[-sample.train, ]

USPS.Normalise.train.X <- USPS.Normalise.train[ ,1:256]
USPS.Normalise.train.Y <- USPS.Normalise.train[ ,257]
USPS.Normalise.test.X <- USPS.Normalise.test[ ,1:256]
USPS.Normalise.test.Y <- USPS.Normalise.test[ ,257]

USPS.Normalise.Results <- data.frame(Distance = character()
                           ,K=integer()
                           ,Predictions_Total=integer()
                           ,Predictions_Correct=integer()
                           ,Predictions_Wrong=integer()
                           ,Error_Rate=double()
                           ,Percent_Correct=double()
                           , stringsAsFactors = FALSE)

i<-1
for (distanceType in c("Euclidean")) {
 
   for (K in c(1:3)) {
  
      USPS.Normalise.test.Predict <- NN(USPS.Normalise.train.X, USPS.Normalise.test.X, USPS.Normalise.train.Y, K, distance = distanceType) 
      predictionsTotal <- length(USPS.Normalise.test.Predict)
      predictionsCorrect <- sum(USPS.Normalise.test.Y==USPS.Normalise.test.Predict)
      predictionsWrong <- sum(USPS.Normalise.test.Y!=USPS.Normalise.test.Predict)
      errorRate <- predictionsWrong/predictionsTotal
      percentCorrect <- predictionsCorrect/predictionsTotal
      
      Normalise.Results <- c(distanceType
                   , K
                   , predictionsTotal
                   , predictionsCorrect
                   , predictionsWrong
                   , errorRate
                   , percentCorrect)

                         
      USPS.Normalise.Results[i,] <- Normalise.Results
  
      i<-i+1
  }
  

}
