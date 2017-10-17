Ion <- read.table("ionosphere.txt", sep=",", header=FALSE)

numTrain <- 200

Ion.train.X <- Ion[1:numTrain,1:34]
Ion.train.Y <- Ion[1:numTrain,35]
Ion.test.X <- Ion[(numTrain+1):nrow(Ion),1:34]
Ion.test.Y <- Ion[(numTrain+1):nrow(Ion),35]

Ion.Results <- data.frame(K=integer()
                           ,Predictions_Total=integer()
                           ,Predictions_Correct=integer()
                           ,Predictions_Wrong=integer()
                           ,Error_Rate=double()
                           ,Percent_Correct=double() )

i<-1
for (K in c(1:3)) {

    Ion.test.Predict <- NN(Ion.train.X, Ion.test.X, Ion.train.Y, K) 
    predictionsTotal <- length(Ion.test.Predict)
    predictionsCorrect <- sum(Ion.test.Y==Ion.test.Predict)
    predictionsWrong <- sum(Ion.test.Y!=Ion.test.Predict)
    errorRate <- predictionsWrong/predictionsTotal
    percentCorrect <- predictionsCorrect/predictionsTotal
    
    Results <- c(K
                 , predictionsTotal
                 , predictionsCorrect
                 , predictionsWrong
                 , errorRate
                 , percentCorrect)
    
    Ion.Results[i,] <- Results

    i<-i+1
}
