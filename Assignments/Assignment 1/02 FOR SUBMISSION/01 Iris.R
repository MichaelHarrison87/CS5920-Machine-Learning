Iris <- read.table("iris.txt", sep=",", header=FALSE)

Iris.train.X <- Iris[1:70,1:4]
Iris.train.Y <- Iris[1:70,5]
Iris.test.X <- Iris[71:100,1:4]
Iris.test.Y <- Iris[71:100,5]

Iris.Results <- data.frame(K=integer()
                           ,Predictions_Total=integer()
                           ,Predictions_Correct=integer()
                           ,Predictions_Wrong=integer()
                           ,Error_Rate=double())

i<-1
for (K in c(1:3)) {

    Iris.test.Predict <- NN(Iris.train.X, Iris.test.X, Iris.train.Y, K) 
    predictionsTotal <- length(Iris.test.Predict)
    predictionsCorrect <- sum(Iris.test.Y==Iris.test.Predict)
    predictionsWrong <- sum(Iris.test.Y!=Iris.test.Predict)
    errorRate <- predictionsWrong/predictionsTotal
    
    Results <- c(K, predictionsTotal, predictionsCorrect, predictionsWrong, errorRate)
    
    Iris.Results[i,] <- Results

    i<-i+1
}
