# Implements K Nearest Neighbours

# Function takes in training attributes (train.X) & training labels (train.Y)
# as well as test attributes, whose labels we wish to predict.

# We will iterate through each row of the test.X data, and apply a subfunction to each one.
# This subfunction works out the Euclidean (for now) distance between the given test point 
# and each of the training atttibutes.

# This distance will also be calculated by a subfunction, which we'll set up to handle other types of distance than just Euclidean

# We then need to find the closest K points in the test data and find their most common label.
# We apply this label to the given test point as our predicted response.

dyn.load("distance.dll")
tangentDistance <- function(imageOne,imageTwo) {
  out <- .C("distance",
    img1=as.double(imageOne),
    img2=as.double(imageTwo),
    dist=as.double(0))
  return(out$dist)
}

 calcDistance <- function(X1,X2, Type='Euclidean') {

   if(Type=='Euclidean') {
  
     squareDiffs = (X2 - X1)^2
     distance = sqrt(sum(squareDiffs))
  
  
   } else if (Type=='Tangent') {
     
      distance = tangentDistance(X1,X2)
      
   } else {
  
    # Placeholder
    distance = 0
   }

  return(distance)
 }
 
getFreqDist <- function(values) {
   

  valuesName <- deparse(substitute(values))
  freqDist <- data.frame(Value = character()
                         , Freq = integer()
                         , stringsAsFactors = FALSE)
  colnames(freqDist) <- c(valuesName, "Freq")

  values <- as.character(values)
  
  counter <- 1
  for (i in values) {
    
    if (sum(i==freqDist[,1])==0) {
      
      freqDist[counter, 1] <- i
      freqDist[counter, 2] <- 1
      counter <- counter +1   
      
    } else {
      
      position <- which(i==freqDist[,1])
      freqDist[position, 2] <- freqDist[position, 2] + 1
      
    }


  }
  
  return(freqDist)
   
}
 
getNN_Indiv <- function(train.X, test.X.Indiv, train.Y, k, distance='Euclidean') {
  
    
# Note test.X is a single test point, i.e. a vector
# Want to return the most common label (train.Y) from 
# the nearest K points to the test obs (test.X) in the training data (train.X)
  
  # Handle edge case where K is larger than the training data
  # Use whole training set as "nearest" neighbours in this case
  if (k>nrow(train.X)) k=nrow(train.X) 
  
  
  # Create vector of distances from training data to test obs
  distances = apply(train.X, 1, function(t) calcDistance(t,test.X.Indiv,distance))
  

# Have list of distances between training data & the test obs
# Need to find the K smallest of these.
# Can't use sort/order commands, so instead iteratively find the min of the list of distances
# But remove these minimum distances from the list at each iteration, so they are ignored in the next one.
# Repeat this until have removed K distances
  
  dummyDistances = cbind(distances, c(1:length(distances)))
  dummyDistances <- as.data.frame(dummyDistances)
  colnames(dummyDistances) <- c("Distance", "Index")
  minDistToUse = data.frame(distances=double()
                                  ,index = double())
  i=0
  while (i < k) {
    
    minDist = min(dummyDistances$Distance)
    minDistItems <- dummyDistances[dummyDistances$Distance==minDist,]
    minDistItems <- as.data.frame(minDistItems)
    colnames(minDistItems) <- c("Distance", "Index")
    numMin <- nrow(minDistItems)
    
    if((i+numMin)>k) {
# Note numMin will usually be 1, but may be larger due to ties 
# (i.e. training obs' with equal distances to the test obs)
# This is fine, unless counting such duplicates exceeds K (when added to the min values
# found in previous iterations).      
# In this case, pick randomly from the duplicate obs to make up to K total obs.      
    
      numMin <- k-i
      sampleIndices <- sample(minDistItems$Index, numMin, replace= FALSE) 
      minDistItems <- minDistItems[minDistItems$Index %in% sampleIndices,]        
      
    }
     

  dummyDistances <- dummyDistances[dummyDistances$Distance!=minDistItems$Distance,]
  minDistToUse  <- rbind(minDistToUse, minDistItems)
  i = i + numMin
  }

  # Get corresponding training labels for the set of k training distances found above
  labelsToUse <- train.Y[minDistToUse$Index]
  labelsFreqDist <- as.data.frame(getFreqDist(labelsToUse))
  
  mostFreq <- max(labelsFreqDist$Freq)
  labelsMostFreq <- labelsFreqDist[labelsFreqDist$Freq ==mostFreq,1]
  
  # If there are ties in most common label, break randomly
  labelToReturn <- sample(labelsMostFreq,1)
  
  
 
  return(labelToReturn)
}


NN <- function(train.X, test.X, train.Y, k, distance='Euclidean') {
  
  train.X <- as.matrix(train.X)
  test.X <- as.matrix(test.X)
  train.Y <- as.matrix(train.Y)
  
  predictedLabels <- apply(test.X, 1, function(t) getNN_Indiv(train.X, t, train.Y, k, distance))
  
  
  return(predictedLabels)
  
  
} 