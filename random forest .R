# load data
path <- "C:\\Users\\ofir\\Desktop\\MLDM\\afterPCA.csv"
table <- read.csv(path, header = T)
rm(path)

# convert to factors
table$X <- NULL
v <- names(table)
cols <- c(v[8:19])
table[,cols] <- lapply(table[,cols], factor)

rm(v,cols)

# tag samples with numbers 1:k standing for the batch
m <- ncol(table) - 1
k <- 5 #we want 5 parts of the data

set.seed(456)
groups <- sample(k, nrow(table),replace=T)#distribute numbers 1 to 5 table rows times

comb <- combinations(5,3) #combination of the five parts into three groups 

#---------------------- select best B and M dividing using CV-K and up-sampling
m <- ncol(table) - 1
B <- seq(30,300,30)
# create empty matrix for results summation
forestResTot <- matrix(0:0, nrow = length(B), ncol = m)

five <- seq(1,5,1)#1:5

testPart <- sample(1:5,1)  # test set- 1/5 of the data
testSet <- table[(groups==testPart),]

notTest <- which(five!=testPart) #train+validation sets - 4/5 of the data

for(i in notTest){ #the test set stay out while we use k-fold on the train+valid sets
  
  trainSet <- table[(groups != i)&(groups!=testPart),] #train set - 3/5 of the data
  valSet <- table[groups == i,] #validation set - 1/5
 
  # upsampling - most of the samples were tagged as OK
  badProp <- round(length(which(trainSet$lable=="ok"))/length(which(trainSet$lable=="bad")))
  goodProp <- round(length(which(trainSet$lable=="ok"))/length(which(trainSet$lable=="good")))
  greatProp <- round(length(which(trainSet$lable=="ok"))/length(which(trainSet$lable=="great")))
  
  add <- trainSet[which(trainSet$lable=="bad"),]
  for(i in 1:badProp){
    trainSet <- rbind(trainSet, add)
  }
  
  add <- trainSet[which(trainSet$lable=="good"),]
  for(i in 1:goodProp){
    trainSet <- rbind(trainSet, add)
  }
  
  add <- trainSet[which(trainSet$lable=="great"),]
  for(i in 1:greatProp){
    trainSet <- rbind(trainSet, add)
  }
  
  # create a vector for different B (number of trees) and m (number of attributes per tree)
  B <- seq(30,300,30)
  m <- ncol(trainSet) - 1
  
  # create a results table
  forestRes <- matrix(0:0, nrow = length(B)+1, ncol = m+1) # create an extra colums for lables
  forestRes[2:nrow(forestRes), 1] <- B #  lable rows
  forestRes[1, 2:ncol(forestRes)] <- 1:m # lable columns
  
  
  # train random forests, test it using validation, calculate and store accuracy for each model
  for (h in 1:(length(B))){
    for(l in 1:m){
      model <- randomForest(x = trainSet[,1:m], y =trainSet[,m+1], ntree=B[h], mtry=l)
      pred <- predict(model, newdata = valSet[,1:m])
      forestRes[h+1,l+1] <- sum(pred == valSet[,m+1])/nrow(valSet)
      
    }
  }
  
  forestResTot <- forestResTot+ forestRes[2:nrow(forestRes),2:ncol(forestRes)] 
}

beep(0)  
path <- "C:\\Users\\ofir\\Downloads\\forestResTotAfterPCA.csv"    
write.csv(forestResTot, path)

bestPred <- max(forestResTot)
optIndex <- which(forestResTot == bestPred, arr.ind = TRUE)
optB <- forestRes[optIndex[1,1]+1,1]#270
optM <- forestRes[1,optIndex[1,2]+1]#3


#--------------------importance 
impTot <- 0

trainTotSet <- table[groups != testPart,]
model <- randomForest(x = trainTotSet[,1:m], y =trainTotSet[,m+1], ntree=optB, mtry=optM, importance = T)
imp <- importance(model)[,6]

path <- "C:\\Users\\ofirmiz\\Downloads\\impTot.csv"    
write.csv(impTot, path)


#--------------------find estimate for error on test set
path <- "C:\\Users\\ofir\\Desktop\\MLDM\\afterPCA.csv"
table <- read.csv(path, header = T)
rm(path)

# convert to factors
table$X <- NULL
v <- names(table)
cols <- c(v[8:19])
table[,cols] <- lapply(table[,cols], factor)

rm(v,cols)

# tag samples with numbers 1:k standing for the batch
m <- ncol(table) - 1
k <- 5 #we want 5 parts of the data

set.seed(456)
groups <- sample(k, nrow(table),replace=T)#distribute numbers 1 to 5 table rows times

comb <- combinations(5,3) #combination of the five parts into three groups 

#change factor levels to ordinal for lable
table$lable  <-  factor(table$lable,levels(table$lable)[c(1,4,2,3)])


# tag samples with numbers 1:k standing for the batch
m <- ncol(table) - 1
numClass <- length(levels(table[,m+1]))

optB <- 310
optM <- 3

testPart <- sample(1:5,1)
testSet <- table[(groups == testPart),]
trainSet <- table[(groups != testPart),] # we up sampling the whole train set (4/5 of the data)

#--------------upsample
badProp <- round(length(which(trainSet$lable=="ok"))/length(which(trainSet$lable=="bad")))
goodProp <- round(length(which(trainSet$lable=="ok"))/length(which(trainSet$lable=="good")))
greatProp <- round(length(which(trainSet$lable=="ok"))/length(which(trainSet$lable=="great")))

add <- trainSet[which(trainSet$lable=="bad"),]
for(i in 1:badProp){
  trainSet <- rbind(trainSet, add)
}

add <- trainSet[which(trainSet$lable=="good"),]
for(i in 1:goodProp){
  trainSet <- rbind(trainSet, add)
}

add <- trainSet[which(trainSet$lable=="great"),]
for(i in 1:greatProp){
  trainSet <- rbind(trainSet, add)
}

#------------run model and measures
model <- randomForest(x = trainSet[,1:m], y =trainSet[,m+1], ntree=optB, mtry=optM)

pred <- predict(model, newdata = testSet[,1:m]) # measure the performances of the model on the test set

acc <- sum(pred == testSet[,m+1])/nrow(testSet)
mae <- sum(abs(as.integer(testSet[,m+1])-as.integer(pred)))/nrow(testSet) #is this true 

conf_mat  <- table(predicted = pred, actual = testSet[,m+1])
    
recall <- rep(0,numClass)
precision <- rep(0,numClass)

for (j in 1:numClass){ #bad, ok, good, great
  recall[j] <- conf_mat[j,j]/sum(conf_mat[,j])
  precision[j] <- conf_mat[j,j]/sum(conf_mat[j,])
}


  
  
#----- what is the current prior probabilities
(length(which(table[,m+1]=="good"))/nrow(table))
(length(which(table[,m+1]=="bad"))/nrow(table))
(length(which(table[,m+1]=="ok"))/nrow(table))
(length(which(table[,m+1]=="great"))/nrow(table))



