library(neuralnet)
library(beepr)
#---- table preparation
path <- "C:\\Users\\ofir\\Desktop\\MLDM\\afterPCA.csv"
table <- read.csv(path, header = T)
rm(path)

#turn categoty to integer
table$lable <- as.character(table$lable)
table[which(table$lable=="bad"),"lable"] <- 1
table[which(table$lable=="ok"),"lable"] <- 2
table[which(table$lable=="good"),"lable"] <- 3
table[which(table$lable=="great"),"lable"] <- 4
table$lable <- as.integer(table$lable)

table <- dummy.data.frame(table)
#table$event <- as.integer(table$event)

#turn lables into hot vectors
m <- dim(table)[2]
table <- cbind(table[,1:(m-1)],class.ind(table[,m]))
names(table) <- c(names(table)[1:m-1],"l1","l2","l3","l4")

#save table for NN
path <- "C:\\Users\\Ofir\\Desktop\\MLDM\\nnTable.csv"
write.csv(table,path)

#----------
path <- "C:\\Users\\ofir\\Desktop\\MLDM\\nnTable.csv"
table <- read.csv(path, header = T)
rm(path)

table$X <- NULL
table$X.1 <- NULL
#prepare CV 5-folds
set.seed(456)
k <- 5
groups <- sample(k, nrow(table),replace=T)#distribute numbers 1 to 5 table rows times
comb <- combinations(5,3)
five <- seq(1,5,1)

testPart <- sample(1:5,1)  # test set- 1/5 of the data
testSet <- table[(groups==testPart),]

notTest <- which(five!=testPart) #train+validation sets - 4/5 of the data


#create model formula
n <- names(table[1:m-1])
f <- as.formula(paste("l1+l2+l3+l4 ~", paste(n[!n %in% "lable"], collapse = " + ")))
rm(comb, five,n,k)

#create vectors for results
acc1tot <- c(0,0,0,0,0,0,0)
mae1tot <- c(0,0,0,0,0,0,0)
acc2tot <- c(0,0,0,0,0,0,0)
mae2tot <- c(0,0,0,0,0,0,0)
acc3tot <- c(0,0,0,0,0,0,0)
mae3tot <- c(0,0,0,0,0,0,0)


#downsample by the least amount of samples
notTestTable <- table[(groups != testPart),]
min <- min(c(length(which(notTestTable$l1==1)),length(which(notTestTable$l2==1)),length(which(notTestTable$l3==1)),length(which(notTestTable$l4==1))))

redNotTest <- notTestTable[1,]
for(i in 1:4){#add min num of rows
  str <- paste0("l",i)
  set <- notTestTable[which(notTestTable[,str]==1),]
  index <- sample(nrow(set), min, replace=F)
  add <- set[index,]
  redNotTest <- rbind(redNotTest,add)
}
redNotTest <- redNotTest[2:dim(redNotTest)[1],]#remove first fective row

#mix data
numbering <- seq(1,min,1)
numbering <- rep(numbering,4)
redNotTest <- cbind(redNotTest, as.data.frame(numbering)) 

temp <- redNotTest[1,1:(dim(redNotTest)[2]-1)]

for(i in 1:min){
  temp <- rbind(temp, redNotTest[which(numbering==i),1:(dim(redNotTest)[2]-1)])
}
redNotTest <- temp[2:(dim(temp)[1]),]

k <- 4
selecting <- c()
for(i in 1:k){
  selecting <- c(selecting, rep(i,min))
}
i <- 2
for(i in 1:k){ #the test set stay out while we use k-fold on the train+valid sets

  trainSet <- redNotTest[(selecting != i),] #train set - 3/4 of the reduced data############################
  valSet <- redNotTest[selecting == i,] #validation set - 1/4
  
  trainSet[,1:7] <- scale(trainSet[,1:7])
  valSet[,1:7] <- scale(valSet[,1:7])
  
  
  #create vectors for output
  acc1 <- c()
  mae1 <- c()
  curAcc1 <- 0
  curMae1 <- 99
  numNeurons <- c(10,50,100,150,200,250,300)
  #run 1 layer perceptron
  for(i in 1:length(numNeurons)){
    for (w in 1:5){#find best out of 5 weight initiations
      nn <- neuralnet(f,data=trainSet,hidden=numNeurons[i] ,linear.output=F, err.fct = "sse", act.fct = "logistic", algorithm = "rprop+", stepmax = 200000, learningrate = 0.01, lifesign = "full", lifesign.step = 1000)
      #predict
      res <- compute(nn, covariate = valSet[,1:(m-1)])
      res <- as.matrix(res$net.result)
      predictions <- c()
      trueLables <- c()
      for (j in 1:dim(res)[1]){
        predictions[j] <- which(res[j,]==max(res[j,]))[1]
        trueLables[j] <- which(valSet[j,m:(m+3)]==max(valSet[j,m:(m+3)]))
      }
      curAcc1 <- max(mean(predictions==trueLables),curAcc1)
      curMae1 <- min(sum(abs(as.integer(trueLables)-as.integer(predictions)))/length(trueLables),curMae1)
    }
    acc1[i] <- curAcc1
    mae1[i] <- curMae1
  }#end for
  acc1tot <- acc1tot+acc1
  mae1tot <- mae1tot+mae1
  
  #create vectors for output
  acc2 <- c()
  mae2 <- c()
  curAcc2 <- 0
  curMae2 <- 99
  numNeurons <- c(10,50,100,150,200,250,300)
  #run 2 layer perceptron
  for(i in 1:length(numNeurons)){
    for (w in 1:5){#find best out of 5 weight initiations
      nn <- neuralnet(f,data=trainSet,hidden=c(numNeurons[i],numNeurons[i]) ,linear.output=F, err.fct = "sse", act.fct = "logistic", algorithm = "rprop+", stepmax = 200000, learningrate = 0.01, lifesign = "full", lifesign.step = 1000)
      #predict
      res <- compute(nn, covariate = valSet[,1:(m-1)])
      res <- as.matrix(res$net.result)
      predictions <- c()
      trueLables <- c()
      for (j in 1:dim(res)[1]){
        predictions[j] <- which(res[j,]==max(res[j,]))[1]
        trueLables[j] <- which(valSet[j,m:(m+3)]==max(valSet[j,m:(m+3)]))
      }
      curAcc2 <- max(mean(predictions==trueLables),curAcc2)
      curMae2 <- min(sum(abs(as.integer(trueLables)-as.integer(predictions)))/length(trueLables),curMae2)
    }
    acc2[i] <- curAcc2
    mae2[i] <- curMae2
  }#end for
  acc2tot <- acc2tot+acc2
  mae2tot <- mae2tot+mae2
  
  #create vectors for output
  acc3 <- c()
  mae3 <- c()
  curAcc3 <- 0
  curMae3 <- 99
  numNeurons <- c(10,50,100,150,200,250,300)
  #run 3 layer perceptron
  for(i in 1:length(numNeurons)){
    for (w in 1:5){#find best out of 5 weight initiations
      nn <- neuralnet(f,data=trainSet,hidden=c(numNeurons[i],numNeurons[i],numNeurons[i]) ,linear.output=F, err.fct = "sse", act.fct = "logistic", algorithm = "rprop+", stepmax = 200000, learningrate = 0.01, lifesign = "full", lifesign.step = 1000)
      #predict
      res <- compute(nn, covariate = valSet[,1:(m-1)])
      res <- as.matrix(res$net.result)
      predictions <- c()
      trueLables <- c()
      for (j in 1:dim(res)[1]){
        predictions[j] <- which(res[j,]==max(res[j,]))[1]
        trueLables[j] <- which(valSet[j,m:(m+3)]==max(valSet[j,m:(m+3)]))
      }
      curAcc3 <- max(mean(predictions==trueLables),curAcc3)
      curMae3 <- min(sum(abs(as.integer(trueLables)-as.integer(predictions)))/length(trueLables),curMae3)
    }
    acc3[i] <- curAcc3
    mae3[i] <- curMae3
  }#end for
  acc3tot <- acc3tot+acc3
  mae3tot <- mae3tot+mae3
}

beep(5)

acc1tot <- acc1tot/k
mae1tot <- mae1tot/k
acc2tot <- acc2tot/k
mae2tot <- mae2tot/k
acc3tot <- acc3tot/k
mae3tot <- mae3tot/k

#create graphs
results <- as.data.frame(rbind(cbind(acc1tot,1,numNeurons), cbind(acc2tot,2,numNeurons), cbind(acc3tot,3,numNeurons)))
names(results) <- c("AVG_Accuracy", "Hidden_Layers", "Num_of_Neurons")
results$Hidden_Layers <- as.factor(results$Hidden_Layers)
ggplot(data = results, aes(x = Num_of_Neurons, y = AVG_Accuracy , group= Hidden_Layers, color = Hidden_Layers)) +
  geom_line(size=2)+scale_color_manual(values=c("#fe9929", "#cc4c02", "#8c2d04"))+xlab("Num of Neurons")+ylab("AVG Accuracy")

results <- as.data.frame(rbind(cbind(mae1tot,1,numNeurons), cbind(mae2tot,2,numNeurons), cbind(mae3tot,3,numNeurons)))
names(results) <- c("AVG_MAE", "Hidden_Layers", "Num_of_Neurons")
results$Hidden_Layers <- as.factor(results$Hidden_Layers)
ggplot(data = results, aes(x = Num_of_Neurons, y = AVG_MAE , group= Hidden_Layers, color = Hidden_Layers)) +
  geom_line(size=2)+scale_color_manual(values=c("#fe9929", "#cc4c02", "#8c2d04"))+xlab("Num of Neurons")+ylab("AVG MAE")

#----------focused runs between 80 to 120
path <- "C:\\Users\\ofir\\Desktop\\MLDM\\nnTable.csv"
table <- read.csv(path, header = T)
rm(path)

table$X <- NULL
table$X.1 <- NULL
#prepare CV 5-folds
set.seed(456)
k <- 5
groups <- sample(k, nrow(table),replace=T)#distribute numbers 1 to 5 table rows times
comb <- combinations(5,3)
five <- seq(1,5,1)

testPart <- sample(1:5,1)  # test set- 1/5 of the data
testSet <- table[(groups==testPart),]

notTest <- which(five!=testPart) #train+validation sets - 4/5 of the data

m <- 25
#create model formula
n <- names(table[1:m-1])
f <- as.formula(paste("l1+l2+l3+l4 ~", paste(n[!n %in% "lable"], collapse = " + ")))
rm(comb, five,n,k)

#create vectors for results
acc3tot <- c(0,0,0,0,0,0,0,0,0)
mae3tot <- c(0,0,0,0,0,0,0,0,0)


#downsample by the least amount of samples
notTestTable <- table[(groups != testPart),]
min <- min(c(length(which(notTestTable$l1==1)),length(which(notTestTable$l2==1)),length(which(notTestTable$l3==1)),length(which(notTestTable$l4==1))))

redNotTest <- notTestTable[1,]
for(i in 1:4){#add min num of rows
  str <- paste0("l",i)
  set <- notTestTable[which(notTestTable[,str]==1),]
  index <- sample(nrow(set), min, replace=F)
  add <- set[index,]
  redNotTest <- rbind(redNotTest,add)
}
redNotTest <- redNotTest[2:dim(redNotTest)[1],]#remove first fective row

#mix data
numbering <- seq(1,min,1)
numbering <- rep(numbering,4)
redNotTest <- cbind(redNotTest, as.data.frame(numbering)) 

temp <- redNotTest[1,1:(dim(redNotTest)[2]-1)]

for(i in 1:min){
  temp <- rbind(temp, redNotTest[which(numbering==i),1:(dim(redNotTest)[2]-1)])
}
redNotTest <- temp[2:(dim(temp)[1]),]

k <- 4
selecting <- c()
for(i in 1:k){
  selecting <- c(selecting, rep(i,min))
}

for(i in 1:k){ #the test set stay out while we use k-fold on the train+valid sets
  
  trainSet <- redNotTest[(selecting != i),] #train set - 3/4 of the reduced data
  valSet <- redNotTest[selecting == i,] #validation set - 1/4
  
  trainSet[,1:7] <- scale(trainSet[,1:7])
  valSet[,1:7] <- scale(valSet[,1:7])
  
  #create vectors for output
  acc3 <- c()
  mae3 <- c()
  curAcc3 <- 0
  curMae3 <- 99
  numNeurons <- seq(80,120,5)
  #run 3 layer perceptron
  for(i in 1:length(numNeurons)){
    for (w in 1:5){#find best out of 5 weight initiations
      nn <- neuralnet(f,data=trainSet,hidden=c(numNeurons[i],numNeurons[i],numNeurons[i]) ,linear.output=F, err.fct = "sse", act.fct = "logistic", algorithm = "rprop+", stepmax = 200000, learningrate = 0.01, lifesign = "full", lifesign.step = 1000)
      #predict
      res <- compute(nn, covariate = valSet[,1:(m-1)])
      res <- as.matrix(res$net.result)
      predictions <- c()
      trueLables <- c()
      for (j in 1:dim(res)[1]){
        predictions[j] <- which(res[j,]==max(res[j,]))[1]
        trueLables[j] <- which(valSet[j,m:(m+3)]==max(valSet[j,m:(m+3)]))
      }
      curAcc3 <- max(mean(predictions==trueLables),curAcc3)
      curMae3 <- min(sum(abs(as.integer(trueLables)-as.integer(predictions)))/length(trueLables),curMae3)
    }
    acc3[i] <- curAcc3
    mae3[i] <- curMae3
  }#end for
  acc3tot <- acc3tot+acc3
  mae3tot <- mae3tot+mae3
}

beep(5)

acc1tot <- acc1tot/k
mae1tot <- mae1tot/k
acc2tot <- acc2tot/k
mae2tot <- mae2tot/k
acc3tot <- acc3tot/k
mae3tot <- mae3tot/k

#create graphs
results <- as.data.frame(cbind(acc3tot,3,numNeurons))
names(results) <- c("AVG_Accuracy", "Hidden_Layers", "Num_of_Neurons")
results$Hidden_Layers <- as.factor(results$Hidden_Layers)
ggplot(data = results, aes(x = Num_of_Neurons, y = AVG_Accuracy , group= Hidden_Layers, color = Hidden_Layers)) +
  geom_line(size=2)+scale_color_manual(values=c("#8c2d04"))+xlab("Num of Neurons")+ylab("AVG Accuracy")

results <- as.data.frame(cbind(mae3tot,3,numNeurons))
names(results) <- c("AVG_MAE", "Hidden_Layers", "Num_of_Neurons")
results$Hidden_Layers <- as.factor(results$Hidden_Layers)
ggplot(data = results, aes(x = Num_of_Neurons, y = AVG_MAE , group= Hidden_Layers, color = Hidden_Layers)) +
  geom_line(size=2)+scale_color_manual(values=c("#8c2d04"))+xlab("Num of Neurons")+ylab("AVG MAE")


#----------test
path <- "C:\\Users\\ofir\\Desktop\\MLDM\\nnTable.csv"
table <- read.csv(path, header = T)
rm(path)

table$X <- NULL
table$X.1 <- NULL
#prepare CV 5-folds
set.seed(456)
k <- 5
groups <- sample(k, nrow(table),replace=T)#distribute numbers 1 to 5 table rows times
comb <- combinations(5,3)
five <- seq(1,5,1)

testPart <- sample(1:5,1) # test set- 1/5 of the data
testSet <- table[(groups==testPart),]

notTest <- which(five!=testPart) #train+validation sets - 4/5 of the data

m <- 25
#create model formula
n <- names(table[1:m-1])
f <- as.formula(paste("l1+l2+l3+l4 ~", paste(n[!n %in% "lable"], collapse = " + ")))
rm(comb, five,n,k)


#downsample by the least amount of samples
notTestTable <- table[(groups != testPart),]
min <- min(c(length(which(notTestTable$l1==1)),length(which(notTestTable$l2==1)),length(which(notTestTable$l3==1)),length(which(notTestTable$l4==1))))

redNotTest <- notTestTable[1,]
for(i in 1:4){#add min num of rows
  str <- paste0("l",i)
  set <- notTestTable[which(notTestTable[,str]==1),]
  index <- sample(nrow(set), min, replace=F)
  add <- set[index,]
  redNotTest <- rbind(redNotTest,add)
}
redNotTest <- redNotTest[2:dim(redNotTest)[1],]#remove first fective row

#mix data
numbering <- seq(1,min,1)
numbering <- rep(numbering,4)
redNotTest <- cbind(redNotTest, as.data.frame(numbering)) 

temp <- redNotTest[1,1:(dim(redNotTest)[2]-1)]

for(i in 1:min){
  temp <- rbind(temp, redNotTest[which(numbering==i),1:(dim(redNotTest)[2]-1)])
}
redNotTest <- temp[2:(dim(temp)[1]),]

k <- 4
selecting <- c()

for(i in 1:k){
  selecting <- c(selecting, rep(i,min))
}

topMaxBP <- 0
topMaxGoP <- 0
topMaxGrP <- 0
fminBP <- 1
fminGP <- 1
fminBR <- 1
fminGR <- 1


trainSet <- redNotTest[(selecting != 1),] #train set - 3/4 of the reduced data############################
valSet <- redNotTest[selecting == 1,] #validation set - 1/4

trainSet[,1:7] <- scale(trainSet[,1:7])
valSet[,1:7] <- scale(valSet[,1:7])

#create vectors for output

totMaxBP <- 0
totMaxGP <- 0
minBP <- 1
minGP <- 1
minBR <- 1
minGR <- 1

for (w in 1:30){#find best out of 5 weight initiations
  nn <- neuralnet(f,data=trainSet,hidden=c(110,110,110) ,linear.output=F, err.fct = "sse", act.fct = "logistic", algorithm = "rprop+", stepmax = 200000, learningrate = 0.01, lifesign = "full", lifesign.step = 1000)
  #predict
  res <- compute(nn, covariate = valSet[,1:(m-1)])
  res <- as.matrix(res$net.result)
  predictions <- c()
  trueLables <- c()
  for (j in 1:dim(res)[1]){
    predictions[j] <- which(res[j,]==max(res[j,]))[1]
    trueLables[j] <- which(valSet[j,m:(m+3)]==max(valSet[j,m:(m+3)]))
  }
  
  conf_mat  <- table(predicted = predictions, actual = trueLables)
  if((dim(conf_mat)[1]==4)&(dim(conf_mat)[2]==4)){
    minBP <- conf_mat[1,4]/sum(conf_mat[1,])
    minGP <- conf_mat[4,1]/sum(conf_mat[4,])
    minBR <- conf_mat[1,4]/sum(conf_mat[,4])
    minGR <- conf_mat[4,1]/sum(conf_mat[1,])
    if((minBP< fminBP) & (minGP< fminGP) & (minBR< fminBR) & (minGR< fminGR)){
      fminBP <- minBP
      fminGP <- minGP
      fminBR <- minBR
      fminGR <- minGR
      
      maxBP <- conf_mat[1,1]/sum(conf_mat[1,])
      maxGP <- conf_mat[4,4]/sum(conf_mat[4,])
      if((totMaxBP<maxBP)&(totMaxGP<maxGP)){
        selectedModel <- nn
        totMaxBP <- maxBP
        totMaxGP <- maxGP
      }
      
    }
  }
}

#predict
res <- compute(selectedModel, covariate = testSet[,1:(m-1)])
res <- as.matrix(res$net.result)
predictions <- c()
trueLables <- c()
for (j in 1:dim(res)[1]){
  predictions[j] <- which(res[j,]==max(res[j,]))[1]
  trueLables[j] <- which(testSet[j,m:(m+3)]==max(testSet[j,m:(m+3)]))
}

#measures
acc <- mean(predictions==trueLables)
mae <- sum(abs(as.integer(trueLables)-as.integer(predictions)))/length(trueLables)
conf_mat  <- table(predicted = predictions, actual = trueLables)

numClass <- 4
recall <- rep(0,numClass)
precision <- rep(0,numClass)
for (j in 1:numClass){
  recall[j] <- conf_mat[j,j]/sum(conf_mat[,j])
  precision[j] <- conf_mat[j,j]/sum(conf_mat[j,])
}


beep(5)