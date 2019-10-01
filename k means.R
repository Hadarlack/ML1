
####------------------ table after PCA and GINI------------------

# load data
path <- "C:\\Users\\hadarlac\\Documents\\hadar ofir\\afterPCA.csv"
afterPCA <- read.csv(path, header = T)
rm(path)

#removing variables after GINI
afterPCA[,18] <- NULL
afterPCA[,16] <- NULL
afterPCA[,13] <- NULL

#--------------------goal#1--------------------
set.seed(456)

#hiding tags
afterPCA$lable <- NULL

#change "events" from categorial to dummy (binary)
afterPCA <- dummy.data.frame(afterPCA)

#normalize continous variables between 0 to 1

for(i in 1:7){
  afterPCA[,i] <- (afterPCA[,i]-min(afterPCA[,i])) / (max (afterPCA[,i])-min (afterPCA[,i]))
}

#K Means
numK <- c(2:15) 
results <- rep(0,length(numK))

for(i in 1:length(numK)){
  k             <- numK[i]
  clust_data    <- kmeans(afterPCA, centers=k, iter.max=100, nstart=10)
  results[i]    <- index.DB(afterPCA,clust_data$cluster) #we want to minimize the davis bouldin index
}

bestK <- numK[which.min (results)] #answer
bestK

plot(2:15, results, type="b", xlab="Number of Clusters",
     ylab="index.DB")

#--------------------goal#2--------------------
set.seed(7456321)
#save and remove lable

afterPCA$lable <- as.character(afterPCA$lable)
afterPCA[which(afterPCA$lable=="bad"),"lable"] <- 1
afterPCA[which(afterPCA$lable=="ok"),"lable"] <- 2
afterPCA[which(afterPCA$lable=="good"),"lable"] <- 3
afterPCA[which(afterPCA$lable=="great"),"lable"] <- 4
afterPCA$lable <- as.integer(afterPCA$lable)

lable <- afterPCA$lable
afterPCA$lable <- NULL #hiding tags

#change "events" from categorial to dummy (binary)
afterPCA <- dummy.data.frame(afterPCA)

#normalize continous variables between 0 to 1

for(i in 1:7){
  afterPCA[,i] <- (afterPCA[,i]-min(afterPCA[,i])) / (max (afterPCA[,i])-min (afterPCA[,i]))
}

kMeansData <- kmeans(afterPCA, 4) # 4 cluster solution

clusters <- kMeansData$cluster
lableFIT <- rep(0,4)

lab1 <- rep(1,2550)
lab2 <- rep(2,2550)
lab3 <- rep(3,2550)
lab4 <- rep(4,2550)
lab <- cbind(lab1,lab2,lab3,lab4)
lab <- as.data.frame(lab)

together <- as.data.frame(cbind(clusters, lable,lab))


lableFIT[1] <- sqldf(" select count(*)
                     from together
                     where clusters==1
                     group by lable")

lableFIT[2] <- sqldf(" select count(*)
                     from together
                     where clusters==2
                     group by lable")
lableFIT[3] <- sqldf(" select count(*)
                     from together
                     where clusters==3 
                     group by lable")
lableFIT[4] <- sqldf(" select count(*)
                     from together
                     where clusters==4 
                     group by lable")

lableFIT <- as.data.frame(lableFIT)



####------------------ table only our variables----------------------------------------

# load data
path <- "C:\\Users\\hadarlac\\Documents\\hadar ofir\\ted_main_initial.csv"
initial <- read.csv(path, header = T)
rm(path)

initial[,2:5] <- NULL
initial[,3:12] <- NULL

#normalize all variables between 0 to 1

for(i in 1:3){
  initial[,i] <- (initial[,i]-min(initial[,i])) / (max (initial[,i])-min (initial[,i]))
}

#-------------goal#3---------------------

set.seed(666)
#K Means
numK <- c(2:15) 
results <- rep(0,length(numK))

for(i in 1:length(numK)){
  k             <- numK[i]
  clusters_data    <- kmeans(initial, centers=k, iter.max=100, nstart=10)
  results[i]    <- index.DB(initial,clusters_data$cluster) #we want to minimize the davis bouldin index
}

bestK <- numK[which.min (results)] #answer
bestK

plot(2:15, results, type="b", xlab="Number of Clusters",
     ylab="index.DB")


#-------------goal#4---------------------
set.seed(8888)

# load data
path <- "C:\\Users\\hadarlac\\Documents\\hadar ofir\\afterPCA.csv"
afterPCA <- read.csv(path, header = T)
rm(path)

afterPCA$lable <- as.character(afterPCA$lable)
afterPCA[which(afterPCA$lable=="bad"),"lable"] <- 1
afterPCA[which(afterPCA$lable=="ok"),"lable"] <- 2
afterPCA[which(afterPCA$lable=="good"),"lable"] <- 3
afterPCA[which(afterPCA$lable=="great"),"lable"] <- 4
afterPCA$lable <- as.integer(afterPCA$lable)

lable <- afterPCA$lable

clusters_4_data    <- kmeans(initial, 4) # 4 cluster solution

clusters <- clusters_4_data$cluster
lableFIT <- rep(0,4)

lab1 <- rep(1,2550)
lab2 <- rep(2,2550)
lab3 <- rep(3,2550)
lab4 <- rep(4,2550)
lab <- cbind(lab1,lab2,lab3,lab4)
lab <- as.data.frame(lab)

together <- as.data.frame(cbind(clusters, lable,lab))


lableFIT[1]<- sqldf(" select count(*)
                     from together
                     where clusters==1
                     group by lable")

lableFIT[2] <- sqldf(" select count(*)
                     from together
                     where clusters==2
                     group by lable")
lableFIT[3] <- sqldf(" select count(*)
                     from together
                     where clusters==3 
                     group by lable")
lableFIT[4] <- sqldf(" select count(*)
                     from together
                     where clusters==4 
                     group by lable")

lableFIT <- as.data.frame(lableFIT)

