#read train & test file to table
dataRow.train <- read.csv(file="train.csv", sep=",", header=T)
dataRow.test <- read.csv(file="test.csv", sep=",", header=T)

#find average age for each title (Miss, Mr, Mrs, Master)
dataRow.name <- model.frame(~ Name + Age, data = dataRow.train)

chars_miss <- "Miss"
miss_age <- 0
n_miss <- 0

chars_mr <- "Mr"
mr_age <- 0
n_mr <- 0

chars_Mrs <- "Mrs"
Mrs_age <- 0
n_Mrs <- 0

chars_Master <- "Master"
Master_age <- 0
n_Master <- 0

all_age <- 0
n_all <- 0

for (i in 1:nrow(dataRow.name)){
  if (grepl(chars_miss,dataRow.name$Name[i])==TRUE){
    miss_age = miss_age+dataRow.name$Age[i]
    n_miss = n_miss+1
  }
  
  if (grepl(chars_mr,dataRow.name$Name[i])==TRUE){
    mr_age = mr_age+dataRow.name$Age[i]
    n_mr = n_mr+1
  }
  
  if (grepl(chars_Mrs,dataRow.name$Name[i])==TRUE){
    Mrs_age = Mrs_age+dataRow.name$Age[i]
    n_Mrs = n_Mrs+1
  }
  
  if (grepl(chars_Master,dataRow.name$Name[i])==TRUE){
    Master_age = Master_age+dataRow.name$Age[i]
    n_Master = n_Master+1
  }
  
  all_age = all_age+dataRow.name$Age[i]
  n_all = n_all+1
  
}

#find average fare for each Pclass
fare_3 <- 0

fare_2 <- 0

fare_1 <- 0

for (i in 1:nrow(dataRow.name)){
  if (dataRow.train$Pclass[i]==3){
    fare_3 = fare_3+dataRow.train$Fare
  }
  if (dataRow.train$Pclass[i]==2){
    fare_2 = fare_2+dataRow.train$Fare
  }
  if (dataRow.train$Pclass[i]==1){
    fare_1 = fare_1+dataRow.train$Fare
  }
}

miss_age_average = miss_age/n_miss
mr_age_average = mr_age/n_mr
Mrs_age_average = Mrs_age/n_Mrs
master_age_average = Master_age/n_Master
all_average <- all_age/n_all


miss_age_average = round(miss_age_average, digits = 2)
mr_age_average = round(mr_age_average, digits = 2)
Mrs_age_average = round(Mrs_age_average, digits = 2)
master_age_average = round(master_age_average, digits=2)
all_average = round(all_average, digits=2)

mean_fare_1 = round(mean(fare_1))
mean_fare_2 = round(mean(fare_2))
mean_fare_3 = round(mean(fare_3))

dataRow.train$Title = ''
dataRow.train$FamilySize = 0
for (i in 1:nrow(dataRow.train)){
  if (grepl(chars_miss,dataRow.train$Name[i])==TRUE){
    dataRow.train$Title[i] <- 'Miss'
  }else if (grepl(chars_Mrs,dataRow.train$Name[i])==TRUE){
    dataRow.train$Title[i] <- 'Mrs'
  }else if (grepl(chars_mr,dataRow.train$Name[i])==TRUE){
    dataRow.train$Title[i] <- 'Mr'
  }else if (grepl(chars_Master,dataRow.train$Name[i])==TRUE){
    dataRow.train$Title[i] <- 'Master'
  }else{
    dataRow.train$Title[i] <- 'Other'
  }
  dataRow.train$FamilySize[i] = dataRow.train$SibSp[i] + dataRow.train$Parch[i]
  
  #missing value handling
  if (is.na(dataRow.train$Age[i])==TRUE){
    if (grepl(chars_miss,dataRow.train$Name[i])==TRUE){
      dataRow.train$Age[i] <- miss_age_average
    }else if (grepl(chars_Mrs,dataRow.train$Name[i])==TRUE){
      dataRow.train$Age[i] <- Mrs_age_average
    }else if (grepl(chars_mr,dataRow.train$Name[i])==TRUE){
      dataRow.train$Age[i] <- mr_age_average
    }else if (grepl(chars_Master,dataRow.train$Name[i])==TRUE){
      dataRow.train$Age[i] <- master_age_average
    }else{
      dataRow.train$Age[i] <- all_average
    }
  }
  
  if (dataRow.train$Fare[i]==0){
    if (dataRow.train$Pclass[i]==1){
      dataRow.train$Fare[i] <- mean_fare_1
    } else if (dataRow.train$Pclass[i]==2){
      dataRow.train$Fare[i] <- mean_fare_2
    } else {
      dataRow.train$Fare[i] <- mean_fare_3
    }
  }
  
  if (!grepl("C",dataRow.train$Embarked[i]) && !grepl("Q",dataRow.train$Embarked[i]) && !grepl("S",dataRow.train$Embarked[i])){ 
    dataRow.train$Embarked[i] <- "S"
  }
}

data.train <- model.frame(~ Survived + Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize, data = dataRow.train)
data.train$Survived <- as.factor(data.train$Survived)
data.train$Embarked <- factor(data.train$Embarked)
data.train$Title <- as.factor(data.train$Title)

#cek missing value
#MS=is.na.data.frame(dataRow.train$Age)
#Missing_value=data.frame(t(MS))

dataRow.test$Title = ''
dataRow.test$FamilySize = 0
for (i in 1:nrow(dataRow.test)){
  if (grepl(chars_miss,dataRow.test$Name[i])==TRUE){
    dataRow.test$Title[i] <- 'Miss'
  }else if (grepl(chars_Mrs,dataRow.test$Name[i])==TRUE){
    dataRow.test$Title[i] <- 'Mrs'
  }else if (grepl(chars_mr,dataRow.test$Name[i])==TRUE){
    dataRow.test$Title[i] <- 'Mr'
  }else if (grepl(chars_Master,dataRow.test$Name[i])==TRUE){
    dataRow.test$Title[i] <- 'Master'
  }else{
    dataRow.test$Title[i] <- 'Other'
  }
  dataRow.test$FamilySize[i] = dataRow.test$SibSp[i] + dataRow.test$Parch[i]
  
  #missing value handling
  if (is.na(dataRow.test$Age[i])==TRUE){
    if (grepl(chars_miss,dataRow.test$Name[i])==TRUE){
      dataRow.test$Age[i] <- miss_age_average
    }else if (grepl(chars_Mrs,dataRow.test$Name[i])==TRUE){
      dataRow.test$Age[i] <- Mrs_age_average
    }else if (grepl(chars_mr,dataRow.test$Name[i])==TRUE){
      dataRow.test$Age[i] <- mr_age_average
    }else if (grepl(chars_Master,dataRow.test$Name[i])==TRUE){
      dataRow.test$Age[i] <- master_age_average
    }else{
      dataRow.test$Age[i] <- all_average
    }
  }
  
  if (dataRow.test$Fare[i]==0 || is.na(dataRow.test$Fare[i])){
    if (dataRow.test$Pclass[i]==1){
      dataRow.test$Fare[i]<-mean_fare_1
    } else if (dataRow.test$Pclass[i]==2){
      dataRow.test$Fare[i]<-mean_fare_2
    } else {
      dataRow.test$Fare[i]<-mean_fare_3
    }
  }
}
data.test <- model.frame(~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize, data = dataRow.test)
data.test$Title <- as.factor(data.test$Title)

summary(data.train)
summary(data.test)

#Random forest
library(randomForest)
rf.model <- randomForest(Survived ~ ., data.train, ntree=5000, mtry=3)
rf.model
rf.test.prediction <- predict(rf.model,data.test)

#SVM
library(e1071)

#iseng-iseng pso
#parameter
n_par <- 10
c_1 <- 2
c_2 <- 2
n_iter <- 20

par <- data.frame(gamma = double(n_par), cost = double(n_par), fitness = double(n_par))
pbest <- par

#init particle
for (i in 1:n_par) {
  par$gamma[i] <- runif(1, 0, 5)
  par$cost[i] <- runif(1, 0, 100)
}

gbest <- data.frame(gamma = double(n_iter), cost = double(n_iter), fitness = double(n_iter))

i <- 1
while (i <= n_iter) {
  print(i)
  #cari pbest dan gbest
  print('hitung pbest dan gbest')
  for (j in 1:n_par) {
    par$fitness[j] <- mean(svm(Survived ~ ., data.train, cross=10, 
                               gamma = par$gamma[j],
                               cost = par$cost[j]
    )$accuracies)
    
    if (par$fitness[j] > pbest$fitness[j]){
      pbest[j,] = par[j,]
    }
    
    if (par$fitness[j] > gbest$fitness[i]){
      gbest[i,] = par[j,]
    }
  }
  
  print('hitung v dan update vektor setiap partikel')
  #hitung v dan update vektor setiap partikel
  for (j in 1:n_par) {
    v_gamma <- par$gamma[j] + 
      c_1 * runif(1, 0, 1) * (pbest$gamma[j] - par$gamma[j]) +
      c_2 * runif(1, 0, 1) * (gbest$gamma[i] - par$gamma[j])
    
    v_cost <- par$cost[j] + 
      c_1 * runif(1, 0, 1) * (pbest$cost[j] - par$cost[j]) +
      c_2 * runif(1, 0, 1) * (gbest$cost[i] - par$cost[j])
    
    par$gamma[j] <- par$gamma[j] + v_gamma
    par$cost[j] <- par$cost[j] + v_cost
    
    if (par$gamma[j] <= 0){
      par$gamma[j] <- 1e-5
    }
    if (par$cost[j] <= 0){
      par$cost[j] <- 1e-5
    }
  }
  
  i<-i+1
}


svm.model <- svm(Survived ~ ., data.train, cross=10, gamma = gbest$gamma[i-1], cost = gbest$cost[i-1])
svm.train.predicted <- predict(svm.model,data.train)
table(true = data.train$Survived, pred = svm.train.predicted)
mean(svm.model$accuracies)
svm.test.prediction <- predict(svm.model,data.test)

rf.result <- model.frame(~ PassengerId, data = dataRow.test)
rf.result$Survived <- rf.test.prediction
write.csv(rf.result, file = "rfprediction.csv",row.names=FALSE)

svm.result <- model.frame(~ PassengerId, data = dataRow.test)
svm.result$Survived <- svm.test.prediction
write.csv(svm.result, file = "svmprediction.csv",row.names=FALSE)
