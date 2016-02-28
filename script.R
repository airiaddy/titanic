dataRow.train <- read.csv(file="C:/Users/evalinalisbeth/Documents/Titanic/train.csv", sep=",", header=T)
dataRow.test <- read.csv(file="C:/Users/evalinalisbeth/Documents/Titanic/test.csv", sep=",", header=T)

data.train <- model.frame(~ Survived + Pclass + Sex + Age + SibSp + Fare + Embarked, data = dataRow.train)
data.test <- model.frame(~  Pclass + Sex + Age + SibSp + Fare + Embarked, data = dataRow.test)

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
Mrs_age <- 0
n_Master <- 0

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
    Master_age = Master_age+dataRow.name$Master[i]
    n_Master = n_Master+1
  }
}

miss_age_average = miss_age/n_miss
