raw.train <- read.csv("/home/iftikar/workspace/kaggle_titanic/input/train.csv", header = TRUE)
raw.test <- read.csv("/home/iftikar/workspace/kaggle_titanic/input/test.csv", header = TRUE)

dataset.train <- model.frame(~ Survived+Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, data = raw.train)
dataset.test <- model.frame(~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, data = raw.test)

#test