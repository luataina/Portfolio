#Prepare data

data <- read.csv("cardiotocography.csv", sep = ";", dec = ",")
data
str(data)

data$NSPF <- factor(data$NSP)
data$FM <- as.numeric(data$FM)
data$AC <- as.numeric(data$AC)
data$UC <- as.numeric(data$UC)
data$DL <- as.numeric(data$DL)
data$DS <- as.numeric(data$DS)
data$DP <- as.numeric(data$DP)
data$MSTV <- as.numeric(data$MSTV)
data$MLTV <- as.numeric(data$MLTV)


#Partition data into training and validation datasets
set.seed(1234)
pd <- sample(2,nrow(data), replace = TRUE, prob = c(0.8,0.2))
train <- data[pd == 1,]
validate <- data[pd == 2,]

#Decision tree with party
install.packages("party")
library(party)
train_tree <- ctree(NSPF~UC+AC+DL, data = train, controls = ctree_control(mincriterion = 0.99,minsplit = 500))
plot(train_tree)


#Prediction
predict(tree,validate,type="prob")


#Decision tree with rpart
library(rpart)

tree1 <- rpart(NSPF~UC+AC+DL, data = train)
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(tree1, extra = 1)


#Prediction
predict(tree1,validate,type="prob")

#Misclassification error for "train" data
tab <- table(predict(tree), train$NSPF)
print(tab)
1-sum(diag(tab))/sum(tab)

#Misclassification error for "validated" data
testpred <- predict(tree, newdata=validate)
tab <- table(testpred, validate$NSPF)
print(tab)
1-sum(diag(tab))/sum(tab)

