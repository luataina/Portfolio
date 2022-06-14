#installing ROC packages
install.packages("pROC")
library(pROC) 

set.seed(420) 
num.samples <- 100

#Genereating 100 values from a normal distribution (mean 172 and standard deviation 29) then sort them
weight <- sort(rnorm(n=num.samples, mean=172, sd=29))

weight
hist(weight)

#Randomly assigning values as obese or not obese
obese <- ifelse(test=(runif(n=num.samples) < (rank(weight)/num.samples)), yes=1, no=0)
obese

#Plot the data
plot(x=weight, y=obese)

#Fit a logistic regression to the data
glm.fit <- glm(obese ~ weight, family=binomial)
lines(weight, glm.fit$fitted.values)

#Draw ROC and AUC using pROC
roc(obese, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE,col="red", lwd=3,print.auc=TRUE)

#storing the roc calculation in a variable
roc.info <- roc(obese, glm.fit$fitted.values, legacy.axes=TRUE)
str(roc.info)

# Creating a data frame with the true posive percentage "tpp" and false positive percentage "fpp"
roc.df <- data.frame(tpp= roc.info$sensitivities*100, 
                     fpp=(1-roc.info$specificities)*100,
                     thresholds=roc.info$thresholds)

#Cheking at the thresholds between TPP 60% and 80%...
roc.df[roc.df$tpp > 60 & roc.df$tpp < 80,]

#highlighting a partial AUC

#Using percentage 0-1
roc(obese, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, 
    col="red", lwd=4, print.auc=TRUE, print.auc.x=0.4, 
    partial.auc=c(1, 0.7), auc.polygon=TRUE, auc.polygon.col="#377eb822")

#using percentage 100%
roc(obese, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, 
    percent=TRUE, col="red", lwd=4, print.auc=TRUE, print.auc.x=45, 
    partial.auc=c(100, 70), auc.polygon = TRUE, auc.polygon.col = "#377eb822")

 
roc(obese, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, 
    percent=TRUE, col="red", lwd=4, print.auc=TRUE, print.auc.x=45, 
    xlab="False positive percentage", ylab="True postive percentage",
    partial.auc=c(100, 70), auc.polygon = TRUE, auc.polygon.col = "#377eb822")









