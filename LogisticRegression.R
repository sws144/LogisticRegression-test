
#Logistic Regression

packages <- c("caret", "pROC")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
library(caret)

#input folder - UPDATE
setwd('C:/Stuff/Important/CareerNCollege/Ad Hoc/Price-Elasticity-of-Demand/') 

training.data.raw <- read.csv('TrainingData.csv',header=T,na.strings=c(""))
plot(training.data.raw)
training.data.transformed = data.frame(QuotedRateFactorLog = log(training.data.raw$QuotedRateFactor))
training.data.transformed$CompetitivePositionLog = log(training.data.raw$CompetitivePosition)
training.data.transformed$Renewed = training.data.raw$Renewed
model <- glm(Renewed ~.,family=binomial(link='logit'),data=training.data.transformed)
summary(model)

#confusion matrix
probs = predict(model, newdata=training.data.transformed, type = "response")
accuracy <- table(probs, training.data.transformed[,"Renewed"])

pred <- ifelse(probs > 0.5,1,0)
misClasificError <- mean(pred != training.data.transformed[,"Renewed"])

print(paste('Accuracy',1-misClasificError))

confusionMatrix(factor(pred), factor(training.data.transformed[,"Renewed"]))


#roc
library(pROC)
g <- roc(Renewed ~ probs, data = training.data.transformed)
plot(g)  
auc(g)

