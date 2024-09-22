pima_indians_diabetes <- read.csv(file.choose())
pima_indians_diabetes
pima_indians_diabetes = pima_indians_diabetes[1:768,1:9]
pima_indians_diabetes
library(stats)
library(dplyr)
pima_indians_diabetes[,1:8] <- replace(pima_indians_diabetes[,1:8],pima_indians_diabetes[,1:8]==0,NA)
pima_indians_diabetes
pima_indians_diabetes <- data.frame(
  sapply(
    pima_indians_diabetes,
    function(x) ifelse(is.na(x),
                       mean(x, na.rm = TRUE),
                       x)))
pima_indians_diabetes
library(stats)
library(dataPreparation)
# Random sample indexes
Accuracy_in_percentage<-numeric(100)
Sensitivity_in_percentage<-numeric(100)
Specificity_in_percentage<-numeric(100)
for (i in 1:100) {
train_index <- sample(1:nrow(pima_indians_diabetes), 0.7 * nrow(pima_indians_diabetes))
test_index <- setdiff(1:nrow(pima_indians_diabetes), train_index)
# Build X_train, y_train, X_test, y_test
x_train <- pima_indians_diabetes[train_index,-9]
y_train <- pima_indians_diabetes[train_index, "ClassVariable"]
x_test <- pima_indians_diabetes[test_index, -9]
y_test <- pima_indians_diabetes[test_index, "ClassVariable"]
## KNN algorithm
library(class)
library(caret)
dat <- cbind(x_train,y_train = as.factor(y_train))
ctrl <- trainControl(method = "cv", number = 10, savePred=T)
# Fitting model
fit <- train(y_train ~ ., data = dat, method = "knn", trControl = ctrl)
print(fit)
summary(fit)
# Predict Output 
predicted_k <- predict(fit,x_test)
table(predicted_k,y_test = as.factor(y_test))
Acc<-table(predicted_k,y_test = as.factor(y_test))
conf_matrix<-table(predicted_k,y_test = as.factor(y_test))
Sensitivity_in_percentage[i]<-sensitivity(conf_matrix)*100
Specificity_in_percentage[i]<-specificity(conf_matrix)*100
Accuracy_in_percentage[i]<- sum(diag(Acc))/sum(Acc)*100 
}
ac<-sum(Accuracy_in_percentage)/100
ac
se<-sum(Sensitivity_in_percentage)/100
se
sp<-sum(Specificity_in_percentage)/100
sp
library(caret)
# Confusion Matrix
confusionMatrix(table(predicted_k,y_test))
