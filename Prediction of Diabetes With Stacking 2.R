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
  ## SVM algorithm
  library(e1071)
  library(dplyr)
  library(caret)
  dat <- cbind(x_train,y_train = as.factor(y_train))
  ctrl <- trainControl(method = "cv", number = 10, savePred=T)
  # Fitting model
  fit <- train(y_train ~ ., data = dat, method = "svmLinear", cost = 4, gamma = 0.5, trControl = ctrl)
  print(fit)
  summary(fit)
  # Predict Output 
  predicted_s<- predict(fit,x_test)
  table(predicted_s,y_test = as.factor(y_test))
  library(caret)
  # Confusion Matrix
  confusionMatrix(table(predicted_s,y_test))
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
  library(caret)
  # Confusion Matrix
  confusionMatrix(table(predicted_k,y_test))
  ## Decision Tree algorithm
  library(rpart)
  library(caret)
  dat <- cbind(x_train,y_train = as.factor(y_train))
  ctrl <- trainControl(method = "cv", number = 10, savePred=T)
  # grow tree 
  fit <- train(y_train ~ ., data = dat, method="rpart", trControl = ctrl)
  print(fit)
  summary(fit)
  # Predict Output 
  predicted_d = predict(fit,x_test)
  table(predicted_d,y_test = as.factor(y_test))
  library(caret)
  # Confusion Matrix
  confusionMatrix(table(predicted_d,y_test))
  ## Naive Bayesian algorithm
  library(e1071)
  library(caret)
  dat <- cbind(x_train,y_train = as.factor(y_train))
  ctrl <- trainControl(method = "cv", number = 10, savePred=T)
  # Fitting model
  fit <- train(y_train ~ ., data = dat, method = "nb", trControl = ctrl)
  print(fit)
  summary(fit)
  # Predict Output 
  predicted_n = predict(fit,x_test)
  table(predicted_n,y_test = as.factor(y_test))
  library(caret)
  # Confusion Matrix
  confusionMatrix(table(predicted_n,y_test))
  ## Stacking algorithm
  library(e1071)
  library(class)
  library(rpart)
  pima_indians_diabetes_new <- cbind(x_test,predicted_n,predicted_k,predicted_d,y_test)
  library(stats)
  library(dataPreparation)
  # Random sample indexes
  train_index_new <- sample(1:nrow(pima_indians_diabetes_new), 0.7 * nrow(pima_indians_diabetes_new))
  test_index_new <- setdiff(1:nrow(pima_indians_diabetes_new), train_index_new)
  # Build X_train, y_train, X_test, y_test
  x_train_new <- pima_indians_diabetes_new[train_index_new,-12]
  y_train_new <- pima_indians_diabetes_new[train_index_new,"y_test"]
  x_test_new <- pima_indians_diabetes_new[test_index_new, -12]
  y_test_new <- pima_indians_diabetes_new[test_index_new,"y_test"]
  ## SVM algorithm   
  library(e1071)
  library(dplyr)
  dat_new <- cbind(x_train_new,y_train_new = as.factor(y_train_new))
  ctrl <- trainControl(method = "cv", number = 10, savePred=T)
  # Fitting model
  fit <- train(y_train_new ~ ., data = dat_new, method = "svmLinear", cost = 4, gamma = 0.5, trControl = ctrl)
  print(fit)
  summary(fit)
  # Predict Output 
  predicted_new = predict(fit,x_test_new)
  table(predicted_new,y_test_new = as.factor(y_test_new))
  Acc<-table(predicted_new,y_test_new = as.factor(y_test_new))
  conf_matrix<-table(predicted_new,y_test_new = as.factor(y_test_new))
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
confusionMatrix(table(predicted_new,y_test_new))

