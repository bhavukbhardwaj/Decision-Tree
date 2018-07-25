
#load the data into R
diabet<-read.csv("Diabetes.csv")
View(diabet)

#divide the data into Training and Testing datasets
#instead of using library caTools and splitting the data, we can split data by
set.seed(3)
id<-sample(2,nrow(diabet),prob = c(0.7,0.3),replace = TRUE)
diabet_train<-diabet[id==1,]
diabet_test<-diabet[id==2,]


#Building Decision Tree
#For rpart() we need to load rpart library
library(rpart)

colnames(diabet)

diabet_model<-rpart(is_diabetic~.,data = diabet_train)
#Here we are using all columns for the model

diabet_model

#We can plot it as

plot(diabet_model,margin = 0.1)
#margin is used to adjust the size of the plot, For viewing labels
text(diabet_model,use.n = TRUE,pretty = TRUE,cex =0.8)

#create subset and verify
temp<-diabet_train[diabet_train$glucose_conc<154.5 & diabet_train$BMI<26.35,]
table(temp$is_diabetic)

#Prediction of test dataset

pred_diabet<-predict(diabet_model,newdata = diabet_test,type = "class")
pred_diabet

#Now we need to compare it with actual values
table(pred_diabet,diabet_test$is_diabetic)

#For creating confusion matrix we can use the following
library(caret)
confusionMatrix(table(pred_diabet,diabet_test$is_diabetic))

#Random Forest
library(randomForest)
diabet_forest<-randomForest(is_diabetic~.,data = diabet_train)
diabet_forest

#Prediction of Test set

pred1_diabet<-predict(diabet_forest,newdata = diabet_test,type = "class")
pred1_diabet

#confusion matrix
library(caret)
confusionMatrix(table(pred1_diabet,diabet_test$is_diabetic))


# Naive Bayes Classifier
library(e1071)
diabet_naive<-naiveBayes(is_diabetic~.,data = diabet_train)
diabet_naive

#Prediction of Test set

pred2_diabet<-predict(diabet_naive,newdata = diabet_test,type = "class")
pred2_diabet

#Confusion matrix
confusionMatrix(table(pred2_diabet,diabet_test$is_diabetic))
