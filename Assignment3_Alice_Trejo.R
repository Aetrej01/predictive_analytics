#Assignment 3 - Alice Trejo

library(tidyverse)
library(readr)
library(ggplot2)
library(GGally)
library(skimr)
library(caret)

install.packages("MASS")
library(MASS)

install.packages("e1071")
install.packages("Matrix")
install.packages("glmnet")
library(e1071)
library(glmnet)
library(Matrix)

install.packages("ROCR")
library(ROCR)


loan_data <- read_csv("loan_default.csv")

loan_data$Default <- as.factor(loan_data$Default)
loan_data$Sex <- as.factor(loan_data$Sex)
loan_data$Marital_status <- as.factor(loan_data$Marital_status)
loan_data$Car_loan <- as.factor(loan_data$Car_loan)
loan_data$Personal_loan <- as.factor(loan_data$Personal_loan)
loan_data$Home_loan <- as.factor(loan_data$Home_loan)
loan_data$Education_loan <- as.factor(loan_data$Education_loan)
loan_data$Emp_status <- as.factor(loan_data$Emp_status)

#number of observations (rows=1000) and predictor variables (columns=15, since one of them is the response variable)
dim(loan_data)

#for missing values
sum(is.na(loan_data))

#for null values
sum(is.null(loan_data))

#summary from the skim function in R
summary_loan<-skim(loan_data)
summary_loan

# Count of 1s
sum(loan_data$Default == 1)

# Count of 0s
sum(loan_data$Default == 0)


#Provide 2 interesting bloxplots.
boxplot(loan_data$Saving_amount ~ loan_data$Default, main="Boxplot of Saving Amount by Default", xlab="Default", ylab="Saving Amount")

boxplot(loan_data$Credit_score ~ loan_data$Default, main="Boxplot of Credit Score by Default", xlab="Default", ylab="Credit Score")

###################################################PART 2

#STEP 1

loan_data$Default <- fct_recode(loan_data$Default, Default = "1",NotDefault = "0")

loan_data$Default <-relevel(loan_data$Default, ref = "Default")
levels(loan_data$Default)

loan_data_predictors_dummy <- model.matrix(Default~ ., data = loan_data) #create dummy variables expect for the response
loan_data_predictors_dummy<- data.frame(loan_data_predictors_dummy[,-1]) #get rid of intercept
loan_dataaa <- cbind(Default=loan_data$Default, loan_data_predictors_dummy)


set.seed(99) 
Index <- createDataPartition(loan_data$Default, p = 0.8, list = FALSE)
data_train <- loan_data[ Index,]
data_test  <- loan_data[-Index,]



######################################################FORWARD

set.seed(10) # set the seed again since within the train method the validation set is randomly selected
loan_model_for <- train(Default ~ .,
                    data = data_train,
                    method = "glmStepAIC",
                    direction="forward", # change this to 'backward'
                    trControl =trainControl(method = "none",
                                            classProbs = TRUE),
                    metric="ROC")


# list coefficients selected
coef(loan_model_for$finalModel)

# First, get the predicted probabilities of the test data.
predprob_forward <- predict(loan_model_for , data_test, type="prob")

MSE <- mean((as.numeric(predprob_forward[, "Default"]) - as.numeric(data_test$Default))^2)
MSE

pred_forward <- prediction(predprob_forward$Default, data_test$Default, label.ordering =c("NotDefault","Default"))
perf_forward <- performance(pred_forward, "tpr", "fpr")
plot(perf_forward, colorize=TRUE)

auc_forward <- unlist(slot(performance(pred_forward, "auc"), "y.values"))
auc_forward


######################################################BACKWARD

set.seed(10)#set the seed again since within the train method the validation set is randomly selected
loan_model <- train(Default ~ .,
                      data = data_train,
                      method = "glmStepAIC",
                      direction="backward",
                      trControl =trainControl(method = "none",
                                              classProbs = TRUE),
                      metric="ROC")
#list coefficients selected
coef(loan_model$finalModel)

#First, get the predicted probabilities of the test data.
predprob_back<-predict(loan_model , data_test, type="prob")

MSE<-mean((as.numeric(predprob_back[, "Default"]) - as.numeric(data_test$Default))^2)
MSE

pred_back <- prediction(predprob_back$Default, data_test$Default,label.ordering =c("NotDefault","Default") )
perf_back <- performance(pred_back, "tpr", "fpr")
plot(perf_back, colorize=TRUE)

auc_back<-unlist(slot(performance(pred_back, "auc"), "y.values"))
auc_back

######################################################LASSO

set.seed(10)#set the seed again since within the train method the validation set is randomly selected
LASSO_model <- train(Default ~ .,
                      data = data_train,
                      method = "glmnet",
                      standardize =T,
                      tuneGrid = expand.grid(alpha =1, #lasso
                                             lambda = seq(0.0001, 1, length = 20)),
                      trControl =trainControl(method = "cv",
                                              number = 5,
                                              classProbs = TRUE,
                                              summaryFunction = twoClassSummary),
                      metric="ROC")
LASSO_model    

#list coefficients selected
coef(LASSO_model$finalModel, LASSO_model$bestTune$lambda)

#First, get the predicted probabilities of the test data.
predprob_lasso<-predict(LASSO_model , data_test, type="prob")

pred_lasso <- prediction(predprob_lasso$Default, data_test$Default,label.ordering =c("NotDefault","Default") )
perf_lasso <- performance(pred_lasso, "tpr", "fpr")
plot(perf_lasso, colorize=TRUE)

#Get the AUC
auc_lasso<-unlist(slot(performance(pred_lasso, "auc"), "y.values"))
auc_lasso