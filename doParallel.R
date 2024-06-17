library(caret)
library("tidyverse")
install.packages("randomForest")
install.packages("xgboost")
library(xgboost)
library(randomForest)

data <- read.csv("insurance_data(1).csv")

#remove zero variance predictors
remove_cols <- nearZeroVar(data[,-86], names = TRUE)

data <-data %>% select(-one_of(remove_cols))

data<-data %>% mutate_at(c(1,5), as.factor)


#create dummy variables expect for the response
dummies_model <- dummyVars(response~ ., data = data)
#if the response is a factor may get a warning that you can ignore


#provide only predictors that are now converted to dummy variables
predictors_dummy<- data.frame(predict(dummies_model, newdata = data)) 

#recombine predictors including dummy variables with response
data <- cbind(response=data$response, predictors_dummy) 

#convert response to factor
data$response <- as.factor(data$response)

#Rename the response from 0, 1 
data$response <- fct_recode(data$response,
                            buy="1",
                            notbuy= "0")
data$response <- relevel(data$response,
                         ref="buy")

library(caret)

set.seed(99) #set random seed
index <- createDataPartition(data$response, p = .8,list = FALSE)
data_train <-data[index,]
data_test<- data[-index,]

install.packages("doParallel")
library(doParallel)

#total number of cores on your computer
num_cores<-detectCores(logical=FALSE)
num_cores

#[1] 2


#start parallel processing
cl <- makePSOCKcluster(num_cores-2)
registerDoParallel(cl)


set.seed(12)
start_time <- Sys.time()
model_gbm <- train(response ~.,
                   data = data_train,
                   method="xgbTree",
                   tuneGrid = expand.grid(
                     nrounds = c(50,200),
                     eta = c(0.025, 0.05),
                     max_depth = c(2, 3),
                     gamma = 0,
                     colsample_bytree = 1,
                     min_child_weight = 1,
                     subsample = 1),
                   trControl = trainControl(method = "cv",
                                            number=5,
                                            classProbs=TRUE,
                                            summaryFunction = twoClassSummary),
                   metric = "ROC")
end_time <- Sys.time()
stopCluster(cl)
registerDoSEQ()
print(end_time - start_time)

plot (varImp(model_gbm))
varImp(model_gbm)
