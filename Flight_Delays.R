


library(caret)
library(tidyverse)
options(scipen=99)

#step 0 eda

flight_data<-read.csv("flight_delays.csv", header=T)
library(skimr)
skim(flight_data)

flight_data<-flight_data %>% mutate_at("carrier", as.factor)

table(flight_data$Carrier)

















set.seed(10)
subset_model<-train(Arr_Delay ~ .,
                    data= flight_train,
                    method="glmStepAIC",
                    direction="backward",
                    trControl = trainControl(method="none"))

coef()

















