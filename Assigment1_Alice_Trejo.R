install.packages("skimr")
library(skimr)
library(tidyverse)
library(readr)


insurance_data <- read_csv("insurance_data(1).csv")

summary(insurance_data)
table(insurance_data$response)


#showing the distribution of the response variable.
barplot(table(insurance_data$response), main="Distribution of Insurance Purchased", xlab="Insurance Purchased", ylab="Frequency")

#number of observations (rows=8840) and predictor variables (columns=85, since one of them is the response variable)
dim(insurance_data)

#for missing values
sum(is.na(insurance_data))

#for null values
sum(is.null(insurance_data))


#Explore both continuous and categorical variables. Provide 3 interesting plots.

plot(insurance_data$num_life ~ insurance_data$hhsize, main="Scatter Plot of Number of Life Insurance by Household Size", xlab="Household Size", ylab="Number of Life Insurance")

boxplot(insurance_data$hhsize ~ insurance_data$perc_avgincome, main="Boxplot of Household Size by Percentage of Average Income", xlab="Percentage of Average Income", ylab="Household Size")

boxplot(insurance_data$num_houses ~ insurance_data$age, main="Boxplot of Number of Houses by Age", xlab="Age", ylab="Number of Houses")



#
two_way <- table(insurance_data$response, insurance_data$perc_avgincome)
print(two_way)

freq_table2<- as.data.frame(two_way)

names(freq_table2)[names(freq_table2) == "Var1"] <- "response"
names(freq_table2)[names(freq_table2) == "Var2"] <- "perc_avgincome"


boxplot(insurance_data$num_houses ~ insurance_data$response, main="Boxplot of Number of Houses by  Insurance policy purchased", xlab=" Insurance policy purchased (1) or not (0) ", ylab="Number of Houses")


#####################################################

housing_data <- read_csv("housing(1).csv")
skim(housing_data)

num_data <- sapply(housing_data, is.numeric)  
numeric_housing <- housing_data[, num_data]

summary_housing <- data.frame(
  Min = apply(numeric_housing, 2, min),
  SD = apply(numeric_housing, 2, sd), 
  Q1 = apply(numeric_housing, 2, function(x) quantile(x, 0.25)),
  Median = apply(numeric_housing, 2, median),
  Mean = apply(numeric_housing, 2, mean),
  Q3 = apply(numeric_housing, 2, function(x) quantile(x, 0.75)),
  Max = apply(numeric_housing, 2, max)
)

summary_housing$Numeric <- "Yes"
print(summary_housing)


#for missing values
sum(is.na(housing_data))

#for null values
sum(is.null(housing_data))

hist(housing_data$rm,prob=T, main="Histogram and Density of average number of rooms per dwelling", xlab="average number of rooms per dwelling")
lines(density(housing_data$rm), lwd=2)
abline(v=mean(housing_data$rm), lty=2, lwd=1.5)

hist(housing_data$indus,prob=T, main="Histogram and Density of proportion of non-retail business acres per town", xlab="proportion of non-retail business acres per town")
lines(density(housing_data$indus), lwd=2)
abline(v=mean(housing_data$indus), lty=2, lwd=1.5)


hist(housing_data$crim, main="crim Histogram", xlab="crim", col="grey")

hist(housing_data$zn, main="zn Histogram", xlab="zn", col="grey")

hist(housing_data$indus, main="indus Histogram", xlab="indus", col="grey")

hist(housing_data$nox, main="nox Histogram", xlab="nox", col="grey")

hist(housing_data$rm, main="rm Histogram", xlab="rm", col="grey")

hist(housing_data$age, main="age Histogram", xlab="age", col="grey")

hist(housing_data$dis, main="dis Histogram", xlab="dis", col="grey")

hist(housing_data$rad, main="rad Histogram", xlab="rad", col="grey")

hist(housing_data$tax, main="tax Histogram", xlab="tax", col="grey")

hist(housing_data$ptratio, main="ptratio Histogram", xlab="ptratio", col="grey")

hist(housing_data$lstat, main="lstat Histogram", xlab="lstat", col="grey")

hist(housing_data$medv, main="medv Histogram", xlab="medv", col="grey")

##################################
install.packages("GGally")
library(GGally)

boxplot(housing_data$medv ~ housing_data$chas, main="Boxplot of median value of owner-occupied homes by Charles River", xlab="(1 if tract bounds river; 0 otherwise)", ylab="median value of owner-occupied homes")
##################################


new_housing <- housing_data[, !(names(housing_data) %in% "chas")]
ggpairs(new_housing[,1:12])

pairs(new_housing, main="Scatterplot Matrix", pch=12)


################################

library(caret)

set.seed(16) #set random seed
index <- createDataPartition(housing_data$medv, p = .8,list = FALSE)
h_train <-housing_data[index,]
h_test <- housing_data[-index,]

housing_model <- train(medv ~ .,
                      data = h_train,
                      method = "lm",
                      trControl =trainControl(method = "none"))

housing_predictions<-predict(housing_model, h_test)

mean((housing_predictions - h_test$medv)^2) 
