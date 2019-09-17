#Load Libraries
#Load Data
#Ask the necessary Questions

library(tidyverse)
library(tidyr)
library(dplyr)
library(caret)


customers <- read.csv("data/WA_Fn-UseC_-Telco-Customer-Churn.csv")

summary(customers)
glimpse(customers)
dim(customers)
names(customers)
View(customers)


#Predict which customers are likey to churn/those that are likey to stop using our services

#But first, do Exploratory Data Analysis

install.packages("funModeling")
install.packages("Hmisc")

library(funModeling)
library(Hmisc)

#Analyzing Categorical Variables

freq(customers)
freq(data, path_out = "output/categorical")  #explort the plots to jpeg

#Analyzing Numerical Values

plot_num(customers)
plot_num(data, path_out = ".") #export the plots to jpeg

#Analyze all numerical/integervariables automatically
#Quantitavie analysis


customers_prof <- profiling_num(customers)

#Try to describe each variable based on its distribution (also useful for reporting)
#Pay attention to variables with high standard deviation.
#Select the metrics that you are most familiar with: customers_prof %>% select(variable, variation_coef, range_98): A high value in variation_coef may indictate outliers. range_98 indicates where most of the values are.

#Analyzing numerical and categorical at the same time
#use describe from the Hmisc package

describe(customers)

#Check min and max values (outliers)
#Check Distributions (same as before


#Build a predictive model (Insample errors)
#fit a model into the customers data

model <- lm(MonthlyCharges ~ TotalCharges, customers[1:20, ])

#predict in-sample

predicted <- predict(model, customers[1:20, ], type = "response")

#Calculate the RMSE Root Mean Square

actual <- customers[1:20, "MonthlyCharges"]
sqrt(mean((predicted- actual) ^ 2))

#Another Approach
# Fit lm model: model

model1 <- lm(MonthlyCharges ~ ., customers)

# Predict on full data: p
predicta <- predict(model1, customers)

# Compute errors: error
error <- predicta - customers$MonthlyCharges

# Calculate RMSE
sqrt(mean(error ^2))


#Out of Sample Errors (No overfitting)

#Fit a model on the first 20 rows

model2 <- lm(MonthlyCharges ~ TotalCharges, customers[1:20, ])

predcita1 <- predict(model2, customers[21:7043, ], type = "response")

actual1 <- customers[21:7043, "MonthlyCharges"]

sqrt(mean((predcita1 - actual1) ^2))


#Let's give it another shot

#randomly order the data first

set.seed(123)

new_data <- na.omit(customers)

#Shuffle the data row indices
rows <- sample(nrow(new_data))

#randomly order the data
shuffled_customers <- new_data[rows, ]

#do a 80/20 split

split <- round(nrow(new_data) * 80)

#create training set
train <- new_data[1:split, ]

#create the test data
test <- new_data[(split +1): nrow(new_data), ]

#fit model into the training set
model_cust <- lm(tenure ~ ., train)

#predict on the test data

pred <- predict(model_cust, test)

#Calculate the RMSE by hand

error_cust <- pred - test$tenure

sqrt(mean(error_cust ^ 2))

#LET'S TRY CROSS VALIDATION

#split data into 10 folds
#then fit final model on full dataset


set.seed(123)



#fit linear regression model

model3 <- train(
  MonthlyCharges ~ . ,new_data, 
  method = "lm",
  trControl = trainControl(
    method = "cv", 
    number = 10,
    repeats = 10,
    verboseIter = TRUE
  )
)

model3


#Make Predictions on new data
