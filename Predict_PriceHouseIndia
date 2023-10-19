library(tidyverse)
library(caret)
library(readxl)

House_Price_India <- read_excel("House Price India.xlsx")
View(House_Price_India)

# Preview data
hist(House_Price_India$Price)

#Log Tranformation
House_Price_India$logPrice <- log(House_Price_India$Price+1)

# Preview after log
hist(House_Price_India$Price)

# 0. Prep Data
HPI <- House_Price_India %>%
          select('number of bedrooms','living area','lot area','Built Year','logPrice')

# 1. Split data
set.seed(26)
n <- nrow(HPI)
id <- sample(1:n, size = 0.8*n)
train_data <- HPI[id, ]
test_data <- HPI[-id, ]

# Train data
lmModel <- train(logPrice ~ .,
                 data = train_data,
                 method = 'lm')

# Test data
p <- predict(lmModel, newdata = test_data)

# Evaluate 
mae <- mean(abs(test_data$logPrice-p))
RMSE <- sqrt(mean((test_data$logPrice-p)^2))

# Preview Result 
cat('Train Set\n','mae : ',lmModel$results[[4]],'\nRMSE : ',lmModel$results[[2]],
'\nTest Set\n','mae : ',mean(abs(test_data$logPrice-p)),'\nRMSE : ',sqrt(mean((test_data$logPrice-p)^2)),
'\nSum-Up : The model fits well with unseen data,',
'\nthe different mae between predicted and actual equal to : ',(lmModel$results[[4]]-mean(abs(test_data$logPrice-p))),
'\nthe different RMSE between predicted and actual equal to : ',(lmModel$results[[2]]-sqrt(mean((test_data$logPrice-p)^2))))


