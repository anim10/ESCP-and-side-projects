library(MASS)
library(dplyr)
library(caret)
library(pROC)
getwd()
data <- read.csv("data for problem B.csv", header = T)
sum(is.na(data))
Num_Var <- scale(data[,c(3,6,9,14)])
data[,c(3,6,9,14)] <- NULL
str(data)


data_appended <- cbind(data, Num_Var)
str(data_appended)

# No missing values in entire dataset 

# Test/Train split 

train <- data_appended[1:700,]
test <- data_appended[701:1000,]

length(train$checkingstatus1)
str(train)

# LOGISTIC REGRESSION 

model <- glm(train$Default ~ ., family = binomial,
             data = train) %>%
  stepAIC(trace = FALSE)

model_nonAIC <- glm(train$Default ~ ., family = binomial,
                    data = train) 
model_nonAIC$aic-model$aic

# AIC reduced by 13.50033

variables <- as.data.frame(list(model$coefficients))
variable_nonaic <- as.data.frame((list(model_nonAIC$coefficients)))

nrow(variable_nonaic)-nrow(variables)


# To summarize, the AIC "optimised" model has higher fit 

prob <- model %>% predict(test, type="response")
prob
test$Default

ROC_Curve<- roc(test$Default, prob, plot=TRUE, legacy.axes=TRUE ,out = FALSE)
plot(ROC_Curve, print.thres="best", col="#377eb8", lwd=4, print.auc=TRUE)

# Logistic Regression proves to be a good model with AUC: 0.775 

Prob <- as.data.frame(prob)
Classification <- ifelse(prob > 0.068,1,0)
Classification <- as.factor(Classification)
Test_ref <- as.factor(test$Default)
ConfX <- confusionMatrix(Classification, reference = Test_ref, positive = '1')

ConfX

# Random Forest 

library(randomForest)

Model_RM <- randomForest(factor(train$Default) ~ ., data= train)
Model_RM$votes[,1]

Model_RM2 <- randomForest(train$Default ~ ., mtry = 2 ,data= train, importance=TRUE)
Model_RM2

pred_RM <- predict(Model_RM, test, type= "prob")
pred_RM

ROC_RF<- roc(test$Default, pred_RM[,1], percent = T, col="#4daf4a", lwd=4)
plot(ROC_RF,  print.thres="best", col="#4daf4a", lwd=4, print.auc=TRUE)

# We see that using Random Forest only decreases the AUC marginally. We go from 0.775 using 
# Logistic Regression to 0.773 using Random Forest. Because both are equally easy to interpret, 
# and that they are more or less computationally equal (atleast on a dataset of this size)
# I would chose Logistic Regression as my Model 







