data <- read.csv("kkbox(1).csv", sep =",")
library(tidyverse)
library(ggpubr)

#train/test split

data1 = sort(sample(nrow(data), nrow(data)*.7))
train <- data[data1,]
test  <- data[-data1,]


#Task 1 

theme_set(theme_pubr())
head(train,4)
ggplot(train, aes(x = city , y= is_churn )) + geom_point() + stat_smooth()

input <- "is_churn ~ age + gender + payment_plan_days + plan_list_price + actual_amount_paid + n_transactions + is_cancel"

glm(formula = input, family = binomial(link ="logit"), data = train)

install.packages("margins")
library("margins")
x <- glm(input, family=binomial (link ='logit'), data=data)
m<- margins(x)
summary(m)

glm(formula = is_churn ~ n_transactions + is_cancel, family = binomial (link = "logit"), data = train)

fitted.results <- predict(x, test, type='response')
fitted.results <- ifelse(fitted.results > 0.1,1,0)
misClasificError <- mean(fitted.results != test$is_churn)
print(1-misClasificError)


#Task 2 clasification tree

install.packages("rpart")
library("rpart")

kkboxtree <- rpart(input, train, method ="class")
printcp(kkboxtree)


my_prediction <- predict(kkboxtree, test, type="class")
tree_error <- mean(my_prediction != test$is_churn)
print(1-tree_error)
