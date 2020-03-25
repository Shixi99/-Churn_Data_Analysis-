library(tidyverse)
library(data.table)
library(caTools)
library(inspectdf)
library(Hmisc)
library(e1071)
library(ROCR)
data <- read_csv('C:/Users/dell/Downloads/Churn_Modelling.csv')
glimpse(data)
data$Exited <- as.factor(data$Exited )

inspect_na(data)
sapply(data,function(x) sum(is.na(x)))
# there is no NA in data


sapply(data, function(x) length(unique(x)))
data %>% glimpse()
df <- data %>% select(-c(1,2,3)) 
df %>% glimpse()
df$Gender <- as.factor(df$Gender)
df$Gender <- as.numeric(df$Gender)
df %>% glimpse()

nrow(df)

# Model 
set.seed(123)
split <- df$Exited %>% sample.split(SplitRatio = 0.8)
train <- df %>% subset(split == T)
test <- df %>% subset(split == F)
nrow(train)

model <- glm(Exited ~.,family=binomial,data=train)
summary(model)

# Analysis of deviance table
anova(model, test="Chisq")

library(pscl)
pR2(model)


# Assessing the predictive ability of the model
fitted.results <- predict(model,newdata = subset(test, select = 1:11),type='response')

fitted.results <- ifelse(fitted.results > 0.5,1,0)
# 
misClasificError <- mean(fitted.results != test$Exited)
print(paste('Accuracy',1-misClasificError))
# ROC
# #install.packages('ROCR')
library(ROCR)
p <- predict(model, newdata = subset(test,select=1:11), type="response")
pr <- prediction(p, test$Exited)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf,colorize=TRUE)


auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


