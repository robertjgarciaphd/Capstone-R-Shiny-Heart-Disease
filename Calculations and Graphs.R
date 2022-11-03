library(shiny)
library(caret)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(ROSE)
library(cowplot)
library(randomForest)
library(doParallel)
library(rpart)
library(rpart.plot)


df <- read.csv(file = "./heart_2020_cleaned.csv")

#Setting margin = 2 applies function to each column
missing_vals <- apply(df, 2, function(x) any(is.na(x)))
missing_vals
#no missing data


#Checking which columns need to be reformatted
str(df)

#Reformatting categorical and ordinal data

#Creating dummy variables
#df$NativeAmerican = ifelse(df$Race == 'American Indian/Alaskan Native', 1, 0)
#df$Asian = ifelse(df$Race == 'Asian', 1, 0)
#df$Black = ifelse(df$Race == 'Black', 1, 0)
#df$Hispanic = ifelse(df$Race == 'Hispanic', 1, 0)
#df$Other = ifelse(df$Race == 'Other', 1, 0)
#df$White = ifelse(df$Race == 'White', 1, 0)

df$HeartDisease = as.factor(df$HeartDisease)
df$HeartDisease = ifelse(df$HeartDisease == 'Yes', 1, 0)

df$Smoking = as.factor(df$Smoking)
df$Smoking = ifelse(df$Smoking == 'Yes', 1, 0)

df$AlcoholDrinking = as.factor(df$AlcoholDrinking)
df$AlcoholDrinking = ifelse(df$AlcoholDrinking == 'Yes', 1, 0)

df$Stroke = as.factor(df$Stroke)
df$Stroke = ifelse(df$Stroke == 'Yes', 1, 0)

df$DiffWalking = as.factor(df$DiffWalking)
df$DiffWalking = ifelse(df$DiffWalking == 'Yes', 1, 0)

df$Sex = as.factor(df$Sex)
df$Female = ifelse(df$Sex == 'Female', 1,0)

df$AgeCategory = factor(df$AgeCategory, levels = c("18-24","25-29","30-34","35-39", "40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79", "80 or older"))
df$Race = as.factor(df$Race)
df$Diabetic = as.factor(df$Diabetic)
df$PhysicalActivity = as.factor(df$PhysicalActivity)
df$PhysicalActivity = ifelse(df$PhysicalActivity == 'Yes', 1, 0)

df$GenHealth = factor(df$GenHealth, levels = c("Poor","Fair","Good", "Very good", "Excellent"))
df$Asthma = as.factor(df$Asthma)
df$Asthma = ifelse(df$Asthma == 'Yes', 1, 0)

df$KidneyDisease = as.factor(df$KidneyDisease)
df$KidneyDisease = ifelse(df$KidneyDisease == 'Yes', 1, 0)

df$SkinCancer = as.factor(df$SkinCancer)
df$SkinCancer = ifelse(df$SkinCancer == 'Yes', 1, 0)

#RaceOther is not significant, therefore I chose to combine
#it with RaceWhite, which it resembles the most.  
#Also collapse Diabetes during pregnancy into another category.

df$Race <- as.character(df$Race)
df$Race[df$Race == 'Other'] <- "White"
df$Race <- as.factor(df$Race)


df$Diabetic <- as.character(df$Diabetic)
df$Diabetic[df$Diabetic == 'Yes (during pregnancy)'] <- "No"
df$Diabetic <- as.factor(df$Diabetic)

#Dropping PhysicalActivity column because it is not a significant predictor
df <- df %>%
  select(-PhysicalActivity)


##############################
#END OF DATA CLEANING #######
##############################


##############################
#CALCULATIONS ################
##############################

#Point-biserial correlation between binary outcome 
cor.test(df$HeartDisease, df$BMI)
#0.05180319 

cor.test(df$HeartDisease, df$PhysicalHealth)
#0.170721 

cor.test(df$HeartDisease, df$MentalHealth)
#0.02859071 

cor.test(df$HeartDisease, df$SleepTime)
#0.008326647 

cor(df$HeartDisease, df$Smoking)
#0.1077642

cor(df$HeartDisease, df$AlcoholDrinking)
#-0.03207974

cor(df$HeartDisease, df$Stroke)
#0.1968353

cor(df$HeartDisease, df$DiffWalking)
#0.201258

cor(df$HeartDisease, df$Female)
#-0.07004048

#cor(df$HeartDisease, df$PhysicalActivity)
#-0.1000299

cor(df$HeartDisease, df$Asthma)
#0.04144415

cor(df$HeartDisease, df$SkinCancer)
#0.09331688

## Run cor for all binary variables here
cor(df[, c('HeartDisease', 'Smoking', 'AlcoholDrinking', 'Stroke', 'DiffWalking', 'Asthma', 'SkinCancer')])

## Run cor for all continuous vars here
cor(df[, c('SleepTime', 'PhysicalHealth', 'MentalHealth', 'BMI')])

#Compare predictor vars with boxplots across outcome var levels
#Do this for other predictors, look for different distributions rather than significant effects on the regression

boxplot(BMI ~ HeartDisease, data = df, xlab="Heart Disease", ylab="BMI" ) #Similiar distribution
boxplot(Smoking ~ HeartDisease, data = df, xlab="Heart Disease", ylab="Smoking" ) #keep
boxplot(PhysicalHealth ~ HeartDisease, data = df, xlab="Heart Disease", ylab="Physical Health" ) #These are different, lots of outliers on O HeartDisease
boxplot(MentalHealth ~ HeartDisease, data = df, xlab="Heart Disease", ylab="Mental Health" ) #Different upper quartiles
boxplot(SleepTime ~ HeartDisease, data = df, xlab="Heart Disease", ylab="Sleep Time" ) #Similar distribution
#BMI, MentalHealth, and SleepTime all seem unhelpful

# Handle imbalanced data for df$HeartDisease
#https://www.analyticsvidhya.com/blog/2016/03/practical-guide-deal-imbalanced-classification-problems/

#Count category instances to see if any are too small
table(df$AgeCategory)
table(df$Race)

#Even the smallest group, American Indian/Alaskan Native has 5202 observations
table(df$AgeCategory, df$Race)
#Crossing these categories still yields at least 176 in each group

#Start by fitting regression on all predictors
#Check significance of all variables and remove the ones that are below threshhold


##############################
####BUILDING MODEL############
##############################


#Create a training data set from 80% of the total data

#use 80% of dataset as training set and 20% as test set
#sample is a mask
sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.8,0.2))
train  <- df[sample, ]
test   <- df[!sample, ]
#balance the training set, keep test unbalanced

table(train$HeartDisease) 
table(test$HeartDisease)

train %>%
  group_by(HeartDisease) %>%
  summarize(count = n())
# Count of 0: 233377
# Count of 1: 21851


data_balanced_over <- ovun.sample(HeartDisease ~ ., data = train, method = "over",N = 2*233377)$data
table(data_balanced_over$HeartDisease)
#Gives 233852 with no HeartDisease and 232902 with HeartDisease

#ROSE method
data_rose <- ROSE(HeartDisease ~ ., data = train, seed = 1)$data
table(data_rose$HeartDisease)

#build decision tree models
tree_rose <- rpart(HeartDisease ~ ., data = data_rose)
tree_over <- rpart(HeartDisease ~ ., data = data_balanced_over)

#make predictions on unseen data
pred.tree.rose <- predict(tree_rose, newdata = test)
pred.tree.over <- predict(tree_over, newdata = test)

str(pred.tree.rose)

#AUC ROSE
roc.curve(test$HeartDisease, pred.tree.rose)
#ROC for ROSE is .778

#AUC Oversampling
roc.curve(test$HeartDisease, pred.tree.over)
#ROC for oversampling is .789

ROSE.holdout <- ROSE.eval(HeartDisease ~ ., data = data_balanced_over, learner = rpart, method.assess = "holdout", extr.pred = function(obj)obj, seed = 1)
ROSE.holdout
#Holdout estimate of ROSE AUC = .787





#Model with all variables
#model = glm(HeartDisease ~ ., family = "binomial", data = data_balanced_over)
#summary(model)

#No significant effect of physical activity, likely eclipsed by DiffWalking and PhysicalHealth



#Model without PhysicalActivity
model = glm(HeartDisease ~ . , family = "binomial", data = data_balanced_over)
summary(model)


#McFadden pseudo R^2 = .288
1 - model$deviance/model$null.deviance

#Confusion matrix

confusion_matrix = table(data_balanced_over$HeartDisease, round(model$fitted.values))

#Accuracy score = .76
sum(diag(confusion_matrix))/nrow(data_balanced_over)

confusion_matrix

library(dplyr)
data_balanced_over %>%
  group_by(HeartDisease) %>%
  summarize (ratio = n()/nrow(data_balanced_over))

#Plugging in the test data to see how the model performs
predicted_over = predict(model, newdata = test)

test_predictions_prob = predict(model, type="response", newdata = test)
test_predictions_prob

#Converting probabilities to discrete binary outcomes
test_predictions = predict(model, type="response", newdata = test)>=0.5

str(test)
str(test_predictions)

#Comparing predictions with actual outcomes
test_confusion_matrix = table(test$HeartDisease, test_predictions)
test_confusion_matrix

#Accuracy, total correct predictions over total predictions: 0.75
sum(diag(test_confusion_matrix))/nrow(test)

#Precision, proportion of positive identifications model got correct: 0.22
test_confusion_matrix[2,2]/sum(test_confusion_matrix[,2])

#Recall, proportion of actual positives model identified correctly: 0.78
test_confusion_matrix[2,2]/sum(test_confusion_matrix[2,])







#No significant difference from a saturated model
pchisq(model$deviance, model$df.residual, lower.tail = F)









######Random Forest attempts##########




#rf_model <- randomForest(HeartDisease ~ ., data = data_balanced_over, proximity = TRUE)
#rf_model

#############

#model = glm(HeartDisease ~ ., family = "binomial", data = train)
#summary(model)

#Need help plotting logistic regression

#Trying to run random forests in parallel

#cl<-makePSOCKcluster(5)

#registerDoParallel(cl)

#model<-train(HeartDisease~., data=data_balanced_over, method='rf')

#stopCluster(cl)

#Random forest only works on dataframe up to 15k rows or about 5% of the total data set
minisample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.95,0.05))
minitest   <- df[!minisample, ]

table(minitest$HeartDisease)
#rf_model_test <- randomForest(HeartDisease ~ ., data = minitest, proximity = TRUE)

fit <- rpart(HeartDisease ~ ., data= train, method='anova')
rpart.plot(fit)

printcp(fit)