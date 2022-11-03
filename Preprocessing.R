library(dplyr)
library(ROSE)
library(cowplot)
library(randomForest)
library(rpart)
library(rpart.plot)

df <- read.csv(file = "./heart_2020_cleaned.csv")

#Reformatting categorical and ordinal data

#Note that the numeric dummy codes are used in the Calculations and Graphs file, but not here.
#df$HeartDisease = ifelse(df$HeartDisease == 'Yes', 1, 0)
df$HeartDisease = as.factor(df$HeartDisease)

#df$Smoking = ifelse(df$Smoking == 'Yes', 1, 0)
df$Smoking = as.factor(df$Smoking)

#df$AlcoholDrinking = ifelse(df$AlcoholDrinking == 'Yes', 1, 0)
df$AlcoholDrinking = as.factor(df$AlcoholDrinking)

#df$Stroke = ifelse(df$Stroke == 'Yes', 1, 0)
df$Stroke = as.factor(df$Stroke)

#df$DiffWalking = ifelse(df$DiffWalking == 'Yes', 1, 0)
df$DiffWalking = as.factor(df$DiffWalking)

df$Sex = as.factor(df$Sex)
df$Female = ifelse(df$Sex == 'Female', 1,0)

df$AgeCategory = factor(df$AgeCategory, levels = c("18-24","25-29","30-34","35-39", "40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79", "80 or older"))
df$Race = as.factor(df$Race)
df$Diabetic = as.factor(df$Diabetic)

#df$PhysicalActivity = ifelse(df$PhysicalActivity == 'Yes', 1, 0)
df$PhysicalActivity = as.factor(df$PhysicalActivity)

df$GenHealth = factor(df$GenHealth, levels = c("Poor","Fair","Good", "Very good", "Excellent"))

#df$Asthma = ifelse(df$Asthma == 'Yes', 1, 0)
df$Asthma = as.factor(df$Asthma)

#df$KidneyDisease = ifelse(df$KidneyDisease == 'Yes', 1, 0)
df$KidneyDisease = as.factor(df$KidneyDisease)

#df$SkinCancer = ifelse(df$SkinCancer == 'Yes', 1, 0)
df$SkinCancer = as.factor(df$SkinCancer)

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

#Dropping Female because Sex gets turned into a dummy
df <- df %>%
  select(-Female)

##############################
#END OF DATA CLEANING #######
##############################



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
summary(data_balanced_over)


#build decision tree model
tree_over <- rpart(HeartDisease ~ ., data = data_balanced_over)

#make predictions on unseen data
pred.tree.over <- predict(tree_over, newdata = test)




#No significant effect of physical activity, likely eclipsed by DiffWalking and PhysicalHealth
#Model without PhysicalActivity

model = glm(HeartDisease ~ . , family = "binomial", data = data_balanced_over)
##summary(model)

#Use this code below to predict probabilities in app, sub in newdata with a single row
#user_prediction = predict(model, type="response", newdata = user_data)

#Saving the model for faster user in subsequent runs of the app.

#####UNCOMMENT THIS LINE WHEN RUNNING THE PROGRAM ON YOUR OWN MACHINE####
saveRDS(model, file = "./model.rda")

#This proves that entering a single row of data yields a single value for probability
single_test_row = data_balanced_over[1,]
single_test_row_output = predict(model, type="response", newdata = single_test_row)
class(single_test_row_output) #numeric output needs to be converted to character


