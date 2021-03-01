# calling libraries 
library(plyr) 
library(gmodels)
library(ggplot2)
library(dplyr)
library(scales)
library(corrplot)
library(ROSE)
library(pROC)
library(ROCR)

# reading dataset
data_bp <- read.csv("C:/Users/prath/Desktop/DMML dataset/Bank_marketing/bank prediction.csv")

# check for "unknown" values
colSums(data_bp == "unknown")

# sum of "unknown" values
sum(data_bp == "unknown")

#  replacing unknown with NA 
data_bp[data_bp=="unknown"] <- NA

# removing missing values 
data_bp <- na.omit(data_bp)

# removing columns
data_bp$duration = NULL
data_bp$default = NULL

str(data_bp)

# in column pdays marking the value '999' as  NA's 
levels(data_bp$pdays)[levels(data_bp$pdays)=="999"] <- NA

# removing NA values 
data_bp <- na.omit(data_bp)

# subsituting variables yes with '1' and other than yes as '0'
data_bp$housing <- ifelse(data_bp$housing== "yes", 1, 0)
data_bp$loan<- ifelse(data_bp$loan== "yes", 1, 0)
data_bp$y <- ifelse(data_bp$y== "yes", 1, 0)


summary(data_bp)


# combining similar categories for better model performance 
levels(data_bp$job)[levels(data_bp$job)=="self-employed"] <-"entrepreneur"
levels(data_bp$job)[levels(data_bp$job)=="management"] <-"admin."
levels(data_bp$job)[levels(data_bp$job)=="technician"] <-"blue-collar"
levels(data_bp$job)[levels(data_bp$job)=="retired"] <-"unemployed"
levels(data_bp$job)[levels(data_bp$job)=="housemaid"] <-"services"

levels(data_bp$marital)[levels(data_bp$marital)=="divorced"] <-"single"

levels(data_bp$education)[levels(data_bp$education)=="basic.9y"] <-"basic"
levels(data_bp$education)[levels(data_bp$education)=="basic.6y"] <-"basic"
levels(data_bp$education)[levels(data_bp$education)=="basic.4y"] <-"basic"



# correlation plot 
corr_bp <- select_if(data_bp, is.numeric)   
correlations <- cor(corr_bp,method="pearson")
corrplot(correlations, number.cex = .9, method = "circle", type = "full", tl.cex=0.8,tl.col = "black")



# check for y 
CrossTable(data_bp$y)
str(data_bp)


# dividing the dataset 
set.seed(802641)
set=sample(nrow(data_bp),round(nrow(data_bp)*0.7),replace = FALSE)
d_train=data_bp[set,];
d_test=data_bp[-set,];


#substituting the value in y for no as '0' and for yes as '1'
d_test$y[d_test$y == "no"] <- "0"
d_test$y[d_test$y == "yes"] <- "1"

d_train$y[d_train$y == "no"] <- "0"
d_train$y[d_train$y == "yes"] <- "1"

d_train$y <- as.factor(d_train$y)
d_test$y <- as.factor(d_test$y)


#Applying models
#randomforest
library(randomForest)
rf_model_bp <- randomForest(y ~ .,
                         data = d_train, 
                         ntree = 501,
                         replace = TRUE,
                         nodesize = 9,
                         importance = TRUE); print(rf_model_bp)


prediction_rf_b <- predict(rf_model_bp, d_test)

accuracy_rf <- confusionMatrix(prediction_rf_b, d_test$y)
accuracy_rf


varImpPlot(rf_model_bp)

#SVM
svm_model_bp <- svm(y ~ ., data = d_train)
print(svm_model_bp)
summary(svm_model_bp)

pred_bp <- predict(svm_model_bp, d_test)

accuracy_bp <- confusionMatrix(pred_bp, d_test$y)
accuracy_bp


