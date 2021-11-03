##########################################
########### Learn by Doing it.############
##########################################
################## Goal ##################
##########################################
# The goal here is to model and predict if a given specimen (row in dataset) is benign or malignant,
# based on 9 other cell features.

#install.packages("mlbench")
data(BreastCancer, package="mlbench")
bc <- BreastCancer[complete.cases(BreastCancer), ]  # create copy(having no missing value)

# 1. look into data structure
str(bc)
# 2. apply needed transformation
#remove id column
bc <- bc[,-1]
#convert to numeric
for (i in 1:9) {
     bc[,i] <- as.numeric(as.character(bc[,i]))
}
#Change Y values to 1's and 0's
bc$Class <- ifelse(bc$Class == "malignant",1,0)
bc$Class <- factor(bc$Class , levels=c(0,1))
# 3. Can you handle class imbalancing? 
table(bc$Class)
#>   benign malignant 
#>      444       239
#yes
# Both classes should have equal numbers of records (or nearly equal)
library(caret)
# 4. Prep Training and Test data.
'%ni%' <- Negate('%in%') #define "not" in func
options(scipen = 999)#prevent printing scientific notations.
set.seed(100)
trainingIndex <- createDataPartition(bc$Class,p = 0.7,list = F)
trainingdata <- bc[trainingIndex,]
testingdata <- bc[-trainingIndex,]
#class distribution of train data
table(trainingdata$Class)
#Down Sample
set.seed(100)
Down_Train <- downSample(x = trainingdata[,colnames(trainingdata) %ni% "Class"],
                         y = trainingdata$Class)
table(Down_Train$Class)
#Up Sample (Optional)
Up_Train <- upSample(x = trainingdata[,colnames(trainingdata) %ni% "Class"],
                     y = trainingdata$Class)
table(Up_Train$Class)
# 5. Building the Logistic Regression Model
logitModel <- glm(Class ~ Cl.thickness + Cell.size + Cell.shape,family = "binomial" ,data = Down_Train)
summary(logitModel)
# 6. Predict on Test Dataset
Pred <- predict(logitModel, newdata = testingdata,type = "response")
Pred
#Recode factors
y_Pred_num <- ifelse(Pred > 0.5 ,1,0)
y_Pred <- factor(y_Pred_num,levels = c(0,1))
y_act <- testingdata$Class
# 7. Check Accuracy
mean(y_Pred == y_act)
#95%

