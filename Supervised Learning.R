####################Mandatory Concepts of Statistics#########################
#####MEAN#####
x <- 1:5
mean(x)
#####MEDIAN####
x <- c(15000:30000,100000)
median(x)
######MODE#####
x <- c(1,2,3,4,5,2,3,3,3,5,5,7)
table(x)
which.max(table(x))
x <- c(1:5)
########RANGE######
x <-c(1,2,3,4,5)
max(x,na.rm = TRUE) - min(x,na.rm = TRUE)
#########INTERQUARTILE-RANGE########
x <- c(1,2,3,4,5)
IQR(x)
x <-c(1,4,3,4,5,6)
IQR(x)
#####VARIANCE####
var(x)
#######STANDARD DEVIATIoN######
x <- c(1:5)
sd(x)
########COVARIANCE#######
x <- c(1:5)
y <- c(6,7,8,9,10)#no. of counts of x and y must be equal.
cov(x,y)
########CORRELATION######
cor(x,y)
datasets::iris
x <- (iris)
x <- x[-c(5)]
x[is.na(x)] <- 0
install.packages("corrplot")
library(corrplot)
M <- cor(x)
M
corrplot(M)
##########################################################
###################Regression Analysis####################
##########################################################
head(cars)#display the first values of the dataset
dim(cars)#this shows rows and columns of the dataset
#Graphical Analysis 
#Scatter plot
scatter.smooth(x = cars$speed,y = cars$dist,main = "Dist ~ Speed")
#Boxplot
par(mfrow = c(1,2))#divide graph area in two columns
boxplot(cars$speed,main = "Speed",sub = paste("Outlier rows :",boxplot.stats(cars$speed)$out))#boxplot for speed
boxplot(cars$dist,main = "Distance",sub = paste("Outlier rows : ",boxplot.stats(cars$dist)$out))#boxplot for distance
#Density plot
#check if the response variable is close to normally
#install.packages("e1071")
library(e1071)
par(mfrow = c(1,2))#divide graph area in two columns
plot(density(cars$speed),main = "Density Plot : Speed",ylab = "Frequency",sub = paste("Skewness:",round(e1071::skewness(cars$speed),2)))#density plot for speed
polygon(density(cars$speed),col = "green")
plot(density(cars$dist),main = "Density Plot : Distance",ylab = "Frequency",sub = paste("Skewness:",round(e1071::skewness(cars$dist),2)))#density plot for dist
polygon(density(cars$dist),col = "green")
#Build linear model
linearMod <- lm(dist ~ speed,data = cars)#BUILD LINEAAR REGRESSION MODEL ON FULL DATA
print(linearMod)
#Analysis of Model Output
summary(linearMod)
###############################################
###################Step 1######################
###############################################
#Create training and test data:-
set.seed(100)#Setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(cars),0.8 * nrow(cars))#row indices for training data
trainingData <- cars[trainingRowIndex,]#model training data
testData <- cars[-trainingRowIndex,]#test data
###############################################
###################Step 2######################
###############################################
#Build the model on training data:-
lmMode <- lm(dist ~ speed,data = trainingData)#build the model
###############################################
###################Step 3######################
###############################################
#Review diagnostic measures:-
summary(lmMode)#model summary
AIC(lmMode)#Calculate akaika information criterion
BIC(lmMode)
###############################################
###################Step 4######################
###############################################
#Compute Prediction + calculate Prediction accuracy and error rates
distPred <- predict(lmMode,testData)#predict distance
actual_preds <- data.frame(cbind(actuals = testData$dist,predicteds = distPred))#make actual prediction dataframe.
correlation_accuracy <- cor(actual_preds)
correlation_accuracy
head(actual_preds)
plot(actual_preds)
#Other Metric and MeanAbsolutePersentageError
mape <- mean(abs((actual_preds$predicteds - actual_preds$actuals)) / actual_preds$actuals)
mape
########################################################
####################Logistic Regression#################
########################################################
Data <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
head(Data)
Data$rank <- factor(Data$rank)
mylogit <- glm(admit ~ gre + gpa + rank, data = Data , family = "binomial")
summary(mylogit)
predicted_data <- predict.glm(mylogit,Data)
predicted_data
par(mfrow = c(1,1))#one row one column.
plot(Data$admit,type = "p",col = "red" ,main = "Admit Prediction",ylab = "Admit Value")
points(predicted_data,type = "p",col = "green")
legend("topright",inset = .0005,c("Actual","Prediction"),lwd = 2,lty = c(1,1,1,1,2),col = c("red","green"))
#install.packages("caret")
library(caret)
imp <- varImp(mylogit)
bp <- barplot(imp[,1],xlab = "Importance",ylab = "Data Variables",
              main = "Variable Importance From Regression Model",col = heat.colors(7),horiz = TRUE,xlim=range(0,10))
a <- imp[,1]
names <- rownames(imp)
text(a,bp,names,pos = 4,cex = 1)#cex is the size of the text and pos is the position of the text
#######################################################
##############Decision Tree Algorithm##################
#######################################################
x <- read.csv("E/thailand/health1.csv")
x <- x[-1]#if you want to remove first column
x[is.na(x)] <- 0
#install.packages("rpart.plot")
#install.packages("rpart")
library(rpart)
fit <- rpart(status ~ . ,method = "class",data = x)
summary(fit)
#install.packages("rattle")
library(rattle)
fancyRpartPlot(fit)
predicted_data <- predict(fit,x,type = "class")
plot(x$status,col = "green",main = "Actual",ylab = "Status Value")
plot(predicted_data,col = "red",main = "Predicted",ylab = "Status Value")
#######Next Example#######
#install.packages("party")          
library(party)
library(caret)
data("iris")
set.seed(12)
indexes = createDataPartition(iris$Species,p = .9,list = F)
train = iris[indexes,]
test = iris[-indexes,]
tmodel = ctree(formula = Species ~.,data = train)
print(tmodel)
plot(tmodel)
pred <- predict(tmodel,test[,-5])

cm = confusionMatrix(test$Species,pred)#drop fifth column
print(cm)
library(rpart)
fit <- rpart(Species ~.,method = "class",data = train)
#install.packages("rattle")
library(rattle)
fancyRpartPlot(fit)
##################################################
###############Random Forest Algorithm############
##################################################
#install.packages("randomForest")
library(randomForest)
fit <- randomForest(Kyphosis ~ Age + Number + Start,data = kyphosis,
                    mtry = 1,ntree = 3)#ntree is used for early stopping and mtry tells us how many random variable it selects at one point.
print(fit)
plot(fit)#view results
importance(fit)#importance of each predictor
#Let's see
fit <- randomForest(Kyphosis ~ Age + Number + Start,data = kyphosis,
                    mtry = 1)#by default ntree is selected 500
print(fit)
plot(fit)#view results
#Let's check when ntree is 20
fit <- randomForest(Kyphosis ~ Age + Number + Start,data = kyphosis,
                    mtry = 3,ntree = 5)
print(fit)
plot(fit)#view results
##################################################
###############Naive_Bayes########################
##################################################
library(e1071)
#Next load the Titanic dataset
data("Titanic")
#Save into dataframe and view it
Titanic_df = as.data.frame(Titanic)
#Create data from table
repeating_sequence = rep.int(seq_len(nrow(Titanic_df)),Titanic_df$Freq)#This will repeat each combination equal to the frequency of each.
#Create the dataset by row repetition created
Titanic_dataset = Titanic_df[repeating_sequence,]
#we no longer need the frequency drop the feature
Titanic_dataset$Freq = NULL
#Fitting the Naive_Bayes model
Naive_Baye_model = naiveBayes(Survived ~ .,data = Titanic_dataset)
#What does the model say print the model summary
Naive_Baye_model
#Prediction on the dataset
NB_Prediction = predict(Naive_Baye_model,Titanic_dataset)
#Confusion matrix to check accuracy
table(NB_Prediction,Titanic_dataset$Survived)
##########################################
########## Naive Bayes in mlr ############
##########################################
#Let's try this with another package
#install.packages("mlr")
library(mlr)
#Create a classification task for learning on Titanic dataset and specify the target feature
task = makeClassifTask(data = Titanic_dataset,target = "Survived")
#Initialize the naive bayes classifier
selected_Model = makeLearner("classif.naiveBayes")
#Train the model:-
NB_mlr = train(selected_Model,task)
#Read the model learned
NB_mlr$learner.model
#predict on the dataset without passing the target feature
predict_mlr = as.data.frame(predict(NB_mlr,newdata = Titanic_dataset[1:3]))
#Confusion Matrix to check accuracy
table(predict_mlr[,1],Titanic_dataset$Survived)
?makeLearner
##########################################
######### Categorical data only ##########
##########################################
library(mlbench)
data(HouseVotes84,package = "mlbench")
model <- naiveBayes(Class ~ .,data = HouseVotes84)
predict(model,HouseVotes84[1:10,])
predict(model,HouseVotes84[1:10,],type = "raw")
pred <- predict(model,HouseVotes84)
table(pred,HouseVotes84$Class)
#using laplace smoothing:-
model <- naiveBayes(Class ~.,data = HouseVotes84,laplace = 3 )#laplace is for replacing the null values with some value e.g:- 3
pred <- predict(model,HouseVotes84[,-1])
table(pred,HouseVotes84$Class)
##########################################
##### Example with metric predictors #####
##########################################
data(iris)
m <- naiveBayes(Species ~ .,data = iris)
#alternatively:
m <- naiveBayes(iris[,-5],iris[,5])
m
table(predict(m,iris),iris)
#################################################
###########Artificial Neural Network#############
#################################################
library(MASS)
data()
?Boston
data <- Boston
data
#Min max normalization
#     (x - min)/max-min
maxs <- apply(data,2,max)#2 means columns
maxs
mins <- apply(data,2,min)
mins
train <- as.data.frame(scale(data,center = mins,scale = maxs - mins))
#install.packages("neuralnet")
library(neuralnet)
n <- names(train)
n
f <- as.formula("medv ~ rm + crim +lstat")
nn <- neuralnet(formula = f,data = train, hidden = c(10,5,2),linear.output = T)#hidden means hidden layer with three layers.
plot(nn)
?neuralnet
######################################################
###############Model Evaluation Techniques############
######################################################
library(tidyverse)
library(caret)
#Load the data:-
data("PimaIndiansDiabetes2",package = "mlbench")
pima.data <- na.omit(PimaIndiansDiabetes2)
sample_n(pima.data,3)
#Split the data into training and test set:-
set.seed(123)
training.samples <- pima.data$diabetes %>%
        createDataPartition(p = 0.8,list = FALSE)
train.data <- pima.data[training.samples,]
test.data <- pima.data[-training.samples,]
####Basic Accuracy and Error####
install.packages("MASS")
library(MASS)
#Fit LDA model:-
fit  <- lda(diabetes ~.,data = train.data)
#Make predictions on test data
predictions <- predict(fit,test.data)
prediction.probability <- predictions$posterior[,2]
predicted.classes <- predictions$class
observed.classes <- test.data$diabetes
accuracy <- mean(observed.classes == predicted.classes)
accuracy
length(observed.classes)
observed.classes == predicted.classes#if observed.classes == predicted.classes then it's true otherwise it's false and then we divide the true by the length or total number and we get the accuracy and by dividing the false with length we get error.
length(predicted.classes)
error <- mean(observed.classes != predicted.classes)
error
#########Confusion matrix,number of classes########
table(observed.classes,predicted.classes)
#Confusion matrix proportion of classes:-
table(observed.classes,predicted.classes) %>%
        prop.table() %>% round(digits = 3)
#Confusion matrix from caret library:-
confusionMatrix(observed.classes,predicted.classes,positive = "pos")# In matrix, Reference means Actual and pos pred value is precision
?confusionMatrix
#Computing and plotting ROC curve:-
library(pROC)
#Compute ROC
res.roc <- roc(observed.classes,prediction.probability)
plot.roc(res.roc,print.auc = TRUE)
#Extract some interesting results:-
roc.data <- data.frame(
        thresholds = res.roc$thresholds,
        sensitivity = res.roc$sensitivities,
        specificity = res.roc$specificities
)
#Ge t the probability threshold for specificity = 0.6
roc.data %>% filter(specificity >= 0.6)
plot.roc(res.roc,print.auc = TRUE,print.thres = "best")
plot.roc(res.roc,print.thres = c(0.3,0.5,0.7))
####################
####Test Example####
####################
install.packages("ROCR")
library(ROCR)
data("ROCR.simple")
data <- as.data.frame(ROCR.simple)
pred <- prediction(data$predictions,data$labels)
perf <- performance(pred,"tpr","fpr")
par(mar = c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis = 1.3,cex.lab = 1.4)
#Plotting the ROC curve
plot(perf,col = "black",lty = 3,wld = 3,main = "ROC Curve")
auc <- performance(pred,"auc")
auc <- unlist(slot(auc,"y.values"))
#Adding Min and Max ROC AUC to the center of the plot
minauc <- min(round(auc , digits = 2))
maxauc <- max(round(auc , digits = 2))
minauct <- paste(c("min(AUC) = "),minauc ,sep = "")
maxauct <- paste(c("max(AUC) = "),maxauc , sep = "")
legend(0.3,0.6,c(minauct,maxauct,"\n",border = "white",cex = 1.9 ,box.col = "white"))
###############END##############
