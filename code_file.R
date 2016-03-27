library(caret)
library(randomForest)

set.seed(1127)

inTrainURL <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testURL <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

trainData <- read.csv(url(trainURL), na.strings=c("NA","#DIV/0!",""))
courseTesting <- read.csv(url(testURL), na.strings=c("NA","#DIV/0!",""))

inTrain <- createDataPartition(y=trainData$classe, p=0.6, list=FALSE)
training <- trainData[inTrain,]
testing <- trainData[-inTrain,]
dim(training); dim(testing)
rm(inTrain,trainData)

NAcounts <- apply(testing, 2, function(x) length(which(!is.na(x))))
table(NAcounts)

# Remove variables that have a least one value missing 
training <- training[ , colSums(is.na(training)) == 0]
testing <- testing[ , colSums(is.na(testing)) == 0]

# Remove variables that have near zero variance in them 
if (length(nearZeroVar(training)) > 0) {
  training <- training[, -nearZeroVar(training)] 
}

if (length(nearZeroVar(testing)) > 0) {
  testing <- testing[, -nearZeroVar(testing)] 
}

dim(training)

# Review names as last cleaning step - then drop columns 1-6
names(training)
training <- training[, -c(1:6)]  
testing <- testing[ ,-c(1:6)]


# Build the model
fitMod <- randomForest(classe ~ .,training, ntree=500)
summary(fitMod)  

# Tree Error Plot
plot(fitMod)

# Variable Importance Plot
varImpPlot(fitMod, sort=TRUE, main="Variable Importance", n.var=10)

# Variable Importance Table
var.imp <- data.frame(importance(fitMod, type=2))
  # make row names as columns
var.imp$Variables <- row.names(var.imp)
var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),]

# Predicting response variable on testing dataset then look at confusionMatrix 
training$predicted.response <- predict(fitMod, training)
confusionMatrix(data=training$predicted.response, reference=training$classe, positive='yes')

# Predicting response variable on testing dataset then look at confusionMatrix 
testing$predicted.response <- predict(fitMod, testing)
confusionMatrix(data=testing$predicted.response, reference=testing$classe, positive='yes')

# Miss classification error rate for testing dataset
missClass = function(values, prediction) {
  sum(prediction != values)/length(values)
}
errRate = missClass(testing$classe, testing$predicted.response)


# Course quiz 
courseTesting <- courseTesting[ , colSums(is.na(courseTesting)) == 0]
if (length(nearZeroVar(courseTesting)) > 0) {
  courseTesting <- courseTesting[, -nearZeroVar(courseTesting)] 
}
courseTesting <- courseTesting[ ,-c(1:6)]
predict(fitMod, courseTesting)
