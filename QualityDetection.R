library(caret)
library(kernlab)
library(e1071)

trainingurl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
trainingCSV <- read.csv(trainingurl)

inTrain <- createDataPartition(y=trainingCSV$classe, p=0.6, list=FALSE)
trainingData <- trainingCSV[inTrain, ]
testingData <- trainingCSV[-inTrain, ]

colWithNa <- apply(trainingData, 2, function(x){any(is.na(x))})
trainingDataWithoutNA <- trainingData[, !colWithNa]

# Remove factor variables because these columns correspond to entries with #DIV/0!.
i <- sapply(trainingDataWithoutNA, is.factor)
trainingWithoutFactor <- trainingDataWithoutNA[, !i]
trainingWithoutFactor <- cbind(trainingWithoutFactor, classe=trainingData[,160])

dim(trainingWithoutFactor)
names(trainingWithoutFactor)

correlationTempTraining <- trainingWithoutFactor[, -c(1:4, 57)]
M <- abs(cor(correlationTempTraining))
diag(M) <- 0
which(M > 0.9, arr.ind=T)

modelFit <- train(classe ~.,data=trainingWithoutFactor, method="rf", prox=TRUE, preProcess="pca")
modelFit

#Training prediction
classeHatTraining <- predict(modelFit, trainingWithoutFactor)
#In sample error rate
confusionMatrix(trainingWithoutFactor$classe, classeHatTraining)

#Testing prediction
classeHatTesting <- predict(modelFit, testingData)
#Out of sample error rate
confusionMatrix(testingData$classe, classeHatTesting)
