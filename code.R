data <- read.csv("pml-training.csv")
# data downloaded from
# https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv
# on 18 January 2021


#divide data into training and test sets
library(caret)
set.seed(198)
inTrain <- createDataPartition(data$classe, p=0.8)[[1]]
training <- data[inTrain,]
testing <- data[-inTrain,]


#filter variables
variables <- grep("(_belt)|(_arm)|(_forearm)|(_dumbbell)|(classe)", names(data))
training <- training[,variables]

NAcol <- which(apply(training, 2,
                     function(x) sum(x==""|is.na(x))/length(x)) >0.5)
training <- training[,-NAcol]


#5-fold cross validation
ctrl <- trainControl(method="cv", number=5)

set.seed(2345)
modRFcv <- train(classe~., data=training, method="rf", trControl=ctrl)

set.seed(2345)
modGBMcv <- train(classe~., data=training, method="gbm", verbose=F, trControl=ctrl)

set.seed(2345)
modLDAcv <- train(classe~., data=training, method="lda", trControl=ctrl)


#check importance of variables in RF model
RFvarimp <- varImp(modRFcv)
no.var <- sum(RFvarimp$importance>10) #19


#training final model
finaltrain <- training[,c(order(RFvarimp$importance, decreasing=T)[1:no.var],53)]

set.seed(5793)
finalRF <- train(classe~., data=finaltrain, method="rf")


#prediction on test set
pred <- predict(finalRF, testing)
conM <- confusionMatrix(pred, factor(testing$classe))

