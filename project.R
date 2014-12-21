library(caret)
library(ggplot2)
pmlTraining = read.csv("pml-training.csv", header = TRUE) # loading the training data
pmlTesting = read.csv('pml-testing.csv', header = TRUE) # loading the testing data
t<-pmlTraining[, colSums(is.na(pmlTraining)) == 0 ] # to remove the columns with NAs
nums <- sapply(t, is.numeric) # to get numerical colums only
nums[1:7] = F # to remove the the first column (X)
nums[1:9]
nums[93] = T # to inclued the classe column
training<-t[, nums]
head(training)
inTrain <- createDataPartition(y=training$classe, p=0.2, list=FALSE)
inTest <- createDataPartition(y=training$classe, p=0.03, list=FALSE)
testing=training[inTest,]
dim(testing)
subSet = training[inTrain,]
dim(subSet)
modFit <- train(classe ~ ., data=subSet, method="rf",prox=TRUE)
names(subSet) %in% names(pmlTesting)
vv<-names(pmlTesting) %in% names(subSet)
testing <- pmlTesting[,vv]
pred <- predict(modFit,testing); testing$predRight <- pred==testing$classe
table(pred,testing$classe)
#testing$classe
#head(testing)
pred

## The following function generates the files for submission 
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(pred)
