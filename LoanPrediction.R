#Reading the given CSV file in RStudio
train <- read.csv("/DataHackathon/train.csv", header=T, na.strings=c("","NA",".","Not Available","n/a","N/A")) ##Changed blank cells to NA

#One-hot encoding
train$Gender<-as.numeric(train$Gender)-1
train$Married<-as.numeric(train$Married)-1
train$Education<-as.numeric(train$Education)-1
train$Self_Employed<-as.numeric(train$Self_Employed)-1
train$Property_Area<-as.numeric(train$Property_Area)-1
train$Loan_Status<-as.numeric(train$Loan_Status) -1

#Imputation
library(mice)
trainTemp <- mice(train,m=1,maxit=10,meth='rf',seed=50)
summary(trainTemp)

completedTrain <- complete(trainTemp,1)
missingpercompletedTrain<-as.data.frame(apply(completedTrain,2,permiss))

#For regression modeling, excluding the date variable
imp_model1 <- completedTrain[,-1]

#Data partition

library(dplyr)
trainPartition<-sample_frac(imp_model1,0.7)
sid<-as.numeric(rownames(trainPartition)) # because rownames() returns character
testPartition<-trainPartition[-sid,]


#Model Building

model1 = glm(Loan_Status ~ .,family=binomial(link="logit"), data= trainPartition)
summary(model1)

#Stepwise Regression
nothing = glm(Loan_Status ~ 1,family=binomial(link="logit"), data= trainPartition)

forwards = step(nothing, scope=list(lower=formula(nothing),upper=formula(model1)), data=trainPartition, direction="forward")

install.packages("caret")
install.packages("pbkrtest")
library(pbkrtest)
library(caret)

model2 = glm(Loan_Status ~ Credit_History + Married + Property_Area + Education,family=binomial(link="logit"), data= trainPartition)
summary(model2)

#Calculating Accuracy of Logistic Model
fitted.results <- predict(model1,newdata=testPartition,type='response')
fitted.results <- ifelse(testPartition$Loan_Status > 0.5,1,0)

misClasificError <- mean(fitted.results != testPartition$Loan_Status)
print(paste('Accuracy',1-misClasificError))

library(ROCR)

pr <- prediction(fitted.results, testPartition$Loan_Status )
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


###Test dataset

test <- read.csv("/DataHackathon/test.csv", header=T, na.strings=c("","NA",".","Not Available","n/a","N/A")) ##Changed blank cells to NA

#One-hot encoding
#Handling categorical variable(text format)
test$Gender<-as.numeric(test$Gender)-1
test$Married<-as.numeric(test$Married)-1
test$Education<-as.numeric(test$Education)-1
test$Self_Employed<-as.numeric(test$Self_Employed)-1
test$Property_Area<-as.numeric(test$Property_Area)-1


#Impute

testTemp <- mice(test,m=1,maxit=1,meth='rf',seed=50)
summary(testTemp)

completedTest <- complete(testTemp,1)
missingpercompletedTest<-as.data.frame(apply(completedTest,2,permiss))


completedTest$Loan_Status <- predict(model1,newdata=completedTest,type='response')
completedTest$Loan_Status <- ifelse(completedTest$Loan_Status > 0.5,1,0)



testFinal = completedTest[c("Loan_ID","Loan_Status")]
finalSubmit<-testFinal

finalSubmit$Loan_Status[which(finalSubmit$Loan_Status== 1)] = "Y"
finalSubmit$Loan_Status[which(finalSubmit$Loan_Status== 0)] = "N"
write.csv(finalSubmit, file="/DataHackathon/testFinalModel1.csv", row.names=FALSE  )




#Model2 on Test Dataset


completedTest$Loan_Status <- predict(model2,newdata=completedTest,type='response')
completedTest$Loan_Status <- ifelse(completedTest$Loan_Status > 0.5,1,0)



testFinal = completedTest[c("Loan_ID","Loan_Status")]
finalSubmit<-testFinal

finalSubmit$Loan_Status[which(finalSubmit$Loan_Status== 1)] = "Y"
finalSubmit$Loan_Status[which(finalSubmit$Loan_Status== 0)] = "N"
write.csv(finalSubmit, file="/DataHackathon/testFinal.csv", row.names=FALSE  )



#Model3 Decision Tree

library(party)
output.tree <- ctree(
  Loan_Status ~ ., 
  data = trainPartition)


completedTest$Loan_Status <- predict(output.tree,newdata=completedTest,type='response')
completedTest$Loan_Status <- ifelse(completedTest$Loan_Status > 0.5,1,0)



testFinal = completedTest[c("Loan_ID","Loan_Status")]
finalSubmit<-testFinal

finalSubmit$Loan_Status[which(finalSubmit$Loan_Status== 1)] = "Y"
finalSubmit$Loan_Status[which(finalSubmit$Loan_Status== 0)] = "N"
write.csv(finalSubmit, file="/DataHackathon/testFinalDT.csv", row.names=FALSE  )



#CART on Test Dataset

library(rpart)
cartModel <- rpart(Loan_Status ~ Credit_History + Married + Property_Area + Education,
             method="class", data=trainPartition)

completedTest$Loan_Status <- predict(cartModel,newdata=completedTest,type='response')
completedTest$Loan_Status <- ifelse(completedTest$Loan_Status > 0.5,1,0)



testFinal = completedTest[c("Loan_ID","Loan_Status")]
finalSubmit<-testFinal

finalSubmit$Loan_Status[which(finalSubmit$Loan_Status== 1)] = "Y"
finalSubmit$Loan_Status[which(finalSubmit$Loan_Status== 0)] = "N"
write.csv(finalSubmit, file="/DataHackathon/testFinalCart.csv", row.names=FALSE  )


######Naive Bayes

library(e1071)
nb_model <- naiveBayes(Loan_Status~.,data = trainPartition)




completedTest$Loan_Status <- predict(nb_model,newdata=completedTest,type='response')
completedTest$Loan_Status <- ifelse(completedTest$Loan_Status > 0.5,1,0)



testFinal = completedTest[c("Loan_ID","Loan_Status")]
finalSubmit<-testFinal



completedTest <- predict(nb_model, completedTest)
completedTest$probabilities

# Get the confusion matrix
preds$ctable

# Get the overall accuracy
preds$accuracy

## End(Not run)
finalSubmit$Loan_Status[which(finalSubmit$Loan_Status== 1)] = "Y"
finalSubmit$Loan_Status[which(finalSubmit$Loan_Status== 0)] = "N"
write.csv(finalSubmit, file="/DataHackathon/testFinalDT.csv", row.names=FALSE  )



plot(completedTrain$ApplicantIncome, completedTrain$LoanAmount)


