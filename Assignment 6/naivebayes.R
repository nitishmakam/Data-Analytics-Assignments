#Naive Bayes Classifier
#Importing the data
liver<-read.csv('Indian Liver Patient Dataset (ILPD).csv',header=TRUE,stringsAsFactors = FALSE)

#Preprocessing to remove invalid data
liver<-na.omit(liver)

#Preprocessing to make data directly understandable
liver[which(liver$is_patient==1),'is_patient']<-"Patient"
liver[which(liver$is_patient==2),'is_patient']<-"Non Patient"

#Preprocessing to normalize data
normalize<-function(x){return(x-min(x)/(max(x)-min(x)))}
liver[,c(1,3:10)]<-as.data.frame(lapply(liver[,c(1,3:10)],normalize))

#Factoring categorical variables
liver$is_patient<-factor(liver$is_patient)
liver$gender<-factor(liver$gender)

#Splitting data into training dataset and testing dataset
train<-liver[c(1:500),]
test<-liver[c(501:579),c(1:10)]

#Loading the required libraries
library(e1071)

#Building the model
naivebayesmodel<-naiveBayes(is_patient~.,data=train)

#Predicting using the model
naivebayesprediction<-predict(naivebayesmodel,test)

#The actual data
actual<-liver[c(501:579),'is_patient']

#The confusion matrix
library(caret)
tabNB<-table(naivebayesprediction,actual)
NB<-confusionMatrix(tabNB,positive='Patient')