#Importing the dataset
liver<-read.csv('Indian Liver Patient Dataset (ILPD).csv',header=TRUE,stringsAsFactors = FALSE)

#Preprocessing to remove invalid data
c<-na.omit(liver$alkphos)
liver[which(is.na(liver$alkphos)),'alkphos']<-mean(c)

#Preprocessing to make data directly understandable
liver[which(liver$is_patient==1),'is_patient']<-"Patient"
liver[which(liver$is_patient==2),'is_patient']<-"Non Patient"

#Preprocessing to normalize data
normalize<-function(x){return(x-min(x)/(max(x)-min(x)))}
liver[,c(1,3:10)]<-as.data.frame(lapply(liver[,c(1,3:10)],normalize))

#Converting categorical data into numerical data
for(i in 1:nrow(liver))
{
  if(liver$gender[i]=="Male")
    liver$gender[i]=1
  else
    liver$gender[i]=2
}
liver$gender<-as.numeric(liver$gender)

#Factoring 
liver$is_patient<-factor(liver$is_patient)

#Loading the required libraries
library(kernlab)

#Splitting the data into training and testing datasets
train1<-liver[c(1:500),c(1:11)]
test1<-liver[c(501:583),c(1:10)]

#Building the model
svmmodel<-ksvm(is_patient~.,data=train1,kpar=list(sigma=0.50),kernel=rbfdot,C=5,cross=3)
svmprediction<-predict(svmmodel,test1)

#Actual data
actual<-liver[c(501:583),11]

#Creating the Confusion Matrix
library(caret)
tabSvm<-table(svmprediction,actual)
SVM<-confusionMatrix(tabSvm,positive='Patient')
