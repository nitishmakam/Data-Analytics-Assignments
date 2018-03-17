#K nearest neighbour classification
#Importing the dataset
liver<-read.csv('Indian Liver Patient Dataset (ILPD).csv',header=TRUE,stringsAsFactors = FALSE)

#Preprocessing to substitute NAs with mean of that column
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
library(class)

#Splitting the data into training and testing datasets
train2<-liver[c(1:500),c(1:10)]
test2<-liver[c(501:583),c(1:10)]

#Factor of true classifications of training set
cl<-liver[c(1:500),11]

#Building the model and predicting use the model
knnpredicted<-knn(train2,test2,cl,k=3,prob=TRUE)
actual<-liver[c(501:583),11]

#Computing the confusion Matrix
library(caret)
tabKnn<-table(knnpredicted,actual)
KNN<-confusionMatrix(tabKnn,positive='Patient')