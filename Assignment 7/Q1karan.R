#Importing the Dataset
setwd("/home/karan/DataAnalytics/Assignment7")

optdigits<-read.csv('optdigits.csv',header=TRUE)
#K-Means Clustering in 200 iterations with 10 clusters because we have 10 digits
set.seed(10)
fit<-kmeans(optdigits[1:64],10,iter.max=200)
str(fit)

#Matrix that records the number of instances of digits in each cluster
#Rows denote the cluster number
#Columns denote the digits
#fit$cluster has the cluster that each row belongs to
k<-matrix(nrow=10,ncol=10,0)
for(i in 1:length(fit$cluster))
{
     k[fit$cluster[i],optdigits$digit[i]+1]<-k[fit$cluster[i],optdigits$digit[i]+1]+1
     #optdigits$digit[i]+1 as it is indexed from 1 and digits start from 0
}
#The digits are from 0-9
colnames(k)<-c(0:9) #c(c())?
d<-vector()
#Labelling each cluster with the digit which has the maximum number of instances in it
for(i in 1:nrow(k))
{
  d[i]<-which.max(k[i,])-1
}
rownames(k)<-d
print(rownames(k))
print(k)
#the first cluster has 113 1s and 97 9s. It's close.

#Part 2
#The cluster is cluster 1
count<-1
for(i in 1:length(fit$cluster))
{
  if(fit$cluster[i]==1)
  {
     d[count]<-i
     count<-count+1
  }
}
#New matrix containing only the rows that got clustered into cluster 1
newopt<-optdigits[d,]
str(newopt)
print(newopt)
#Hierarchical Clustering
#can you cluster for 2
clusters<-hclust(dist(newopt[1:64]))
#get digit value?
#rename cluster to rownum??
#merge back with row number?

#how to specify number of clusters in hclust
#reuse kmeans for 2 clusters?
#plot(clusters)
# dend<-as.dendrogram(clusters)
# plot(dend)
# plot(cut(dend,h=50)$upper)
plot(cutree(clusters, k = 2)) #can choose number of branches or cut height
memships = cutree(clusters, k = 2)
str(memships)
cluster1 = subset(newopt, memships==1)
cluster2 = subset(newopt, memships==2)
print(cluster1)
table(cluster1$digit)
table(cluster2$digit)

# > table(cluster1$digit)
# 
# 0  1  3  4  5  7  8  9 
# 1  2  5 25  6  2  2 96 
# > table(cluster2$digit)
# 
# 1   4   7   8   9 
# 111   5   4   3   1 

#Part 3
clusnum <- vector()
clusindex <- vector()
fit$centers[1,]
#fit$centers[1,] is the set of centers for the first cluster. There are 10 clusters.
#Load test data

test<-read.csv('optdigits_test.csv',header=TRUE)
for(i in 1:nrow(test)){
  distance = .Machine$integer.max
  for(j in 1:10){ #there are 10 clusters
    if(dist(rbind(test[i,2:ncol(test)], fit$centers[j,])) < distance){
      distance = dist(rbind(test[i,2:ncol(test)], fit$centers[j,]))
      clusnum[i] = rownames(k)[j]
      clusindex[i] = j
    }
  }
}
#clusnum refers to the digit that matches the input
#imagenumber is the index of the image in the test data
print(clusnum)
imagenumber = c(1:20)
result = data.frame(imagenumber, clusnum, clusindex)
print(result)


#Part 4
# Print the number of data points present under each label. 
length(cluster1$digit) #139 numbers
length(cluster2$digit) #124 numbers
str(cluster1) #it is a data frame

cluster1$clusternumber = seq(0,0,length = nrow(cluster1))
cluster2$clusternumber = seq(0,0,length = nrow(cluster2))
#cluster1 is mostly 9
#cluster2 is mostly 1
#add the cluster number and merge them
for(row in 1:nrow(cluster1)){
  cluster1[row,"clusternumber"] = 1;
}
for(row in 1:nrow(cluster2)){
  cluster2[row,"clusternumber"] = 2;
}
str(cluster1)
str(cluster2)
final = rbind(cluster1, cluster2)
str(final)

#       imagenumber clusnum clusindex
# 1            1       3         9
# 2            2       1         3
# 3            3       0         6
# 4            4       1         1
# 5            5       2         2
# 6            6       7         4
# 7            7       4         8
# 8            8       5         5
# 9            9       6         7
# 10          10       8        10
# 11          11       3         9
# 12          12       0         6
# 13          13       1         1
# 14          14       2         2
# 15          15       3         9
# 16          16       4         8
# 17          17       5         5
# 18          18       6         7
# 19          19       7         4
# 20          20       8        10

#We observe that two images, the 4th and the 13th, were classified into cluster 1. They are the test dataset. We predict clusindex for them.
testdata = test[c(4,13),] #the ones classified to clusindex 1
traindata = test[-c(4,13),] #the ones that weren't
test_labels = clusindex[c(4,13)]
train_labels = clusindex[-c(4,13)]
library(class)
knnpredicted<-knn(traindata,testdata,cl = train_labels,k=7,prob=TRUE)
table(knnpredicted)
# 2  3  4  5  6  7  8  9 10 
# 0  0  0  0  0  0  0  2  0 

#Hence their new label is cluster number 9, whose most frequent element is 3.