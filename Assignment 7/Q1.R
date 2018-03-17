#Importing the Dataset
optdigits<-read.csv('optdigits.csv',header=TRUE)
#K-Means Clustering in 200 iterations with 10 clusters because we have 10 digits
set.seed(10)
fit<-kmeans(optdigits[1:64],10,iter.max=200)
#Matrix that records the number of instances of digits in each cluster
#Rows denote the cluster number
#Columns denote the digits
k<-matrix(nrow=10,ncol=10,0)
for(i in 1:length(fit$cluster))
{
     k[fit$cluster[i],optdigits$digit[i]+1]<-k[fit$cluster[i],optdigits$digit[i]+1]+1
}
#The digits are from 0-9
colnames(k)<-c(c(0:9))
d<-vector()
#Labelling each cluster with the digit which has the maximum number of instances in it
for(i in 1:nrow(k))
{
  d[i]<-which.max(k[i,])-1
}
rownames(k)<-d

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

#Hierarchical Clustering
clusters<-hclust(dist(newopt[1:64]))
dend<-as.dendrogram(clusters)
plot(cut(dend,h=50)$upper)

