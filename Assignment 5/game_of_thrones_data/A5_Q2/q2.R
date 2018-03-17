#a
setwd("E:/Sem 5/Data Analytics/Assignment 5/game_of_thrones_data/A5_Q2")
got<-read.csv('got_viewership.csv',header=TRUE,stringsAsFactors=FALSE)
episode_num<-vector()
for(i in 1:nrow(got))
episode_num[i]=got$Season[i]+(0.01*got$Epsisode[i])
got<-subset(got,select=-c(Season,Epsisode))
got<-cbind(episode_num,got)
#b
gotmodel1<-lm(formula=Viewership..in.million~episode_num+Year+Number.of.Major.Deaths+Critic.Ratings,data=got)
#c
gotmodel2<-lm(formula=Viewership..in.million~episode_num+Year+Number.of.Major.Deaths,data=got)

#d
Number.of.Major.Deaths<-mean(got$Number.of.Major.Deaths)
Critic.Ratings<-min(got$Critic.Ratings)
episode_num<-seq(from=7.01,to=7.07,by=0.01)
Year<-2017
s7<-data.frame(episode_num,Year,Critic.Ratings,Number.of.Major.Deaths)
viewership1<-predict(gotmodel1,s7)
viewership2<-predict(gotmodel2,s7)
episode<-c(1:7)
actual<-read.csv('got_season7_ratings.csv',header=TRUE)
d<-data.frame(cbind(episode,viewership1,viewership2,actual$ratings))
names(d)<-c('episode','viewership1','viewership2','actual')
rmse<-function(error)
{
  sqrt(mean(error^2))
}
error1<-d$actual-d$viewership1
error2<-d$actual-d$viewership2
rmse(error1)
rmse(error2)
#e
library(ggplot2)
library(reshape2)
d<-melt(d,id.vars='episode')
pdf('plot.pdf')
ggplot(d,aes(episode,value,col=variable))+geom_point()+stat_smooth()+xlab('Episode Number')+ylab('Viewership in millions')+ggtitle('Predicted and Actual Viewership')
dev.off()
