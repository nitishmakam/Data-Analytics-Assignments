#QUestion 2
#Installing the required libraries
install.packages("arules")
install.packages("arulesViz")

#Loading the required Libraries
library(arules)
library(arulesViz)

#Importing the dataset
hwr<-read.csv('handwriting_recognition.csv',header=TRUE)
hwr<-hwr[rep(row.names(hwr),hwr$Freq),]
hwr<-hwr[,c(2:4)]
default<-apriori(hwr)
rules<-apriori(hwr,parameter = list(support=0.001, confidence=0.001))
print("Association rules with default settings are")
inspect(default)

#Subpart 1 
#{Artist,Female}=> Recognized
part1<-subset(rules, lhs %ain% c("Profession=Artist","Gender=Female") & rhs %ain% c("Recognition=Recognized"))
inspect(part1)

#Subpart 2
#{Engineer}=>Male
part2<-subset(rules,lhs %ain% c("Profession=Engineer") & rhs %ain% c("Gender=Male"))
part2<-part2[1]
inspect(part2)

#Subpart 3
#{Actor,Recognized} => Female
part3<-subset(rules,lhs %ain% c("Profession=Actor","Recognition=Recognized") & rhs %ain% c("Gender=Female"))
inspect(part3)

#Subpart 4
#{Doctor,Male} => Unrecognized
part4<-subset(rules,lhs %ain% c("Profession=Doctor","Gender=Male") & rhs %ain% c("Recognition=Unrecognized"))
inspect(part4)