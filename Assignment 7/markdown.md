---
title: "Assignment 7"
author: "The Major Miners"
date: "5 November 2017"
output: pdf_document
---

```{r setup, include=FALSE, warning=FALSE,message=FALSE,echo=FALSE}
library(arules)
library(arulesViz)
library(data.table)
```
## Question 1


## Question 2
## Importing the dataset and modifying it to make it suitable for computation
```{r}
hwr<-read.csv('handwriting_recognition.csv',header=TRUE)
hwr<-hwr[rep(row.names(hwr),hwr$Freq),]
hwr<-hwr[,c(2:4)]
```
## Association rules with default settings
```{r}
default<-apriori(hwr,control=list(verbose=FALSE))
default_dt<-as.data.frame(data.table(lhs=labels(lhs(default)),rhs=labels(rhs(default)),quality(default)))
default_dt<-default_dt[,c(1:5)]
print(default_dt)
```
## Association rules for the remaining parts
```{r}
rules<-apriori(hwr,parameter = list(support=0.001, confidence=0.001),control=list(verbose=FALSE))
```
## Subquestion 1
##{Artist,Female}=> Recognized
```{r,echo=FALSE}
part1<-subset(rules, lhs %ain% c("Profession=Artist","Gender=Female") & rhs %ain% c("Recognition=Recognized"))
part1_dt<-as.data.frame(data.table(lhs=labels(lhs(part1)),rhs=labels(rhs(part1)),quality(part1)))
part1_dt<-part1_dt[,c(1:5)]
print(part1_dt)
```
## Subquestion 2
## {Engineer}=>Male
```{r,echo=FALSE}
part2<-subset(rules,lhs %ain% c("Profession=Engineer") & rhs %ain% c("Gender=Male"))
part2<-part2[1]
part2_dt<-as.data.frame(data.table(lhs=labels(lhs(part2)),rhs=labels(rhs(part2)),quality(part2)))
part2_dt<-part2_dt[,c(1:5)]
print(part2_dt)
```
## Subquestion 3
## {Actor,Recognized} => Female
```{r,echo=FALSE}
part3<-subset(rules,lhs %ain% c("Profession=Actor","Recognition=Recognized") & rhs %ain% c("Gender=Female"))
part3_dt<-as.data.frame(data.table(lhs=labels(lhs(part3)),rhs=labels(rhs(part3)),quality(part3)))
part3_dt<-part3_dt[,c(1:5)]
print(part3_dt)
```
## Subquestion 4
## {Doctor,Male} => Unrecognized
```{r,echo=FALSE}
part4<-subset(rules,lhs %ain% c("Profession=Doctor","Gender=Male") & rhs %ain% c("Recognition=Unrecognized"))
part4_dt<-as.data.frame(data.table(lhs=labels(lhs(part4)),rhs=labels(rhs(part4)),quality(part4)))
part4_dt<-part4_dt[,c(1:5)]
print(part4_dt)
```


