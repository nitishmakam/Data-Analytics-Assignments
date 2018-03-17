#According to the Internet, TMC includes TMH and ACTREC
#Actual values can be found by subtraction
setwd("/home/karan/DataAnalytics/Assignment4")
install.packages("mice")
install.packages("Amelia")

library(mice)
months <- read.csv("cancer_2015.csv")
#mean
print(mean(months$Outpatients.TMH, na.rm = TRUE))
print(mean(months$New.Registrations.ACTREC, na.rm = TRUE))
print(mean(months$Laboratory.Investigations.ACTREC, na.rm = TRUE))

lerp <- function(col){
  for(i in 1:nrow(months)){
    if(is.na(months[i,col])){
      prev = months[i-1,col]
      check = i + 1
      while(is.na(months[check,col])){
        check = check + 1
      }
      nex = months[check,col]
      lerpval = prev +  (nex-prev)/(check - (i-1))#as x-x0 is always 1 here. We start right after
      months[i,col] = lerpval
      print(lerpval)
    }
  }
}

querp <- function(col){
  for(i in 1:nrow(months)){
    if(is.na(months[i,col])){
      prev1 = months[i-1, col]
      prev2 = months[i-2, col]
      check = i + 1
      while(is.na(months[check,col])){
        check = check + 1
      }
      nex = months[check,col]
      trix = matrix(c((i-2)^2, (i-2), 1, (i-1)^2, (i-1), 1, check^2, check, 1), nrow = 3, ncol = 3, byrow = TRUE)
      ans = matrix(c(prev2, prev1, nex), nrow = 3, ncol = 1, byrow = TRUE)
      x = solve(trix, ans)
      matt = matrix(c(i^2, i, 1), nrow = 1, ncol = 3, byrow = TRUE)
      months[i,col] = matt %*% x
      print(matt %*% x)
    }
  }
}

lerp("Outpatients.TMH")
lerp("New.Registrations.ACTREC")
lerp("Laboratory.Investigations.ACTREC")

querp("Outpatients.TMH")
querp("New.Registrations.ACTREC")
querp("Laboratory.Investigations.ACTREC")

imp = mice(months,m=5,maxit=50,meth='pmm',seed=500)
summary(imp)
moused = complete(imp)
print(moused)

linear_reg <- function(y,x,intercept,slope){
  for(i in 1:nrow(months)){
    if(is.na(months[i,y])){
      print(intercept + slope*months[i, x])
    }
  }
}

symnum(cor(months[,2:ncol(months)], use = "complete.obs")) #use = complete.obs suggest to take only the complete data, leave out first column which is not numeric
#we see that Outpatients.TMH is somewhat correlated with LIT.total
#LIA has some correlations but they don't make sense
#New.Registrations.ACTREC is highly correlated with Outpatients.ACTREC

relation = lm(New.Registrations.ACTREC ~ Outpatients.ACTREC, months)
# Coefficients:
#   (Intercept)  Outpatients.ACTREC  
# 11.593593            0.006804  
linear_reg("New.Registrations.ACTREC", "Outpatients.ACTREC", summary(relation)$coefficients[1,1], summary(relation)$coefficients[2,1])

relation = lm(Outpatients.TMH ~ Laboratory.Investigations.TMC.Total, months)
# Coefficients:
#   (Intercept)  Laboratory.Investigations.TMC.Total  
# 2.330e+03                            2.095e-02  
linear_reg("Outpatients.TMH", "Laboratory.Investigations.TMC.Total", summary(relation)$coefficients[1,1], summary(relation)$coefficients[2,1])

relation = lm(Laboratory.Investigations.ACTREC ~ New.Registrations.TMC.Total, months)
# Coefficients:
#   (Intercept)  New.Registrations.TMC.Total  
# 15844.179                       -2.776  
linear_reg("Laboratory.Investigations.ACTREC", "New.Registrations.TMC.Total", summary(relation)$coefficients[1,1], summary(relation)$coefficients[2,1])
