library(tidyverse)
library(caTools)
auto_mpg_edit = read_csv("C:/Users/poona/Desktop/3155/auto-mpg-edit.csv")
View(auto_mpg_edit)
str(auto_mpg_edit)
options(max.print = 3010)

dim(auto_mpg_edit); head(auto_mpg_edit)

# Scatterplot 
plot(mpg~weight,data=auto_mpg_edit,xlab="weight",ylab="mpg")

# Correlation Coefficient between mpg and weight
(cor.18=cor(auto_mpg_edit$mpg,auto_mpg_edit$weight))

# Regression of Transformed Data
m<-lm(log10(mpg)~weight,data=auto_mpg_edit) ; summary(m)
m$coefficients

# Scatterplot of Tranformation on x
plot(log10(mpg)~weight,data=auto_mpg_edit,xlab="weight",ylab="log10(mpg)")
abline(m,col="red")

# Confidence Interval for Slope
confint(m,level=0.95)

# t-test for whether slope is different from zero
m<-lm(log10(mpg)~weight,data=auto_mpg_edit) ; summary(m)

# Superimpose 90% Confidence Band
summary(auto_mpg_edit$weight)
newx=seq(1613,5140,by=50)

ci90=predict(m,newdata=data.frame(weight=newx),level=0.90,interval="confidence")
plot(log10(mpg)~weight,data=auto_mpg_edit,xlab="weight",ylab="log10(mpg)")
abline(m,col="red")
lines(newx,ci90[,2],col="blue"); lines(newx,ci90[,3],col="blue")

# Residual Plot
auto_mpg_edit.lm=lm(log10(mpg)~weight,data=auto_mpg_edit)
auto_mpg_edit.res=resid(auto_mpg_edit.lm)
plot(auto_mpg_edit$weight, auto_mpg_edit.res, ylab="residuals", xlab="weight",main="auto_mpg_edit")
abline(h=0.0, col="red")

# Normal Probabiliy Plot of Residuals
m$residuals
qqnorm(m$residuals)
qqline(m$residuals)

#M2.1
# Regression Model with 3 numerical predictor variables
m1<-lm(mpg~weight+acceleration+displacement,data=auto_mpg_edit); summary(m1)
m1$coefficients

# ANOVA Table
anova(m1)

# F Test
summary(m1)

# t test for x3 (Displacement)
summary(m1)




#Part 2
proportions = table(auto_mpg_edit$`car name`)/length(auto_mpg_edit$`car name`)
proportions
min(proportions)
#Was testing for the lowest frequency car names

#Model 2.2
weight = lm(auto_mpg_edit$mpg ~ auto_mpg_edit$weight, data=auto_mpg_edit, na.action = na.omit)
summary(weight)

horsepower = lm(auto_mpg_edit$mpg ~ auto_mpg_edit$horsepower, data=auto_mpg_edit, na.action = na.omit)
summary(horsepower)

name = lm(auto_mpg_edit$mpg ~ auto_mpg_edit$`car name`, data=auto_mpg_edit, na.action = na.omit)
summary(name)
#Too many levels in car name for practical use

brand = lm(auto_mpg_edit$mpg ~ auto_mpg_edit$`car brand`, data=auto_mpg_edit, na.action = na.omit)
summary(brand)
#Still too many levels here

country = lm(auto_mpg_edit$mpg ~ auto_mpg_edit$`country of origin`, data=auto_mpg_edit, na.action = na.omit)
summary(country)

weightcountry = lm(auto_mpg_edit$mpg ~ (auto_mpg_edit$weight)*(auto_mpg_edit$`country of origin`), data=auto_mpg_edit, na.action = na.omit)
summary(weightcountry)

M2.2 = lm(auto_mpg_edit$mpg ~ auto_mpg_edit$weight+auto_mpg_edit$`country of origin`+
          ((auto_mpg_edit$weight)*(auto_mpg_edit$`country of origin`)), data=auto_mpg_edit, na.action = na.omit)
summary(M2.2)
plot(M2.2)   #Look at all of these plots before running M2.3

#Shouldn't drop the interaction term as the Residual Standard error has gone down and the R-squared values and F-statistic,
#The p value for some of its levels isn't significant and can be disposed of but the ineraction variable as a whole is significant.

#Model 2.3
M2.3 = lm(auto_mpg_edit$mpg ~ auto_mpg_edit$cylinders  + auto_mpg_edit$horsepower +
            auto_mpg_edit$weight + auto_mpg_edit$acceleration + auto_mpg_edit$`model year` +
            auto_mpg_edit$`car name`+auto_mpg_edit$displacement+auto_mpg_edit$origin+
            auto_mpg_edit$`country of origin`+auto_mpg_edit$`car brand`, data=auto_mpg_edit, na.action = na.omit)
summary(M2.3)
step(M2.3,direction = 'backward')
#AIC = 656.32
#Includes cylinders, horsepower, acceleration, weight, model year, and car name

