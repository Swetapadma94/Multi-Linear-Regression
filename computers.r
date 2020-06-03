install.packages("readr")
library(readr)
computer <- read.csv("E:\\Assignment\\multidataset\\Computer_Data.csv")
View(computer)
# creating dummies 
install.packages("dummies")
library(dummies)
computer.new <- dummy.data.frame(computer, sep = ".")
View(computer.new)
attach(computer.new)
## Exploratory Data Analysis(60% of time)
# 1. Measures of Central Tendency
# 2. Measures of Dispersion
# 3. Third Moment Business decision
# 4. Fourth Moment Business decision
# 5. Probability distributions of variables
# 6. Graphical representations
#  > Histogram,Box plot,Dot plot,Stem & Leaf plot, 
#     Bar plot
summary(computer.new)
is.na(computer.new)
sum(is.na(computer.new))
var(computer.new)
install.packages("moments")
library(moments)
skewness(computer.new)
kurtosis(computer.new)
scale(computer.new)
#Find the correlation b/n Output and input.
pairs(computer.new)
plot(computer.new)
# correlation coeffiecent matrix-  strength and direction of correlation.
cor(computer.new)
### Partial Correlation matrix - Pure Correlation  b/n the varibles
install.packages("corpcor")
library(corpcor)
cor2pcor(cor(computer.new))
# The Linear Model of interest
  model.computer.new <- lm(price ~ ., data = computer.new)
summary(model.computer.new)
# To check colinearity
model.computer.news <- lm(price ~ speed, data = computer.new)
summary(model.computer.news)
model.computer.newss <- lm(price ~ screen, data = computer.new)
summary(model.computer.newss)
model.computer.newsss <- lm(price ~ speed+screen, data = computer.new) 
summary(model.computer.newsss)
model.computer.newr <-lm(price ~ ram, data = computer.new)
summary(model.computer.newr)
model.computer.newrs <- lm(price ~ ram+screen, data = computer.new )
summary(model.computer.newrs)
model.computer.newt <- lm(price ~ trend, data = computer.new)
summary(model.computer.newt)
model.computer.newst <- lm(price ~ screen + trend, data = computer.new)
summary(model.computer.newst)
# no partial correlation 
### Scatter plot matrix along with Correlation Coefficients
installe.packages("psych")
library(psych)
pairs.panels(computer.new)
# It is Better to delete influential observations rather than deleting entire column which is 
# costliest process
# Deletion Diagnostics for identifying influential observations
influence.measures(model.computer.new)
library(computer.new)
## plotting Influential measures 
influenceIndexPlot(model.computer.new,id.n=3)
influencePlot(model.computer.new,id.n=3)
# Regression after deleting the 77th observation, which is influential observation
model.computer.new1<-lm(price~.,data=computer.new[-142,])
summary(model.computer.new1)
#Regression after deleting the 5th & 6th Observations
model.computer.new2 <- lm(price~.,data=computer.new[-c(5,6), ])
summary(model.computer.new2)
model.computer.new3 <- lm(price~., data = computer.new[-c(5,6,20,25,28,38,42,45,61,71,113,142), ])
summary(model.computer.new3)
# after deleting obsrvation also we are getting sase R^2 value is not getting incresed)
## Variance Inflation factor to check collinearity b/n variables 
vif(model.computer.new)
avPlots(model.computer.new,id.n=2,id.cex=0.7)
vif(model.computer.new)
## from VIF and AV plot no correlation is theree
model <- lm(price ~ speed+hd+ram+ads+trend+cd.no+cd.yes+premium.no+premium.yes+multi.no+multi.yes, data = computer.new)
summary(model)## after deleting screen we are getting less MR^2
modeel1 <- lm(price ~ speed+hd+screen+ads+trend+cd.no+cd.yes+premium.no+premium.yes+multi.no+multi.yes, data = computer.new)
summary(modeel1)## after deleting ram we are getting less MR^2
## final model
finalmodel <- lm(price~.,data = computer.new)
summary(finalmodel)
# Evaluate model LINE assumptions 
plot(finalmodel)
qqPlot(model.computer.new,id.n = 5)
# QQ plot of studentized residuals helps in identifying outlier 

