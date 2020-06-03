install.packages("readr")
library(readr)
startup <- read.csv("E:\\Assignment\\multidataset\\startups.csv")
View(startup)
# creating dummies 
install.packages("dummies")
library(dummies)
startup.new <- dummy.data.frame(startup, sep = ".")
View(startup.new)
attach(startup.new)
## Exploratory Data Analysis(60% of time)
# 1. Measures of Central Tendency
# 2. Measures of Dispersion
# 3. Third Moment Business decision
# 4. Fourth Moment Business decision
# 5. Probability distributions of variables
# 6. Graphical representations
#  > Histogram,Box plot,Dot plot,Stem & Leaf plot, 
#     Bar plot
summary(startup.new)
is.na(startup.new)
sum(is.na(startup.new))
var(startup.new)
sample(startup.new)
install.packages("moments")
library(moments)
skewness(startup.new)
kurtosis(startup.new)
scale(startup.new)#Find the correlation b/n Output and input.
pairs(startup.new)
plot(startup.new)
# correlation coeffiecent matrix-  strength and direction of correlation.
cor(startup.new)
### Partial Correlation matrix - Pure Correlation  b/n the varibles
#install.packages("corpcor")
library(corpcor)
cor2pcor(cor(startup.new))
# The Linear Model of interest
model.startup.new <- lm(Profit ~ ., data = startup.new)
summary(model.startup.new)
# Prediction based on only Adminstration
model.startup.newa<-lm(Profit~Administration, data = startup.new)
summary(model.startup.newa)
#  Polynomial model with 3 degree
reg3degree<-lm(log(Profit)~Administration + I(Administration*Administration) + I(Administration*Administration*Administration))
 summary(reg3degree)
logpol3 <- predict(reg3degree)
# applying exponential
plot(Administration, log(Profit))
cor(Administration, log(Profit))
reg_exp <- lm(log(Profit) ~ Administration)
summary(reg_exp)
reg2degree <- lm(log(Profit) ~ Administration + I(Administration*Administration))
summary(reg2degree)
# applying log
reg_log <- lm(Profit ~ log(Administration))
summary(reg_log)
# Prediction based on only r.d spend
model.startup.newr<-lm(Profit~R.D.Spend, data = startup.new)
summary(model.startup.newr)
# Prediction based on r.d spend and adminstration
model.startup.newar <- lm(Profit ~ Administration+R.D.Spend, data = startup.new)
summary(model.startup.newar)
# Prediction based on only marketingspend
model.startup.newm <- lm(Profit ~ Marketing.Spend, data = startup.new)
summary(model.startup.newm)
# Prediction based on r.d spend and marketing spend
model.startup.newmr <- lm(Profit ~ R.D.Spend+Marketing.Spend, data = startup.new)
summary(model.startup.newmr)
### Scatter plot matrix along with Correlation Coefficients
library(psych)
pairs.panels(startup.new)
# It is Better to delete influential observations rather than deleting entire column which is 
# costliest process
# Deletion Diagnostics for identifying influential observations
influence.measures(model.startup.new)
## plotting Influential measures 
influenceIndexPlot(model.startup.new,id.n=3) # index plots for infuence measures
influencePlot(model.startup.new,id.n=3) # A user friendly representation of the above
# Regression after deleting the 50th observation, which is influential observation
model.startup<-lm(Profit~Administration+R.D.Spend+Marketing.Spend+State.California+State.Florida+`State.New York`,data=startup.new[-50,])
summary(model.startup)
model.startup1<-lm(Profit~Administration+R.D.Spend+Marketing.Spend+State.California+State.Florida+`State.New York`,data=startup.new[-50,])
summary(model.startup)
model.startup1<-lm(Profit~Administration+R.D.Spend+Marketing.Spend+State.California+State.Florida+`State.New York`,data=startup.new[-50,])
summary(model.startup)
model.startup1<-lm(Profit~Administration+R.D.Spend+Marketing.Spend+State.California+State.Florida+`State.New York`,data=startup.new[-c(50,46,47,49), ])
summary(model.startup1)
model.startup.newamr<-lm(Profit~Administration+R.D.Spend+Marketing.Spend,data = startup.new)
summary(model.startup.newamr)
vif(model.startup.newamr)
## Variance Inflation factor to check collinearity b/n variables 
vif(model.start)
## vif>10 then there exists collinearity among all the variables 
## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(model.startup.new,id.n=2,id.cex=0.7)
finalmodel<-lm(Profit~R.D.Spend+sqrt(Administration)+(Marketing.Spend)^2+State.California+State.Florida+`State.New York`, data = startup.new[-c(50,49), ])
summary(finalmodel)
final1 <- lm(log(Profit) ~ (Administration + I(Administration*Administration)+R.D.Spend+Marketing.Spend+state.California+State.Florida+`State.New York`, data = startup.new[-c(50,49), ]))
summary(final1)
## # applying exponential
final<-lm(log(Profit) ~ Administration+R.D.Spend+Marketing.Spend+`State.New York`+State.California+State.Florida, data = startup.new[-c(50,49), ])
summary(final)
final<-lm(profit~Administration+R.D.Spend+Marketing.Spend, data = satrtup.new)
summary(final)
# applying log
finalmodel<-lm(Profit~R.D.Spend+log(Administration)+Marketing.Spend+State.California+State.Florida+`State.New York`, data = startup.new[-c(50,49), ])
summary(finalmodel)
##  Polynomial model with 3 degree
finalmodel<- lm(log(Profit)~Administration + I(Administration*Administration) + I(Administration*Administration*Administration)+R.D.Spend+Marketing.Spend+State.California+State.Florida+`State.New York`, data = startup.new)
summary(finalmodel)
##Polynomial model with 2 degree
final<-lm(log(Profit) ~ Administration + I(Administration*Administration)+Marketing.Spend+R.D.Spend+State.California+State.Florida+`State.New York`, data = startup.new[-c(50,49), ])
summary(final)
final<- lm(Profit~R.D.Spend+Marketing.Spend,data = startup.new[-c(50,49), ])
summary(final) ### Best model ###
plot(final)
#Residual plots,QQplot,std-Residuals Vs Fitted,Cook's Distance 
qqPlot(model.startup.new,id.n = 5)
# QQ plot of studentized residuals helps in identifying outlier


