installed.packages("readr")
library(readr)
toyota<-read.csv("E:\\Assignment\\toyotacorolla.csv")
View(toyota)
## Exploratory Data Analysis(60% of time)
# 1. Measures of Central Tendency
# 2. Measures of Dispersion
# 3. Third Moment Business decision
# 4. Fourth Moment Business decision
# 5. Probability distributions of variables
# 6. Graphical representations
#  > Histogram,Box plot,Dot plot,Stem & Leaf plot, 
#     Bar plot
library(dummies)
toyota.new <- dummy.data.frame(toyota, sep = ".")
View(toyota.new)
summary(toyota)
is.na(toyota)
sum(is.na(toyota))
var(toyota)
sample(toyota)
install.packages("sqldf")
library(sqldf)
df1 <- sqldf("select price,km,age_08_04,mfg_month,mfg_year,cc,doors,gears,quarterly_tax,weight,hp from toyota")
View(df1)
summary(df1)
# to create a data frame which contain interested data
install.packages("moments")
library(moments)
skewness(df1)
scale(df1)
#Find the correlation b/n Output and input.
pairs(df1)
plot(df1)
plot(df1)
# correlation coeffiecent matrix-  strength and direction of correlation.
cor(df1)
### Partial Correlation matrix - Pure Correlation  b/n the varibles
#install.packages("corpcor")
library(corpcor)
cor2pcor(cor(df1))
# The Linear Model of interest
model.df1 <- lm(Price ~ ., data = df1)
summary(model.df1)
model.df1<- lm(Price~log(Doors), data = toyota)
summary(model.df1)
model.df<- lm(Price~log(cc), data = toyota)
summary(model.df1)
model.df1 <- lm(log(Price)~Doors+log(cc)+Age_08_04+Mfg_Month+Mfg_Year+Gears+Weight+Quarterly_Tax+KM+HP, data = toyota)
summary(model.df1) ## *** best one***###
model.dfk<- lm(Price~KM, data = df1)
summary(model.dfk)
model.dfq<- lm(Price~Quarterly_Tax, data = df1)
summary(model.dfq)
model.dfkq<- lm(Price~KM+Quarterly_Tax, data = df1)
summary(model.dfkq)
model.dfw<- lm(Price~Weight, data = df1)
summary(model.dfw)
model.dfh<- lm(Price~HP, data = df1)
summary(model.dfh)
model.dfwh<- lm(Price~Weight+HP, data = df1)
summary(model.dfwh)
model.dfm<- lm(Price~Mfg_Month, data = df1) ##
summary(model.dfm) 
model.dfg<- lm(Price~Gears, data = df1)
summary(model.dfg)
model.dfmg<- lm(Price~Mfg_Month+Gears, data = df1)
summary(model.dfmg)

library(psych)
pairs.panels(df1)
# It is Better to delete influential observations rather than deleting entire column which is 
# costliest process
# Deletion Diagnostics for identifying influential observations
influence.measures(model.df1)
## plotting Influential measures 
influenceIndexPlot(model.df1,id.n=3) # index plots for infuence measures
influencePlot(model.df1,id.n=3) # A user friendly representation of the above
influenceplot(model.df1,id.n=2) 
# Regression after deleting the 77th observation, which is influential observation
model.df<- lm(Price~. , data = df1[-c(53,54,50), ])
summary(model.df)
## Variance Inflation factor to check collinearity b/n variables 
Vif(model.df)
## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(model.df1,id.n=2,id.cex=0.7)
## VIF and AV plot has given us an indication to delete "door" and "CC"variable
final<- lm(Price~KM+Age_08_04+Mfg_Month+Mfg_Year+HP+Weight+Gears+Quarterly_Tax, data = df1)
summary(final)
 ##### after applying transformation on "door" and "cc" variables  we can get a best model #####
## significant output###
model.final <- lm(log(Price)~Doors+log(cc)+Age_08_04+Mfg_Month+Mfg_Year+Gears+Weight+Quarterly_Tax+KM+HP, data = toyota)
summary(model.final)
# Evaluate model LINE assumptions 
plot(final)
#Residual plots,QQplot,std-Residuals Vs Fitted,Cook's Distance 
qqPlot(final,id.n = 5)
# QQ plot of studentized residuals helps in identifying outlier 
plot(model.final)
qqplot(final, id.n = 5)

