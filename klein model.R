#############################################################################
##################      Bivariate Analysis           ########################
#############################################################################
###   Bivariate analysis is used to find out if there is a relationship #### 
###             between two different variables                 ####
####  Check the data.set   ####
library(AER)
data(package = "AER")
data("KleinI")
df<-KleinI
df
####   Exploration data   ####
names(df)
str(df)
head(df)
tail(df)
Hmisc::describe(df)
####  A- correlation ####
args(cor)
cor(df,method =c("pearson"))
cor(df,method =c("spearman"))
cor(df,method =c("kendall"))
pairs(df)
library(ggcorrplot)
args(ggcorrplot)
ggcorrplot(df, method = c("circle"),colors = c("blue","red"))
library(psych)
psych::cor.plot(df)
#### B- SIMPLE REGRESSION ####
#### GRAPHIC REPRESENTATION ####
library(ggplot2)
ggplot(df,aes(consumption,x2))+geom_point(color="red")+geom_smooth(method="lm",se=F)
ggplot() +
  geom_point(data = df, aes(consumption,gnp), colour = 'red') + +geom_smooth(method="lm",se=F)
plot(consumption,gnp)
##### ESTIMATION #########
model<-lm(consumption~gnp, data = df)
summary(model)
library(gvlma)
gvlma(model)
####chick the assumptions / DIAGNOSTIC ####
## 1- Linearity in coefficients:
residualPlot(model)
crPlots(model)
## 2- Normality:
qqPlot(model, labels = row.names(df),
       simulate = T, main = "Q-Q Plot", col = "red")
## 3-Independence to errors:
durbinWatsonTest(model)
## 4-Homoskedasticity (constant variance):
ncvTest(model)
spreadLevelPlot(model)
## 5-Multi-collinearity:
vif(model)
sqrt(vif(model)) > 2
model1<-lm(consumption~gnp+capital, data = df)
vif(model1)
sqrt(vif(model1)) > 2
## 6- Outliers :
outlierTest(model)
## 7- High-leverage point:
influenceIndexPlot(model, vars = "hat")
abline(h = c(2,3) * mean(hatvalues(model)), col = c("blue", "red"),
       lty = 2, lwd = 2)
## 8- Influential data plot:
influenceIndexPlot(model, vars = "Cook")
avPlots(model, ask = F)
influencePlot(model, main = "Influence Plot",
              sub = "Circle size is proportional to Cook's distance")
###### Forcasting / Predictions #######
fit<-predict(model, head=20)
fit1<-as.data.frame(fit)
fit1
