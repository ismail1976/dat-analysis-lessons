####Check the data.set####
library(haven)
exemple1 <- read_sav("exemple1.xlsx")
View(exemple1)
####Exploration data####
df<-CPS1985
names(df)
str(df)
head(df)
tail(df)
dim(df)
glimpse(df)
ncol(df)
nrow(df)
#### Univariate analysis ####
summary(df)
psych(df)
Hmisc(df)
tab1<-table(x1)
tab1
prop.table(tab1)
class(tab1)
barplot(tab1,col=2:7)
levels(gender)
nlevels(gender)
tab3<-table(gender)
prop.table(tab3)
class(tab3)
barplot(tab3,col=2:7)
levels(occupation)
nlevels(occupation)
tab4<-table(occupation)
tab4
prop.table(tab4)
class(tab4)
####ESTIMATION / FITTING REGRESSION ####
### QUANTITATIVE / QUANTITATIVE 
mod1<-lm(wage~education, data = df)
summary(mod1)
library(ggplot2)
ggplot(df,aes(age,wage))+geom_point(color="red")+geom_smooth(method="lm",se=F)
ggplot() +
  geom_point(data = df, aes(age,wage), colour = 'red') + +geom_smooth(method="lm",se=F)
 plot(age,wage)
 ####polynomial regression####
 mod3<-lm(wage~education+I(education^2), data = df)
 summary(mod3)
 #####regression with categrical variable ####
 mod4<-lm(wage~occupation, data = df)
 summary(mod4)
 ####chick the assuptions ####
library(tseries)
 library(forecast)
 durbinWatsonTest(mod1)
 dw<-durbinWatsonTest(mod1)
 dw
 fit<-lm(wage~education)
 par(mfrow=c(2,2))
 plot(fit)
 par(mfrow=c(1,1))
 fit<-lm(wage~occupation)
 par(mfrow=c(2,2))
 plot(fit)
 par(mfrow=c(1,1))
 fit1<-lm(wage~education+experience+age)
 par(mfrow=c(2,2))
 plot(fit1)
 par(mfrow=c(1,1))
 fit2<-lm(wage~education+experience+age+occupation+region+ethnicity+sector+union+married)
 par(mfrow=c(2,2))
 plot(fit2)
 par(mfrow=c(1,1))
 
 