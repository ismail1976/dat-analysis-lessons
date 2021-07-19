#############################################################################
##################      Second Project: Real estate valuation   ###############
##### presented by: BENGANA ISMAIL / KHODIR ZAKARIA ########
#############################################################################
###   The market historical data set of real estate valuation are ###
###collected from Sindian Dist., New Taipei City, Taiwan. The ‚???oreal estate ###
###valuation‚???ù is a regression problem. The data set was randomly split into###
###the training data set (2/3 samples) and the testing data set (1/3 samples). #### 
###             between†two†different variables                 ####
####  Check the data.set   ####
library(readxl)
Real_estate_valuation_data_set <- read_excel("Real estate valuation data set.xls")
View(Real_estate_valuation_data_set)
####   Exploration data   ####
df<-Real_estate_valuation_data_set
names(df)
library(tidyverse)
rename(df, x1 = "X1 transaction date",x2="X2 house age",x3="X3 distance to the nearest MRT station",x4="X4 number of convenience stores",x5="X5 latitude",x6="X6 longitude",y="Y house price of unit area")
str(df)
head(df)
tail(df)
Hmisc::describe(df)
####  A- correlation ####
args(cor)
cor(df,method =c("pearson"))
cor(df,method =c("spearman"))
cor(df,method =c("kendall"))
pairs(df, lower.panel = NULL, col="blue")
library(ggcorrplot)
args(ggcorrplot)
library(PerformanceAnalytics)
chart.Correlation(df, histogram = TRUE, col="blue")
library(psych)
psych::cor.plot(df)
cor1<-cor(df)
corrplot::corrplot(cor1)
corrplot::corrplot(cor1,type = "lower",order = "hclust", tl.col = "black",
                   tl.srt = 45)
args(heatmap)
heatmap(cor1, symm = TRUE, cexRow = 0.7, cexCol = 0.7)
p<-ggcorrplot::ggcorrplot(cor1, method = "circle", type="lower",
                          show.diag = FALSE, colors = c("blue", "white", 
                                                        "red"), tl.cex = 12,
                          tl.col = "black", tl.srt = 45, digits = 2 )
library(WVPlots)
ScatterHist(df, xvar =y, yvar = x1, smoothmethod = "lm")
psych::lowerCor(x=df)

#### B- SIMPLE REGRESSION ####
#### GRAPHIC REPRESENTATION ####
library(ggplot2)
ggplot(df,aes(y,x1))+geom_point(color="red")+geom_smooth(method="lm",se=F)
ggplot() +
  geom_point(data = df, aes(y,x1), colour = 'red') + +geom_smooth(method="lm",se=F)
plot(y,x1)
##### ESTIMATION #########
model<-lm(y~x1, data = df)
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
model1<-lm(x1~x2+x3, data = df)
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