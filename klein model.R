#############################################################################
##################          FIRST PROJECT           ########################
##### presented by: BENGANA ISMAIL / KHODIR ZAKARIA ########################
#############################################################################
###   Klein's macroeconomic model of the U. S. economy (Klein, 1950) often ######
###   appears in econometrics texts (e.g., Greene, 2003), contain annual  ########
###            observations from 1920 to 1941          ####### 
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
cor<-cor(df)
corrplot::corrplot(cor)
corrplot::corrplot(cor,type = "lower",order = "hclust", tl.col = "black",
                   tl.srt = 45)
args(heatmap)
heatmap(cor, symm = TRUE, cexRow = 0.7, cexCol = 0.7)
p<-ggcorrplot::ggcorrplot(cor1, method = "circle", type="lower",
                          show.diag = FALSE, colors = c("blue", "white", 
                                                        "red"), tl.cex = 12,
                          tl.col = "black", tl.srt = 45, digits = 2 )

#### B- SIMPLE REGRESSION ####
#### GRAPHIC REPRESENTATION ####
library(ggplot2)
ggplot(df,aes(consumption,x2))+geom_point(color="red")+geom_smooth(method="lm",se=F)
ggplot() +
  geom_point(data = df, aes(consumption,gnp), colour = 'red') + +geom_smooth(method="lm",se=F)
plot(consumption,gnp)
library(WVPlots)
ScatterHist(df, xvar = consumption, yvar = gnp, smoothmethod = "lm")
psych::lowerCor(x=df)
##### ESTIMATION #########
data("KleinI", package = "AER")
plot(KleinI)
## Greene (2003), Tab. 15.3, OLS
library(dynlm)
fm_cons <- dynlm(consumption ~ cprofits + L(cprofits) + I(pwage + gwage), data = KleinI)
fm_inv <- dynlm(invest ~ cprofits + L(cprofits) + capital, data = KleinI)
fm_pwage <- dynlm(pwage ~ gnp + L(gnp) + I(time(gnp) - 1931), data = KleinI)
summary(fm_cons)
summary(fm_inv)
summary(fm_pwage)
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

