library(readxl)
df <- read_excel("C:/Users/START/Desktop/saoudit files/Course specifications2023/Computational Statistics/تطبيقات R/data of modèle.xls")
View(df)
names(df)
G<-as.vector(df[[2]])
Y<-as.vector(df[[3]])
UNP<-as.vector(df[[4]])
ER<-as.vector(df[[5]])
INF<-as.vector(df[[8]])
IR<-as.vector(df[[7]])
G<-ts(G,frequency = 1,start = 1980)
Y<-ts(Y,frequency = 1,start = 1980)
UNP<-ts(UNP,frequency = 1,start = 1980)
ER<-ts(ER,frequency = 1,start = 1980)
INF<-ts(INF,frequency = 1,start = 1980)
IR<-ts(IR,frequency = 1,start = 1980)
df1<-cbind(G,Y,UNP,ER,INF,IR)
plot(df1)
#Descriptive  analysis :
#different descriptive statistics:
summary(df1)
library(psych)
describe(df1)
install.packages("pastecs")
library(pastecs)
stat.desc(df1)
library(skimr)
skim(df1)
library(summarytools)
descr(df1)

### Fiscal Policy Impact on Economic Growth ###
# OLS Model
model1 <- lm(Y ~ G, data = df)
summary(model1)
install.packages("modelsummary")
library(modelsummary)
modelsummary(model1)
### Inflation Targeting Analysis ###

model2 <- lm(INF ~ IR, data = df)
summary(model2)

### Phillips Curve ###
model3 <- lm( INF ~ UNP,data = df)
summary(model3)

### Okun's Law ###
model4 <- lm( UNP ~ Y,data = df)
summary(model4)

### Exchange Rate Pass-Through ###
model5 <- lm(INF ~ ER, data = df)
summary(model5)

### Exchange Rate Pass-Through by ARDL ###
install.packages(ARDL)
install.packages("vars")
library(ARDL)
library(vars)
VARselect(INF)
VARselect(IR)
model6 <- ardl(INF ~ IR,data = df1,order = c(1,1), start(1980))
summary(model6)
bounds_f_test(model6,case = 2)
bounds_t_test(model6,case=2)
### Cointegration Analysis ###
library(urca)
cointegration_test <- ca.jo (df1[,c("Y","IR")], type = "trace",K = 2)
summary(cointegration_test)
### VAR Model ###
library(vars)
var_data <- df1[,c( "INF","IR","Y")]
var_model <- VAR(var_data,p = 2,type = "const")
summary(var_model)

### Impulse Response Functions ###
irf_result <- irf(var_model, impulse = "IR",response = "INF",n.ahead = 12)
plot(irf_result)

### Panel Data Analysis ###
### Static panel ###
library(readxl)
library(readxl)
pdata <- read_excel("PANEL.xlsx")
View(pdata)
library(plm)
pdata <- pdata.frame(pdata, index = c("country","year"))
### pooled regression model ##
pooled_model <- plm(Y ~ I +G,data = pdata,model = "pooling")
summary(pooled_model)
plmtest(pooled_model, type = "bp")
### fixed Effects Model ###
fixed_model <- plm(Y ~  I +G,data = pdata,model = "within")
summary(fixed_model)

### Random Effects Model ###
random_model <- plm( Y ~ I + G, data = pdata, model = "random")
summary(random_model)

### Hausman Test ###
phtest(fixed_model,random_model)

### COINTEGRATION PANEL ###
library(plm)
library(urca)
library(pdR)
### STATIONARTY TESTS ###
purtest( pdata$Y, test = "levinlin")
purtest( pdata, test = "ips")
purtest( pdata,  test = c("levinlin", "ips", "madwu", "Pm", "invnormal", "logit", "hadri"), exo = c("none", "intercept", "trend"),  lags = c("SIC", "AIC", "Hall"))
### COINTEGRATION TESTS ###
## 1-PEDRONI TEST ##
install.packages("pco")
library(pco)
pedroni( Y ~I +G, data = pdata)

## 2- KOA Test ##
library(plm)
model <- plm(Y ~ I +G,data = pdata,model = "within")
residuals_model <- residuals(model)
purtest(residuals_model,test = "ips")

## 3- Westerlund Test ##
library(foreign)
library(pdynmc)
westerlund_test <- pdwtest(Y ~ I+ G,data = pdata)
summary(westerlund_test)

## ESTIMATION MODEL ###
## BY FMLOS ##
install.packages("cointReg")
library(cointReg)
fmols_model <- cointRegFM(y = pdata$Y, X = as.matrix( pdata[,c("I", "G" )]))
summary(fmols_model)
## BY DOLS ##
dols_model <- cointRegD(y  = data$Y,X =  pdata[,c( "I","G" )] )
summary(dols_model)

## Error Correction Model ##
library(plm)
dY=diff(pdata$Y); dI=diff(pdata$I)
ecm <- plm(dY ~ dI  + ECT, data = pdata)
summary(ecm)
### Inflation Forecasting ###
library(forecast)
fit <- auto.arima(df$INF)
forecast(fit, h = 12)

### Random Forest for Growth Prediction ###
install.packages("randomForest")
library(randomForest)
rf_model <- randomForest(Y ~G +INF + IR,data = df)
print(rf_model)
plot(rf_model)
### XGBoost ###
install.packages("xgboost")
library(xgboost)
X <- as.matrix(df1[,c("IR","INF","G")])
Y <- df$Y
model <- xgboost(data = X,label = Y,nrounds = 100,objective = "reg:squarederror")
