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
install.packages("plm")
library(plm)
pdata <- pdata.frame(df, index = c("country","year"))
model <- plm(Y ~ G + INF +UNP,data = pdata,model = "within")
summary(model)

### Random Effects Model ###
random_model <- plm( GDP_Growth ~ Investment + Inflation +Education, data = pdata, model = "random")
summary(random_model)

### Hausman Test ###
phtest(fixed_model,random_model)

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
