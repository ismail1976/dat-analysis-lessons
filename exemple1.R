##########################
####Check the data.set####
##########################
library(readxl)
exemple1 <- read_excel("exemple1.xlsx")
View(exemple1)
##########################
####Exploration data####
##########################
df<-exemple1
names(df)
str(df)
head(df)
tail(df)
dim(df)
ncol(df)
nrow(df)
glimpse(df)
#############################
#### Univariate analysis ####
#############################
tab1<-table(x1)
tab1
prop.table(tab1)
summary(df)
summary(df$x1)
IQR(df$x1)
boxplot(df$x1)
pie(df$x1)
barplot(tab1,col=2:10, main ="barplot of tab1", xlab = "variable", ylab = "values")
hist(tab1,,col=2:7)
psych(df)
Hmisc(df)
tab1<-ts(tab1, frequency = 1, c(1990))

