#############################
######### GRAPH #############
#############################
x<-c(7,5,6,7,8,9,11) 
plot(x)
barplot(x)
pie(X)
boxplot(x)
hist(x)
summary(x)
###############################
##### LINEAR CORELLATION ######
###############################
x1 <- c(10, 12, 15, 18, 20)
x2 <- c(8, 11, 14, 17, 19)
x3 <- c(5, 7, 9, 11, 13)
# Combine into a data frame
df <- data.frame(x1, x2, x3)
df
########correlation by calculation #######
## 1st method: correlation matrix ##
args(cor)
m1 <- cor(df, method = "pearson")
print("correlation matrix:")
print(m1)
## 2nd method: correlation matrix ##
# Install if needed
install.packages("psych")

# Load package
library(psych)

# Correlation test
m2 <- corr.test(df)

# Correlation matrix
m2$r

# P-values
m2$p
## 3rd method: correlation matrix ##
# Install if needed
install.packages("PerformanceAnalytics")
# Load package
library(PerformanceAnalytics)
# Correlation matrix
m3 <- cor(df)
print("correlation matrix:")
print(m3)
########correlation by graph #######
## 1st method: correlation matrix ##
# Visual correlation matrix
pairs.panels(df,
             method = "pearson",   # correlation method
             hist.col = "lightblue",
             density = TRUE,
             ellipses = TRUE)
## 2nd method: correlation matrix ##
# Install if needed
install.packages("corrplot")

# Load library
library(corrplot)

# Correlation matrix
cor_matrix <- cor(df)

# Plot heatmap
corrplot(cor_matrix, method = "color", type = "upper",
         addCoef.col = "black", tl.col = "black")
## 3rd method: correlation matrix ##
# Install packages if needed
install.packages("ggplot2")
install.packages("reshape2")

# Load libraries
library(ggplot2)
library(reshape2)

# Correlation matrix
cor_matrix <- cor(df)

# Convert matrix to long format
cor_melt <- melt(cor_matrix)

# Plot
ggplot(data = cor_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2))) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1)) +
  theme_minimal() +
  labs(title = "Correlation Matrix Heatmap")
#### 4th method: correlation ####
# Chart correlation matrix
chart.Correlation(df,
                  histogram = TRUE,
                  pch = 19)
###### LINEAR REGRESSION ######
# Regression: y explained by x1 and x2
y<-x3
model1 <- lm(y ~ x1 + x2, data = df)

# Results
summary(model1)
# Plot data
plot(x1, y, pch = 19)

# Add regression line
abline(lm(y ~ x1), col = "blue", lwd = 2)
library(gvlma)
gvlma(model1)

