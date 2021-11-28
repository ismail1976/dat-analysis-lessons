###########################################################################
###########################################################################
###                                                                     ###
###                                                                     ###
###         Correspondence Analysis(CA-AFC)                                ###
###                                                                     ###
###########################################################################
###########################################################################
###*                        Loading Packages
###*                        ----------------
suppressPackageStartupMessages(library(FactoMineR))
suppressPackageStartupMessages(library(Factoshiny))
suppressPackageStartupMessages(library(factoextra))
##========================================================================
##                     Reading in the data                     
##========================================================================
library(readxl)
CA_data <- read_excel("C:/Users/lenovo/Desktop/CA.data.xls")
View(CA_data)
df<-CA_data[2:5]
df <- as.table(as.matrix(df))
str(df)

##***************************************************************
##                 Data by plot matrix                  **
##***************************************************************
library("gplots")
# 1. convert the data as a table
df <- as.table(as.matrix(df))
# 2. Graph
balloonplot(t(df), main ="company/insurance", xlab ="company", ylab="insurnce",
            label = FALSE, show.margins = TRUE)

##***************************************************************
##                   CA by  FactoMineR                         **
##***************************************************************
library(FactoMineR)
res.ca <- CA(df)
print(res.ca)
summary(res.ca)
plot.CA(res.ca)

##***************************************************************
##                  CA by  factoextra                        **
##***************************************************************
fviz_screeplot(res.ca, addlabels = TRUE, ylim = c(0, 50))
fviz_screeplot(res.ca) +
  geom_hline(yintercept=33.33, linetype=2, color="red")
# Contributions of rows to dimension 1
fviz_contrib(res.ca, choice = "row", axes = 1, top = 10)
# Contributions of rows to dimension 2
fviz_contrib(res.ca, choice = "row", axes = 2, top = 10)
fviz_ca_biplot(res.ca, 
               map ="rowprincipal", arrow = c(TRUE, TRUE),
               repel = TRUE)

##***************************************************************
##                  CA by  Factoshiny                         **
##***************************************************************
CAshiny(df)
##***************************************************************
##                  Chi-square statistics                      **
##***************************************************************
# 1ST method:
chisq <- chisq.test(df)
chisq
# 2 nd method:
chi2 <- 51.626
# Degree of freedom
dl <- (nrow(df) - 1) * (ncol(df) - 1)
# P-value
pval <- pchisq(chi2, df = dl, lower.tail = FALSE)
pval