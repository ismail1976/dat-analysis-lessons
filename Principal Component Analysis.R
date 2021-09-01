###########################################################################
###########################################################################
###                                                                     ###
###                                                                     ###
###             Principal Component Analysis(PCA-ACP)                   ###
###                                                                     ###
###########################################################################
###########################################################################
###*                        Loading Packages
###*                        ----------------
library(readxl)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(ggcorrplot))
suppressPackageStartupMessages(library(Metrics))
suppressPackageStartupMessages(library(corrplot))
suppressPackageStartupMessages(library(psych))
suppressPackageStartupMessages(library(rela))
suppressPackageStartupMessages(library(MASS))
suppressPackageStartupMessages(library(parallel))
library(FactoMineR)
library(Factoshiny)
##========================================================================
##                     Reading in the data                     
##========================================================================
demoPCA <- read_excel("C:/Users/lenovo/Desktop/demoPCA.xlsm", 
                      sheet = "Data (rates x 1000)")
View(demoPCA)
df<-demoPCA
str(df)

##***************************************************************
##                  correlation plot matrix                  **
##***************************************************************
correl <- cor(df[,2:9])
psysch::cor.plot(correl)
corrplot::corrplot(correl)

# Heatmap 

heatmap(correl)
# ggcorrplot
p <- ggcorrplot::ggcorrplot(correl, 
                            method = "circle", 
                            type = "lower", 
                            ggtheme = ggplot2::theme_linedraw, 
                            lab_col = "blue", 
                            lab_size = 3,
                            tl.cex = 10, 
                            lab = TRUE, 
                            pch.cex = 10, 
                            colors = c("#6D9EC1", "white", "#E46726")) 
p
p + guides(scale = "none")
##***************************************************************
##                  assumptions of  PCA                        **
##***************************************************************
assumptions <- paf(correl)
bartlettTest <- cortest.bartlett(correl, n = 98)
bartlettTest
print(assumptions$kMO)
det(correl)
##***************************************************************
##                  PCA by  FactoMineR                         **
##***************************************************************
model<-PCA(df[,2:9])
summary(model)
##### Graphic of variables
plot(model)
model$eig
model$var.coef
##### Graphic of individual
model$ind
##***************************************************************
##                  PCA by  Factoextra                        **
##***************************************************************

library("factoextra")

model2 <- prcomp(df[,2:9],  scale = TRUE)
##### Graphic of variables
fviz_pca_var(model2, select.var = list(contrib = 8))
##### Graphic of individual
p1 <- fviz_pca_ind(model2, habillage=df$State,
                   addEllipses=TRUE, ellipse.level=0.95)
print(p1)
##### Biplot of individual and variables
fviz_pca_biplot(model2, habillage=df$State,
                addEllipses=TRUE, ellipse.level=0.95)

##***************************************************************
##                  PCA by  Factoshiny                         **
##***************************************************************
PCAshiny(df[,2:9])
HCPCshiny(df[,2:9])
