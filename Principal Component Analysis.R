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
head(df)
tail(df)
Hmisc::describe(df)
Hmisc::contents(df)
glimpse(df)
car::brief(df)
##***************************************************************
##                  correlation plot matrices                  **
##***************************************************************
correl <- cor(df[,2:9])
psysch::cor.plot(correl)
corrplot::corrplot(correl)

# Tweak the knobs
corrplot(correl, type = "upper", 
         order = "hclust", 
         tl.col = "black",
         tl.srt = 45)

# check ?corrplot

# Heatmap 

heatmap(correl, symm = TRUE, 
        cexRow = 0.7, 
        cexCol = 0.7)

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
##                  PCA by  FactoMineR                         **
##***************************************************************
model<-PCA(df[,2:9])
summary(model)
plot(model)
model$eig
model$var.coef
model$ind
##***************************************************************
##                  PCA by  Factoshiny                         **
##***************************************************************
PCAshiny(df)
HCPCshiny(df)
