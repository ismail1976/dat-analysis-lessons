##### SEM ########
install.packages("lavaan")
library(lavaan)
library(haven)
tp2 <- read_sav("C:/Users/user/Desktop/tp2.sav")
View(tp2)
#####SEM#####
sem<-'INP=~inp1+inp2+inp3+inp4+inp5+inp6+inp7+inp8+inp9+inp10
TPD=tpd1+tpd2+tpd3+tpd4+tpd5+tpd6+tpd7+tpd8+tpd9+tpd10+tpd11+tpd12+tpd13+tpd14+tpd15+tpd16+tpd17+tpd18+tpd19+tpd20+tpd21
TP=tp1+tp2+tp3+tp4+tp5+tp6+tp7+tp8+tp9+tp10+tp11+tp12+tp13+tp14+tp15+tp16
SE=se1+se2+se3+se4'
