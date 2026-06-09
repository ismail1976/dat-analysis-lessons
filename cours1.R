#### vector #####
x<-c(7,5,6,7,8,9,11) 
length(x) 
x[5]
#c(2,5,4,8,9,10,13)
arg(cor)
cor(x,y)
y[-12]
y[1:3]
y[c(1,5,9)]
y[1]=12
y[3]=y[3]^2 
y[1:3]<-7
y[c(7,8,9)]<-c(7,8,9) 
rev(y)
y[y>8]<-4 
y[(length(y)-8):length(y)]
rp<-c(rep(1,3),rep(2,5),rep(3,6),rep(4,2),rep(5,3))
rp
sum(x)
##### MATRIX ######
#### Form #####
A=matrix(c(3,2,8,7,5,1,7,8,6,7,2,9),3,4)
matrix(c(3,5,6,2,1,7,8,7,2,7,8,9),4,3,byrow=T)
matrix(c(3,2,8,7,5,1,7,8,6,7,2,9),c(4,3))
matrix(c(3,5,6,2,1,7,8,7,2,7,8,9),c(4,3),byrow=T)
##### exemple ####
y<-matrix(c(3,2,8,7,5,1,7,8,6,7,2,9),4,3);y
y[, 2] 
y[1, ]<-c(4,7,9);y
y1<- y[c(1,2,3), 1:3];y1 
y2<- y[, 2 ] 
y3<-y[, 3] 
y<-cbind(y,c(1,3,4,2)) ;y
y<-y[1:4,1:3];y 
y<-rbind(y,c(2,7,8,9)) 
mean(y[2, ]) 
apply(y,2,  mean)
##### LISTE ######
####  FORM ####
lst1<-list("a"=2.5, "b"=T, "c"=1:3)
x<-list("name"="bisher","age"=26,"marks"=c(90,92,94))
x[3]
x[[3]]
x["name"]
x[["name"]]
x$name
x$n
#### Data Frame ####
Name<-c("A","B","C","D")
Married<-c(F,T,F,F)
Average<-c(75,77,80,81)
df<-data.frame(Name,Married,Average)
df
paste(df$Name)
paste(c("A","B","C","D"))
paste("A","B","C","D")
paste("A","","B","","C","","D")
nchar(df$Name)
length(df$Name)
len(df$Name)
size(df$Name)
nchar(Name)
df1 <- data.frame(
  Name=c("A","B","C","D"),
  Score=c(75,88,92,81),
  Age=c(19,21,20,22));df1
IQR(df1$Age)
quantile(df1,0.75)-quantile(df1,0.25)
IQR(df1)
IQR(Age)
##### exemple ####
summary(table)
table[1, ]
table[ ,3]
table$col
table$Name
View(table)
fix(table)
stack(table)


