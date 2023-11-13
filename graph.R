#######Line Charts########

# Define the vector with 5 values
y <- c(1, 3, 6, 4, 9)
# Graph the vector with all defaults
plot(y)
# Graph using blue points overlayed by a line 
plot(y, main="evolution", xlab="time", ylab="T",type="h", col="green",pch="7")
# Create a title with a red, bold/italic font
title(main="Autos", col.main="red", font.main=4)
# Define 2 vectors
cars <- c(1, 3, 6, 4, 9)
trucks <- c(2, 5, 4, 5, 12)

# Graph  using a y axis that ranges from 0 to 12
plot(cars, type="o", col="blue", ylim=c(0,12))

# Graph trucks with red dashed line and square points
lines(trucks, type="o", pch=22, lty=2, col="red")

# Create a title with a red, bold/italic font
title(main="Autos", col.main="red", font.main=4)
x<-seq(-pi,pi,0.1)
plot(x,sin(x))
x<-seq(-pi,pi,0.1)
plot(x,sin(x),col="blue",type="l",main="My Plot", pch=25, lty=4, xlab="xs",ylab="ys")

##### scatter plot #######
x1<-c(10,13,11,8,7,16,18,12,9,10)
y1<-c(11,15,12,10,9,15,13,10,8,7)
plot(x1,y1,main="scatter", col="50")

###### Bar Charts ########

# Define the  vector with 5 values
x <- c(1, 3, 6, 4, 9)

# Graph x
barplot(x, col="blue",main="My Plot", pch=7, lty=6, xlab="xs",ylab="ys")


data<-
  data.frame("gender"=c(rep("m",5),rep("f",5)),"age"=c(rep(20:21,2),rep(22:23,3)))
dt<-table(data)
dt
barplot(dt,beside=TRUE,col=c("red","blue"))
legend("topleft",c("female","male"),fill=c("red","blue"))

###### pie chart #######
pie(dt , main= "Title",labels=names)

pie(c(31,46),labels=c("female","male"),main="gender")

######## Histograms #######
age <- c(10,7,18,10,12,19,18,20,3,14,19,55,25,31,26,11,34)
hist(age,breaks=6)

# Define the z vector with 5 values
z <- c(4,4,6,6,16)

# Create a histogram for suvs
hist(z)
# Create a histogram for autos in light blue with the y axis
# ranging from 0-5
hist(z, col="lightblue", ylim=c(0,5))
# Create a histogram for y with fire colors, set uneven
# breaks, make x axis range from 0-max_num, disable right-
# closing of cell intervals, set heading, make y-axis labels 
# horizontal, make axis labels smaller, make areas of each
# column proportional to the count
hist(z, col=heat.colors(length()), 
     xlim=c(0,max_num), right=F, main="Probability Density", 
     las=1, cex.axis=0.8, freq=F)

##### Box plot #######
boxplot(x1,ylab="box",main="boxplot")



