#THis script creates data and a few plots
x<-rnorm(n=1000, mean=0,sd=3)
x
hist(x, nclass=20)

###Linear regression
x<-runif(1000,0,100)
x
hist(x)

###Set parameters
m<-3
b<- -2
eps<-rnorm(1000,mean=0,sd=100)
y<-m*x+b+eps
plot(y)
plot(x,y)
 #Reduce size of the points
plot(x,y,cex=.5,col="blue")
