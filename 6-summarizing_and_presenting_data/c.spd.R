
## ------------------------------------------------------------------------
par(las=1,mar=c(5,2,3,1)+0.1)
x <- rnorm(25,20,3)
y <- rnorm(10,24,5)
z <- rep(1:2,c(length(x),length(y)))
z <- z + runif(length(z),-0.1,0.1)
plot(z,c(x,y),
     xlim=c(0.3,2.8),xaxt="n",xlab="Group",ylab="",lwd=2)
abline(v=c(1,2),lty=2,col="gray70")
points(z,c(x,y),lwd=2)
u <- par("usr")
segments(c(1,2),u[3],c(1,2),u[3]-diff(u[3:4])*0.02,xpd=TRUE)
text(c(1,2),u[3]-diff(u[3:4])*0.06,c("A","B"),xpd=TRUE,font=2)
mex <- mean(x)
mey <- mean(y)
segments(0.8,mex,1.2,mex,lwd=2,col="blue")
segments(1.8,mey,2.2,mey,lwd=2,col="blue")


## ------------------------------------------------------------------------
par(las=1,mfrow=c(2,1),mar=c(3,1,4,1)+0.1)
x <- rnorm(300,20,5)
y <- exp(rnorm(300,2,0.75))
mx <- ceiling(max(c(x,y)))
hist(x,breaks=30,xlab="",ylab="",
     yaxt="n",main="Symmetric distribution")
hist(y,breaks=30,xlab="",ylab="",
     yaxt="n",main="Skewed distribution")


## ------------------------------------------------------------------------
par(las=1,mfrow=c(1,1),mar=c(5,2,3,1)+0.1)
x <- rnorm(300,40,5)
y <- rnorm(300,50,5)
z <- rnorm(300,40,10)
w <- rnorm(300,60,5)
boxplot(x,y,z,w,xlab="Group",names=c("A","B","C","D"),range=0)


