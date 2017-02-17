
## ------------------------------------------------------------------------
par(bty="n")
x <- seq(-4,4,length=501)
plot(x,dnorm(x),type="l",xaxt="n",yaxt="n",xlab="",ylab="",lwd=2)
abline(h=0)
x <- c(-2,-1,1,2)
segments(x,0,x,-0.01,xpd=TRUE)
segments(0,0,0,-0.01,xpd=TRUE,col="blue")
text(0,-0.04,expression(mu),xpd=TRUE,cex=1.3,col="blue")
segments(c(0,0,1),c(0.04,0.03,0.03),c(1,0,1),c(0.04,0.05,0.05),lwd=2,col="red")
text(0.5,0.07,expression(sigma/sqrt(n)),cex=1.3,col="red")


## ------------------------------------------------------------------------

p <- 5; n <- 100
lo3 <- hi3 <- lo2 <- hi2 <- lo <- hi <- vector("list",p)
for(i in 1:p) {
  dat <- matrix(rnorm(n*10,3.5,sd=1.5),ncol=10)
  m <- apply(dat,1,mean)
  s <- apply(dat,1,sd)
  lo[[i]] <- m-qnorm(0.975)*1.5/sqrt(10)
  hi[[i]] <- m+qnorm(0.975)*1.5/sqrt(10)
  lo2[[i]] <- m-qnorm(0.975)*s/sqrt(10)
  hi2[[i]] <- m+qnorm(0.975)*s/sqrt(10)
  lo3[[i]] <- m-qt(0.975,9)*s/sqrt(10)
  hi3[[i]] <- m+qt(0.975,9)*s/sqrt(10)
}
r <- range(unlist(c(lo,hi,lo2,hi2,lo3,hi3)))
par(mfrow=c(1,5), las=1, mar=c(5.1,2.1,6.1,2.1))
for(i in 1:p) {
  plot(0,0,type="n",ylim=0.5+c(0,n),xlim=r,ylab="",xlab="",yaxt="n")
  abline(v=3.5,lty=2,col="red",lwd=2)
  segments(lo[[i]],1:n,hi[[i]],1:n,lwd=2)
  o <- (1:n)[lo[[i]] > 3.5 | hi[[i]] < 3.5]
  segments(lo[[i]][o],o,hi[[i]][o],o,lwd=2,col="orange")
}
par(mfrow=c(1,1))
mtext(expression(paste("500 confidence intervals for ",mu)),side=3,cex=1.5,xpd=TRUE,line=4)
mtext(expression(paste("(",sigma," known)")),side=3,cex=1.3,xpd=TRUE,line=2.7)


## ------------------------------------------------------------------------
par(mfrow=c(1,5), las=1, mar=c(5.1,2.1,6.1,2.1))
for(i in 1:p) {
  plot(0,0,type="n",ylim=0.5+c(0,n),xlim=r,ylab="",xlab="",yaxt="n")
  abline(v=3.5,lty=2,col="red",lwd=2)
  segments(lo2[[i]],1:n,hi2[[i]],1:n,lwd=2)
  o <- (1:n)[lo2[[i]] > 3.5 | hi2[[i]] < 3.5]
  segments(lo2[[i]][o],o,hi2[[i]][o],o,lwd=2,col="orange")
}
par(mfrow=c(1,1))
mtext(expression(paste("500 BAD confidence intervals for ",mu)),side=3,cex=1.5,xpd=TRUE,line=4)
mtext(expression(paste("(",sigma," unknown)")),side=3,cex=1.3,xpd=TRUE,line=2.7)


## ------------------------------------------------------------------------
par(mfrow=c(1,5), las=1, mar=c(5.1,2.1,6.1,2.1))
for(i in 1:p) {
  plot(0,0,type="n",ylim=0.5+c(0,n),xlim=r,ylab="",xlab="",yaxt="n")
  abline(v=3.5,lty=2,col="red",lwd=2)
  segments(lo3[[i]],1:n,hi3[[i]],1:n,lwd=2)
  o <- (1:n)[lo3[[i]] > 3.5 | hi3[[i]] < 3.5]
  segments(lo3[[i]][o],o,hi3[[i]][o],o,lwd=2,col="orange")
}
par(mfrow=c(1,1))
mtext(expression(paste("500 confidence intervals for ",mu)),side=3,cex=1.5,xpd=TRUE,line=4)
mtext(expression(paste("(",sigma," unknown)")),side=3,cex=1.3,xpd=TRUE,line=2.7)


## ------------------------------------------------------------------------
x <- seq(-4,4,length=501)
plot(x,dnorm(x),type="l",xlab="",ylab="",yaxt="n",bty="n")
lines(x,dt(x,2),col="blue")
lines(x,dt(x,4),col="red")
lines(x,dt(x,14),col="green")
lines(x,dnorm(x),col="black")
abline(h=0)
legend(2,0.36,c("df=2","df=4","df=14","normal"),col=c("blue","red","green","black"),lwd=1)

x <- seq(-5,5,length=501)
z <- qt(0.975,3)
plot(x,dt(x,3),type="l",xlab="",ylab="",yaxt="n",bty="n")
a <- seq(z,5,length=501)
X <- c(a,a[1])
Y <- c(dt(a,3),0)
polygon(X,Y,col="gray40")
abline(h=0)
segments(z,0,z,-0.05,col="blue",lwd=2,xpd=TRUE)
segments(z,0,z,-0.05,col="blue",lwd=2,xpd=TRUE)
text(z,-0.1,expression(t(alpha/2,n-1)),col="blue",xpd=TRUE)
text(4,0.04, expression(alpha/2), col="red")


## ------------------------------------------------------------------------

# Data
x <- c(0.2, 1.3, 1.4, 2.3, 4.2, 4.7, 4.7, 5.1, 5.9, 7.0)

# mean and SD
xm <- mean(x)
xs <- sd(x)
xn <- length(x)

# A plot of the data
stripchart(x, method="jitter", pch=1)
abline(h=1,lty=2,col="gray40")
segments(xm, 0.9, xm, 1.1, col="blue", lwd=2)

# t value
tval <- qt(0.975, xn - 1)

# estimated SE of the difference between the means
se <- xs / sqrt(xn)

# confidence interval
round( xm  + c(-1,1)*(tval*se), 2)

# Alternatively, you can use the function t.test()
t.test(x)
t.test(x)$conf.int


## ------------------------------------------------------------------------
# Example data
x <- c(1.17, 6.35, 7.76)

# 95% CI for population mean 
t.test(x)


## ------------------------------------------------------------------------
x <- c(34.9, 28.5, 34.3, 38.4, 29.6,
       29.6, 38.7, 22.4, 30.1, 23.1,
       29.6, 33.4, 20.6, 33.6, 42.4,
       28.2, 25.3, 22.1, 37.3, 32.1)

# mean, sd
mean(x)
sd(x)

# standard error
sd(x)/sqrt(length(x))

# confidence interval
t.test(x)$conf.int
           
# a plot
stripchart(x, method="jitter", pch=1)
abline(h=1, lty=2, col="gray")
segments(mean(x), 0.9, mean(x), 1.1, col="blue", lwd=2)


## ------------------------------------------------------------------------
# Data
x <- c(2.78, 5.23, 2.03, 4.78, 3.80, 2.57, 4.63, 1.93, 1.30)
y <- c(3.70, 4.77, 4.59, 5.82, 6.51, 3.88)

# means and SDs
xm <- mean(x)
ym <- mean(y)
xs <- sd(x)
ys <- sd(y)
xn <- length(x)
yn <- length(y)

# A plot of the data
z <- rep(0:1,c(length(x),length(y)))
plot(c(x,y),z,ylim=c(-0.5,1.5),yaxt="n",ylab="",xlab="",lwd=2)
abline(h=0:1,lty=2,col="gray40")
segments(xm, -0.1, xm, 0.1, col="blue", lwd=2)
segments(ym, 0.9, ym, 1.1, col="red", lwd=2)

# pooled estimate of the common population SD
spooled <- sqrt( (xs^2*(xn-1) + ys^2*(yn-1)) / (xn + yn - 2) )

# t value
tval <- qt(0.975, xn + yn - 2)

# estimated SE of the difference between the means
se <- spooled * sqrt(1/xn + 1/yn)

# confidence interval
round( (xm - ym) + c(-1,1)*(tval*se), 2)

# Alternatively, you can use the function t.test()
t.test(x,y,var.equal=TRUE)
t.test(x,y,var.equal=TRUE)$conf.int


## ------------------------------------------------------------------------
# Example data
x <- c(59.2, 54.6, 58.1, 41.8, 64.7, 50.8, 51.6, 47.0, 66.3, 58.1)
y <- c(73.3, 91.4, 69.1, 104.2, 64.6, 78.4, 56.4, 55.6, 61.1, 39.8,
       43.9, 63.6, 78.6, 76.5, 45.1, 90.2)

# 95% CI for mean of the first population
t.test(x)

# 95% CI for mean of the second population
t.test(y)

# 95% CI for difference in means assuming pop'n SDs are equal
t.test(x, y, var.equal=TRUE)

# 95% CI for difference in means not assuming pop'n SDs are equal
t.test(x, y)


## ------------------------------------------------------------------------
# 95% CI for SD of first population
sd(x) * sqrt( (length(x)-1) / qchisq(c(0.975,0.025), length(x)-1) )

# 95% CI for SD of first population
sd(y) * sqrt( (length(y)-1) / qchisq(c(0.975,0.025), length(y)-1) )


