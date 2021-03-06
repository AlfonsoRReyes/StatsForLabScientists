## ------------------------------------------------------------------------
runif(10)
x <- matrix(runif(10*20),ncol=10)
x
dim(x)

# first column
x[,1]
# first row
x[1,]
mean(x[1,])
apply(x,1,mean)[1]

?apply
apply(x,1,mean)
apply(x,1,median)

## ------------------------------------------------------------------------
nit <- 10000
n <- 10
set.seed(51)

x <- matrix(runif(n*nit),ncol=n)
xA <- apply(x,1,mean)

x <- matrix(runif(n*nit),ncol=n)
xB <- apply(x,1,median)

n <- 100

x <- matrix(runif(n*nit),ncol=n)
xC <- apply(x,1,mean)

x <- matrix(runif(n*nit),ncol=n)
xD <- apply(x,1,median)

par(mfcol=c(2,2),las=1)
hist(xA,breaks=seq(0,1,length=41),col="lightgrey",main="A:  Mean  n=10",xlab="",ylim=c(0,1200),cex.main=1.2)
hist(xB,breaks=seq(0,1,length=41),col="lightgrey",main="B:  Median  n=10",xlab="",ylim=c(0,1200),cex.main=1.2)
hist(xC,breaks=seq(0,1,length=41),col="lightgrey",main="C:  Mean  n=100",xlab="",ylim=c(0,3000),cex.main=1.2)
hist(xD,breaks=seq(0,1,length=41),col="lightgrey",main="D:  Median  n=100",xlab="",ylim=c(0,3000),cex.main=1.2)

round(mean(xA),3)
round(mean(xB),3)
round(mean(xC),3)
round(mean(xD),3)

round(sd(xA),2)
round(sd(xB),2)
round(sd(xC),2)
round(sd(xD),2)

## ------------------------------------------------------------------------
p <- c(0.3,0.25,0.25,0.2)
m <- c(10,15,25,35)
s <- c(3,4,6,7)

## ------------------------------------------------------------------------
x <- seq(0,59,by=0.1)
f <- rep(0,length(x))
for(i in 1:length(m)) f <- f + p[i] * dnorm(x, m[i], s[i])

## ------------------------------------------------------------------------
plot(x,f,type="l",lwd=2,yaxt="n",ylab="",bty="n",ylim=c(0,max(f)),xlab="")

## ------------------------------------------------------------------------
M <- sum(m*p)
S <- sqrt(sum(s^2*p) + sum(m^2*p) - sum(m*p)^2)

## ------------------------------------------------------------------------
u <- par("usr") # gets the actual limits of figure region
plot(x,f,type="l",lwd=2,yaxt="n",ylab="",bty="n",ylim=c(0,max(f)),xlab="")
segments(M,u[3],M,u[3]+0.002,xpd=TRUE,lwd=2,col="blue")
text(M,u[3]+0.004,expression(mu),cex=1.3,col="blue")
segments(c(M,M,M+S), u[3]+0.01-c(0,0.001,0.001),c(M+S,M,M+S), u[3]+0.01+c(0,0.001,0.001),
         lwd=2,col="red")
text(M+S/2,u[3]+0.012,expression(sigma),cex=1.3,col="red")

## ------------------------------------------------------------------------
sammix <- function(n=10,m,s,p){
  a <- 1:length(m)
  x <- sample(1:length(m),n,rep=TRUE,prob=p)
  rnorm(n,m[x],s[x])
}

## ------------------------------------------------------------------------
sammix(10,m,s,p)
x <- sammix(10000,m,s,p)
hist(x,breaks=101,col="lightgray")

## ------------------------------------------------------------------------
x5 <- matrix(sammix(10000*5,m,s,p),ncol=5)
x10 <- matrix(sammix(10000*10,m,s,p),ncol=10)
x25 <- matrix(sammix(10000*25,m,s,p),ncol=25)
x100 <- matrix(sammix(10000*100,m,s,p),ncol=100)
head(x5)
head(x10)

## ------------------------------------------------------------------------
m5 <- apply(x5,1,mean)
m10 <- apply(x10,1,mean)
m25 <- apply(x25,1,mean)
m100 <- apply(x100,1,mean)

## ------------------------------------------------------------------------
r <- range(c(m5,m10,m25,m100)) # range of sample means
br <- seq(r[1],r[2],length=101) # defines bins in histograms
par(mfrow=c(2,2)) # results in 2 rows and 2 columns of plots
hist(m5,breaks=br,xlab="",ylab="",main="n=5",yaxt="n")
abline(v=M,col="blue",lwd=2)
hist(m10,breaks=br,xlab="",ylab="",main="n=10",yaxt="n")
abline(v=M,col="blue",lwd=2)
hist(m25,breaks=br,xlab="",ylab="",main="n=25",yaxt="n")
abline(v=M,col="blue",lwd=2)
hist(m100,breaks=br,xlab="",ylab="",main="n=100",yaxt="n")
abline(v=M,col="blue",lwd=2)

## ------------------------------------------------------------------------
s5 <- apply(x5,1,sd)
s10 <- apply(x10,1,sd)
s25 <- apply(x25,1,sd)
s100 <- apply(x100,1,sd)

## ------------------------------------------------------------------------
rs <- range(c(s5,s10,s25,s100)) # range of sample means
brs <- seq(rs[1],rs[2],length=101) # defines bins in histograms
par(mfrow=c(2,2)) # results in 2 rows and 2 columns of plots
hist(s5,breaks=brs,xlab="",ylab="",main="n=5",yaxt="n")
abline(v=S,col="red",lwd=2)
hist(s10,breaks=brs,xlab="",ylab="",main="n=10",yaxt="n")
abline(v=S,col="red",lwd=2)
hist(s25,breaks=brs,xlab="",ylab="",main="n=25",yaxt="n")
abline(v=S,col="red",lwd=2)
hist(s100,breaks=brs,xlab="",ylab="",main="n=100",yaxt="n")
abline(v=S,col="red",lwd=2)

## ------------------------------------------------------------------------

# Data
x <- c(0.2, 1.3, 1.4, 2.3, 4.2, 4.7, 4.7, 5.1, 5.9, 7.0)

# Mean and SD
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
t.test(x)$conf.int[1:2]
round(t.test(x)$conf.int[1:2],2)

## ------------------------------------------------------------------------
# Example data
x <- c(1.17, 6.35, 7.76)

# 95% CI for population mean 
t.test(x)$conf.int

## ------------------------------------------------------------------------
x <- c(34.9, 28.5, 34.3, 38.4, 29.6,
       29.6, 38.7, 22.4, 30.1, 23.1,
       29.6, 33.4, 20.6, 33.6, 42.4,
       28.2, 25.3, 22.1, 37.3, 32.1)

# Mean, SD
mean(x)
sd(x)

# Standard error
sd(x)/sqrt(length(x))

# Confidence interval
t.test(x)$conf.int
t.test(x,conf=0.9)$conf.int
t.test(x,conf=0.99)$conf.int

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

