
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
x5 <- matrix(sammix(10000*5,m,s,p),ncol=5)
x10 <- matrix(sammix(10000*10,m,s,p),ncol=10)
x25 <- matrix(sammix(10000*25,m,s,p),ncol=25)
x100 <- matrix(sammix(10000*100,m,s,p),ncol=100)


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


