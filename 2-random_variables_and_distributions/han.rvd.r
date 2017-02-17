par(las=1,mfrow=c(3,2),cex.lab=1.3,oma=c(0,0,4,0))
x=seq(-2,12,length=1001)
plot(x,dnorm(x,5,2),type="l",xlab="x",ylab="f(x)",main="dnorm(x,mean=5,sd=2)")
plot(x,pnorm(x,5,2),type="l",xlab="x",ylab=expression(P(X <= x)),main="pnorm(x,mean=5,sd=2)")
xx=seq(-2,12,length=51)
plot(xx,dnorm(xx,5,2),xlab="xx",ylab="f(xx)",main="dnorm(seq(-2,12,length=51),mean=5,sd=2)")
plot(xx,pnorm(xx,5,2),xlab="xx",ylab=expression(P(X <= xx)),main="pnorm(seq(-2,12,length=51),mean=5,sd=2)")
p=seq(0,1,length=1001)
plot(p,qnorm(p,5,2),type="l",main="qnorm(p,mean=5,sd=2)",xlim=c(-0.2,1.2))
plot(1:50,rnorm(50,5,2),ylim=c(-2,12),xlab="50 random draws",main="rnorm(50,mean=5,sd=2)")
mtext(expression(paste("X", " ~ ","Normal(",mu,"=5,",sigma,"=2)",sep="")),outer=T,cex=1.5)

par(las=1,mfrow=c(3,2),cex.lab=1.3,oma=c(0,0,4,0))
x=seq(3,12,length=1001)
plot(x,dunif(x,5,10),type="l",xlab="x",ylab="f(x)",main="dunif(x,min=5,max=10)")
plot(x,punif(x,5,10),type="l",xlab="x",ylab=expression(P(X <= x)),main="punif(x,min=5,max=10)")
xx=seq(3,12,length=51)
plot(xx,dunif(xx,5,10),xlab="xx",ylab="f(xx)",main="dunif(seq(3,12,length=51),min=5,max=10)")
plot(xx,punif(xx,5,10),xlab="xx",ylab=expression(P(X <= xx)),main="punif(seq(3,12,length=51),min=5,max=10)")
p=seq(0,1,length=1001)
plot(p,qunif(p,5,10),type="l",main="qunif(p,min=5,max=10)",xlim=c(-0.2,1.2))
plot(1:50,runif(50,5,10),ylim=c(3,12),xlab="50 random draws",main="runif(50,min=5,max=10)")
mtext(paste("X", " ~ ","Uniform(5,10)",sep=""),outer=T,cex=1.5)

par(las=1,mfrow=c(3,2),cex.lab=1.3,oma=c(0,0,4,0))
barplot(dbinom(0:5,5,0.45),space=0.3,names=0:5,col="lightgrey",
        main="dbinom(0:5,5,0.45)",xlab="x",ylab="p(x)")
barplot(pbinom(0:5,5,0.45),space=0.3,names=0:5,col="lightgrey",
        main="pbinom(0:5,5,0.45)",xlab="x",ylab=expression(P(X <= x)))
xx=seq(-1,6,by=0.25)
plot(xx,dbinom(xx,5,0.45),main="dbinom(seq(-1,6,by=0.25),5,0.45)",ylab="p(xx)")
plot(xx,pbinom(xx,5,0.45),main="pbinom(seq(-1,6,by=0.25),5,0.45)",ylab=expression(P(X <= xx)))
p=seq(0,1,length=1001)
plot(p,qbinom(p,5,0.45),type="s",main="qbinom(p,5,0.45)",xlim=c(-0.2,1.2))
points(pbinom(0:4,5,0.45),0:4,pch=21,bg="black",cex=0.75)
points(pbinom(0:4,5,0.45),1:5,pch=21,bg="white",cex=0.75)
plot(1:50,rbinom(50,5,0.45),ylim=c(0,5),xlab="50 random draws",main="rbinom(50,5,0.45)")
mtext(paste("X", " ~ ","Binomial(n=5,p=0.45)",sep=""),outer=T,cex=1.5)

par(las=1,mfrow=c(3,2),cex.lab=1.3,oma=c(0,0,4,0))
barplot(dpois(0:8,2),space=0.3,names=0:8,col="lightgrey",
        main="dpois(0:8,2)",xlab="x",ylab="p(x)")
barplot(ppois(0:8,2),space=0.3,names=0:8,col="lightgrey",
        main="ppois(0:8,2)",xlab="x",ylim=c(0,1),ylab=expression(P(X <= x)))
xx=seq(-1,9,by=0.25)
plot(xx,dpois(xx,2),main="dpois(seq(-1,9,by=0.25),2)",ylab="p(xx)")
plot(xx,ppois(xx,2),main="ppois(seq(-1,9,by=0.25),2)",ylab=expression(P(X <= xx)))
p=seq(0,1,length=1001)
plot(p,qpois(p,2),type="s",main="qpois(p,2)",xlim=c(-0.2,1.2))
points(ppois(0:7,2),0:7,pch=21,bg="black",cex=0.75)
points(ppois(0:7,2),1:8,pch=21,bg="white",cex=0.75)
plot(1:50,rpois(50,2),ylim=c(0,8),xlab="50 random draws",main="rpois(50,2)")
mtext(expression(paste("X", " ~ ","Poisson(",lambda,"=2)",sep="")),outer=T,cex=1.5)













