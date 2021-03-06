
## ------------------------------------------------------------------------
x <- c(0.008, 0.018, 0.056, 0.055, 0.135,
       0.052, 0.077, 0.026, 0.044, 0.300,
       0.025, 0.036, 0.043, 0.100, 0.120,
       0.110, 0.100, 0.350, 0.100, 0.300,
       0.011, 0.060, 0.070, 0.050, 0.080,
       0.110, 0.110, 0.120, 0.133, 0.100,
       0.100, 0.155, 0.370, 0.019, 0.100,
       0.100, 0.116)

# histograms with different numbers of bins
hist(x,breaks=5)
hist(x,breaks=15)

# plot values by index
plot(x)

# dot plot with jittered y values
#    - runif draws random numbers between 0 and 1
#    - yaxt="n" prevents y-axis from being plotted
#    - ylim changes the limits of the y-axis
#    - ylab="" prevents y label from being plotted
#    - abline(h=0.5) plots a horizontal line at 0.5
y <- runif(length(x))
plot(x,y,ylim=c(-2,3),yaxt="n",ylab="")
abline(h=0.5,lty=2,col="gray70")

# there's also a built-in function to do this, "stripchart"
#     - method="jitter" jitters the values
#     - pch=1 plots circles (the default is pch=0, squares)
#     - I still like to add a horizontal line
stripchart(x,method="jitter",pch=1)
abline(h=1,lty=2,col="gray70")

# the same plot with x-axis on log scale
plot(x,y,ylim=c(-2,3),yaxt="n",ylab="",log="x")
abline(h=0.5,lty=2,col="gray70")

# using "stripchart"
stripchart(x,method="jitter",pch=1,log="x")
abline(h=1,lty=2,col="gray70")

# boxplot
boxplot(x)

# boxplot without the goofy outlier business
boxplot(x,range=0)

# boxplot horizontally
boxplot(x,range=0,horizontal=TRUE)

# boxplot on log scale
boxplot(x,range=0,horizontal=TRUE,log="x")

# boxplot on log scale, blue
boxplot(x,range=0,horizontal=TRUE,log="x",col="blue")

# boxplot on log scale, but not horizontal
boxplot(x,range=0,log="y",col="blue")


## ------------------------------------------------------------------------
x <- seq(2, 40, by=2)
length(x)
x[5]
x[c(1,3,9)]
x[-(1:10)]
x[-5]

z <- c(1, 3, 5, 9)
z
z[2] <- -3
z

y <- c(rep(TRUE,4), rep(FALSE,14), TRUE, TRUE)
length(y)
x[y]

a <- c(rep(c(TRUE, FALSE), 2), NA)
b <- c(rep(c(TRUE, FALSE), c(2,2)), FALSE)
a
b
!a
a & b
a | b
!(a | b)
!a | b

x <- c(1,5,3,NA,9,11,2,3)
x<=5
x>3 & x<11
is.na(x)
x[!is.na(x)]
x[!is.na(x) & x<5]


## ------------------------------------------------------------------------
# variables
letters
LETTERS

x <- c(1,2,3,4)
x
x/2
y <- sqrt(x)
y
x <- rnorm(100)
x
summary(x) 

# user-defined functions
mean(x)
sum(x)/length(x)

myMean <- function(x){
  sum(x)/length(x)
  }

myMean(x)
myMean(y)
myMean(1:100)

# cleaning up
objects()
ls()
rm(y,z)
ls()

# end session
# q()


