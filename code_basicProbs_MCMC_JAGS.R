## some basic stuff + simple probs & stats
## for loops
## plot functions
## prob distr functions

# Monte Carlo
k <- 500
nrep <- 10000
maxs <- rep(0, nrep)
{for(i in 1:nrep)
  birthdays <- sample(1:365, size = k, replace = TRUE)
  maxs[i] <- max(table(birthdays))
}

birthdays
table(birthdays)
max(table(birthdays))
sum(maxs)
mean(maxs)

sum(maxs >= 2)
mean(maxs >= 2)


# Markov Chain
P = matrix(c(0,1,9,.2,0,.8,.1,.3,.6),nrow = 3,byrow = T)
n = 100
path = numeric(n)
pi0 = c(0,0,1)
x = sample(1:3, 1, prob = pi0)
path[1] = x
for (i in 2:n) {
  x = sample(1:3, 1, prob = P[x,])
  path[i] = x
}
path


#Hidden Markov Model example - JAGS model
install.packages("rjags", configure.args="--enable-rpath")


model{
  x[1] ~ dcat(pi1)
  for(i in 2:n){x[i] ~ dcat(A[x[i-1],])}
  for(i in 1:n){y[i] ~ dcat(B[x[i],])}
  for(i in 1:2){pi1[i] <-0.5}
  p ~ dunif(0,1)
  q ~ dunif(0,1)
  A[1,1] <-1-p
  A[1,2] <-p
  A[2,1] <-q
  A[2,2] <-1-q
  for(j in 1:6){
    b1[j] ~ dexp(1)
    b2[j] ~ dexp(1)
    B[1,j] <-b1[j] / sum(b1)
    B[2,j] <-b2[j] / sum(b2)
  }
}

Bmeans = apply(r[,1:12], 2, mean)
Bmeans = matrix(Bmeans, nrow = 2, byrow = T)
round(Bmeans,2)

install.packages('swirl')
library(swirl)
swirl()
?swirl()
?runif
z <- runif(12)
z<0.5
y <- sample(10, 10, replace = TRUE)
y
y2 <- sample(10, 10, replace = FALSE)
y2
table(y)
table(y2)
max(y)
y
1-pbinom(2,5,0.1)
dbinom(2,5,0.1)
plot(0:50, dbinom(0:50,50,.6), type="h")
plot(0:50, dbinom(0:50,50,.6))
plot(0:50, pbinom(0:50,50,.6))
points(0:50, dbinom(0:50,50,.6), pch=20)

curve(x^18*(1-x)^3, 0, 1, xlab='theta', ylab='L(theta)')

thetas <- seq(0, 1, by=.001)
k <- length(thetas)
liks <- rep(0,k)
for(i in 1:k){
  liks[i] <- dbinom(18, 21, thetas[i])
}
plot(thetas, liks, type='l', xlab='theta', ylab='L(theta)')

plot(thetas, liks, type='l', xlab='theta', ylab='L(theta)')
abline(v = 18/21)

thetas <- seq(0, 1, .01)
post <- thetas^18 * (1-thetas)^3
post <- post/sum(post)
plot(thetas, post, type="h")
points(thetas, post, pch=20, cex=0.5)

sum(post[thetas>0.5])
?points
?abline

plot(dcauchy,-4,4)
1-2*pcauchy(-4)


#spinner game
n = 20
nrep = 1000
results20 = numeric(nrep)
for(i in 1:nrep){
  spins = sample(c(0,2,9),n,prob=c(.5,.3,.2),replace=T)
  results20[i] = mean(spins)
}
results20
mean(results20)
hist(results20)
quantile(results20,c(.05,.95))

n = 2000
nrep = 1000
results2000 = numeric(nrep)
for(i in 1:nrep){
  spins = sample(c(0,2,9),n,prob=c(.5,.3,.2),replace=T)
  results2000[i] = mean(spins)
}
hist(results2000)
quantile(results2000,c(.05,.95))


x <- 0:100
p <- 0.5 # can also use other p's, e.g., 0.6 or 0.7
y <- dbinom(x,100,p)
plot(x,y)
?dbinom

logy <- log(y)
plot(x,logy)


mu1 <- 0; sig1 <- 1
mu2 <- 6; sig2 <- .33
a <- mu1-3.5*sig1
b <- mu2+5*sig2
plot(c(a,b), c(0,1.05*dnorm(mu2,mu2,sig2)), type='n', yaxs='i', xlab='x', ylab='f(x)')
curve(dnorm(x,mu1,sig1), a, mu2-5*sig2, add=T, col='red')
curve(dnorm(x,mu2,sig2),mu2-5*sig2,b,add=T, col='blue')
legend("topleft", legend = c(expression(N(0,1^2)),expression(N(6,0.33^2))),
       col=c('red','blue'), lty=c(1,1), cex=0.5)

pnorm(615, 505, 110)
?pnorm


dat <- read.csv('http://www.stat.yale.edu/~jtc5/238/data/RainAndSeedingClouds.csv')
rain <- dat[1:26,1]
hist(rain)
qqnorm(rain)
?qqnorm
lograin <- log(rain)
hist(lograin)
qqnorm(lograin)


#How to simulate a MC
P <- matrix(c(0,1,0,.2,0,.8,.1,.3,.6),nrow=3,byrow=T)
P
n <- 10000
x <- numeric(n)
pi0 <- c(.5, 0, .5)
state <- sample(1:3, 1, prob=pi0)
x[1] <- state
for(i in 2:n){
  state <- sample(1:3, 1, prob = P[state,])
  x[i] <- state
}
step <- data.frame(x)


matpow = function(M,n) {
  # Finds matrix power M^n.
  # Use this for n = integer greater than 1.
  ans = M
  for(i in 1:(n-1)) {
    ans = ans %*% M
  }
  return(ans)
}
c(0,1,0) %*% matpow(P,2)
matpow(P,3)
c(0,1,0) %*% matpow(P,3)
matpow(P,50)
c(0,1,0) %*% matpow(P,50)


#MCMC example
x0 <- matrix(c(8,1,4,2,7,1,3,1,1),nrow = 3)
x0
nr <- dim(x0)[1] # number of rows in x0; here 3
nc <- dim(x0)[2] # number of columns in x0; here 3
delta1 <- matrix(c(1,-1,-1,1),nrow=2)
delta2 <- matrix(c(-1,1,1,-1),nrow=2)
delta1
delta2

move <- function(x, print=T) {
  nr <- dim(x)[1]
  nc <- dim(x)[2]
  rows <- sample(nr,2)
  i1 <- min(rows)
  i2 <- max(rows)
  cols <- sample(nc,2)
  j1 <- min(cols)
  j2 <- max(cols)
  change <- F
  if(runif(1) < 0.5){
    if(min(x[i1,j2],x[i2,j1]) > 0){
      change <- T
      x[c(i1,i2),c(j1,j2)] <- x[c(i1,i2),c(j1,j2)] + delta1
    }
  } else{
    if(min(x[i1,j1],x[i2,j2]) > 0){
      change <- T
      x[c(i1,i2),c(j1,j2)] <- x[c(i1,i2),c(j1,j2)]+ delta2
    }
  }
  if(print){
    cat(paste("(i1,i2)=(",i1,",",i2,") (j1,j2)=(",j1,",",j2,")",sep=""))
    if(!change)cat(" no change")
    cat("\n")
    print(x)
  }
  return(x)
}

x <- x0
x <- move(x)
#incomplete...

#MCMC via Metropolis-Hastings
#Example [Implementing the previous example in R]
f <- function(x){
  (x > 0) * (x < 3) * (10*x - 3*x^2 ) * sin(x-2)^2 / sqrt(1 + exp(tan(x)))
}

c <- integrate(f, 0, 3)$value
c

nit <- 200000
path <- rep(0,nit)
x <- 2
path[1] <- x
scale <- 1
for(i in 2:nit){
  y <- runif(1, x-scale, x+scale)
  r <- f(y)/f(x)
  if(runif(1) < r) x <- y
  path[i] <- x
}

plot(path, pch=".")
hist(path, n=200, col='skyblue3')

hist(path, n=200, col='skyblue3', probability=TRUE)
c <- integrate(f, 0, 3)$value
curve(f(x)/c, add=TRUE, col='red', lwd=2, n=1000)


#....

#Ch5.Data analysis and statistical inference using MCMC
dat <- read.csv("http://www.stat.yale.edu/~jtc5/238/data/subliminal.csv")
dat
imp <- dat$post - dat$pre # improvements
imp
trt <- imp[dat$group == "t"]
ctrl <- imp[dat$group == "c"]
trt

par(mfrow=c(2,1))
hist(trt, 100, col="red")
hist(ctrl, 100, col="red")

rg <- range(imp)
par(mfrow=c(2,1))
hist(trt,100,col="red",xlim=rg)
hist(ctrl,100,col="red",xlim=rg)

#check page180 for the rest of it...

#Introducing JAGS
install.packages('rjags')

library(rjags)
x <- 55 # data
mymodel <- "
model{
x ~ dbin(p, 100)
p ~ dunif(0,1)
}
"
jm <- jags.model(textConnection(mymodel), data=list(x=x))
cs <- coda.samples(jm, c("p"), 10000)
s <- as.data.frame(cs[[1]])
hist(s$p)
#...


#Regression
dat <- read.csv("http://www.stat.yale.edu/~jtc5/238/data/crying.csv")
dat

plot(x=dat$crying, y=dat$IQ)

m <- lm(dat$IQ ~ dat$crying)
m
plot(x=dat$crying, y=dat$IQ)
abline(m, col="red")
#....


#Correlation
fs <- read.csv("http://www.stat.yale.edu/~jtc5/238/data/pearson-heights.csv")
nrow(fs) # the number of father-son pairs
head(fs) # the first 6 rows of the data

x <- fs$fathers
y <- fs$sons
xbar=mean(x)
sx=sd(x)
ybar=mean(y)
sy=sd(y)
r=cor(x,y)
xbar
r

plot(x, y, xlab="x=fathers", ylab="y=sons", pch=20, cex=.2)
abline(ybar-sy*xbar/sx, sy/sx, col="red")
abline(ybar-r*sy*xbar/sx, r*sy/sx, col="blue")


par(mfrow=c(1,2))
f = function(x,y){
  1/(2*pi*sqrt(1-rho^2))*exp(-(x^2-2*rho*x*y+y^2)/(2*(1-rho^2)))
}
L=50
x = seq(-3,3,length=L)
y = seq(-3,3,length=L)
rho <- 0.5
z = outer(x,y,f)
persp(x,y,z,phi=30,theta=-30,expand=.5,col = "lightblue",ticktype = "detailed",
      main=bquote(rho==.(rho)))
rho <- 0.9
z = outer(x,y,f)
persp(x,y,z,phi=30,theta=-30,expand=.5,col = "lightblue",ticktype = "detailed",
      main=bquote(rho==.(rho)))


#Multiple linear regression
d1 <- read.csv("http://www.stat.yale.edu/~jtc5/238/data/salaries-and-gender-1.csv")
d1
salary <- d1$salary; gender <- d1$gender
male <- as.numeric(gender=='male')
male
plot(male, salary)


m0 <- "
model{
for(i in 1:n1){x1[i] ~ dnorm(mu1, tau1)}
for(i in 1:n2){x2[i] ~ dnorm(mu2, tau2)}
mu1 ~ dnorm(0,1.0E-14)
mu2 ~ dnorm(0,1.0E-14)
tau1 ~ dgamma(0.01,0.01)
tau2 ~ dgamma(0.01,0.01)
}
"

library(rjags)
x1 <- salary[gender=="female"]
n1 <- length(x1)
x2 <- salary[gender=="male"]
n2 <- length(x2)
jm <- jags.model(textConnection(m0), 
                 data=list(x1=x1, x2=x2, n1=n1, n2=n2))
cs <- coda.samples(jm, var=c("mu1","mu2","tau1","tau2"), 100000)
summary(cs)

s <- as.data.frame(cs[[1]])
names(s)

delta <- s$mu2 - s$mu1
hist(delta, col=5)
mean(delta > 0)
quantile(delta, c(.05,.95))
quantile(delta, c(.025,.975))
#So our posterior probability that the expected male salary is 
#greater than the expected female salary is nearly 97%.



d2 <- read.csv("http://www.stat.yale.edu/~jtc5/238/data/salaries-and-gender-2.csv")
d2

m1 <- "
model{
for(i in 1:12){
salary[i] ~ dnorm(b0 + b1*male[i] + b2*experience[i], tau)
}
b0 ~ dnorm(0.0, 1.0E-14)
b1 ~ dnorm(0.0, 1.0E-14)
b2 ~ dnorm(0.0, 1.0E-14)
tau ~ dgamma(.01,.01)
}
"

jm1 <- jags.model(textConnection(m1),
                  data=list(salary=salary, male=male, experience=experience))

update(jm1, 10000)
cs1 <- coda.samples(jm1, c("b0","b1","b2","tau"), 100000)

summary(cs1)

s1 <- as.data.frame(cs1[[1]])
names(s1)

quantile(x = s1$b1, probs = c(.025, .975))

mean(s1$b1 > 0)


#....
#I am not focusing on Bayesian inference for the moment