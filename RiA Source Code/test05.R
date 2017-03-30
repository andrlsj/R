#----ch7-------
states <- state.x77[, 1:6]
cov(states)
cor(states)
cor(states, method="spearman")
#---------------------
cor.test(states[, 3], states[, 5])
#--------------------
install.packages("psych")
library(psych)
corr.test(states,use='complete')
#-----ch8---------------------
fit <- lm(weight ~ height, data = women)
summary(fit)
women$weight
fitted(fit)
residuals(fit)


rnorm(100)
hist(rnorm(100))
dnorm(0)
dnorm(1)
dnorm(-1)
dnorm(2)
dnorm(-2)
dnorm(0,mean=3,sd=5)
dnorm(3,mean=3,sd=5)

curve(dnorm,-3,3)
sample(1:10)
sample(1:10,size = 5)
sample(c(0,1),10,replace = T)
a <- sample.int(20,12)
length(a)
#
coins <- c("head","tail")
fair_coin <- sample(coins,1000,replace = T)
table(fair_coin)
#
outcome=c("heads","tails")
unfair_coin = sample(outcome,100,replace = T,prob = c(0.3,0.7))
table(unfair_coin)
#
rolling_dice = function(n){sample(1:6,size=n,rep=T)}
rolling_dice(100)
sum_of_two_dice = rolling_dice(10000) + rolling_dice(10000)
sum_of_two_dice
table(sum_of_two_dice)
barplot(table(sum_of_two_dice))
table(sum_of_two_dice) / length(sum_of_two_dice)

#
sum=sample(1:6,10000,rep=T)+sample(1:6,10000,rep=T)
sum

#
load("/Users/Albert/Documents/6_R_Tool/ames.RData")
str(ames)
#
b = rbinom(n = 20,size= 10,prob= 0.5)
table(b)
barplot(table(b))

par(mfrow=c(3,1))
xlimits = range(-10,10)
hist(rnorm(10000,mean = 0,sd = 1),xlim=xlimits)

curve(dnorm(x), -5, 5, col="black")
curve(dt(x, df=2), -5, 5, col="green", add=T)
curve(dt(x, df=10), -5, 5, col="red", add=T)
curve(dt(x, df=200), -5, 5, col="yellow", add=T)

#example1:300ml
#H0: mu = 300
#H1: mu =\= 300

pop_mean <- 300
pop_sd <- 10
coke = c(278,289,291,291,291,285,295,278,304,287,291,287,288,300,309,280,294,283,292,306)

sde <- pop_sd / sqrt(length(coke))

z <- (mean(coke) - pop_mean) / sde
z

##
states <- as.data.frame(state.x77[, c("Murder", "Population", 
                                      "Illiteracy", "Income", "Frost")])
install.packages("car")
cor(states)
library(car)
scatterplotMatrix(states, spread = FALSE, lty.smooth = 2, 
                  main = "Scatterplot Matrix")

b = rbinom(n = 20,size= 10,prob= 0.5)
table(b)
barplot(table(b))

