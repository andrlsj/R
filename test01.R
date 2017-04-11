
name <- c("Brain","Toby","Sherry")
height <- c(180,169,173)
weight <- c(73,87,43)

re_height <- height/100
bmi = weight/(re_height^2)
names(bmi) <- name
name
bmi
bmi[bmi>24|bmi<18.5]

x = list(c(1,2,3,4),c(5,6,7,8),c(1,2,3,4))
lapply(x,sum)
sapply(x,sum)

x <- c("abc","cde","fghij")
length(x)
nchar(x[3])

#==============================
Student <- c("John Davis", "Angela Williams", 
             "Bullwinkle Moose", "David Jones", 
             "Janice Markhammer", "Cheryl Cushing",
             "Reuven Ytzrhak", "Greg Knox", "Joel England",
             "Mary Rayburn")
Math <- c(502, 600, 412, 358, 495, 512, 410, 625, 573, 522)
Science <- c(95, 99, 80, 82, 75, 85, 80, 95, 89, 86)
English <- c(25, 22, 18, 15, 20, 28, 15, 30, 27, 18)
roster <- data.frame(Student, Math, Science, English,stringsAsFactors=FALSE)
roster
#==================================

options(digits=3)

?option
attach(mtcars)
aggdata <-aggregate(mtcars, by=list(cyl,gear), 
                    FUN=mean, na.rm=TRUE)
aggdata
#==================================
install.packages("reshape")
#=================================
install.packages(c('vcd', 'plotrix', 'sm', 'vioplot')) 

# Load vcd package
library(vcd)

# Get cell counts for improved variable
counts <- table(Arthritis$Improved)
counts
#=====================================
vars <- c("mpg","hp","wt")
head(mtcars[vars])

#99
#method by vector
a=1:9
b=t(a)
c <- a%*%b
c
#method by matrix

mat = matrix(1:81,byrow = T,nrow=9)
for(i in seq_len(nrow(mat))){
  for(j in seq_len(ncol(mat))){
    mat[i,j]=i*j;
  }
}
mat
#method by sapply
sapply(1:9,function(x) x* 1:9)

#-------
for(i in 1:9){
  for(j in 1:9){
    #print(i,"*", j ,"=",i*j) 
    
  }
}

#match.text
filename="e:/r/match.txt"
mat1 = matrix(rep(-1,5^2),byrow = T,nrow=5,dimnames = list(c('A','B','C','D','E'),c('A','B','C','D','E')))
mat1

test.data = read.table(filename,header=F,sep="|")
str(test.data)
#A|B|1
#1 A B 1


for(i in 1:nrow(test.data)){
  mat1[test.data[i,1], test.data[i,2]]=test.data[i,3]  
}


mat1
#ABCDEF matrix function
filename="e:/r/match.txt"
match_func=function(filename){
  mat1 = matrix(rep(-1,5^2),byrow = T,nrow=5,dimnames = list(c('A','B','C','D','E'),c('A','B','C','D','E')))
  mat1
  
  test.data = read.table(filename,header=F,sep="|")
  str(test.data)
  #1 A B 1
  
  for(i in 1:nrow(test.data)){
    mat1[test.data[i,1], test.data[i,2]]=test.data[i,3]  
  }
  mat1
}
  
match_func(filename)
#------------------------------------------------
m1=matrix(1:4,byrow = TRUE,nrow = 2)
m2=matrix(5:8,byrow = T,nrow=2)
li= list(m1,m2)
li
sapply(li,mean)
sapply(li,function(e) e[1,])
#------------------------------------------------
m=matrix(1:4,byrow=T,nrow=2)
m
apply(m,1,sum)
apply(m,2,sum)

#------------------------------------------------
x <- c(80,70,59,88,72,57)
t <- c(1,1,2,1,1,2)
length(t)
tapply(x,t,mean)
#------------------------------------------------
data(iris)
str(iris)
tapply(iris$Sepal.Length,iris$Species,mean)
#------------------------------------------------
#method1
dd <- data.frame(x1=c(2,2,6,4),x2=c(3,4,2,8))

dd$sumx <- dd$x1 + dd$x2
dd$meanx <- (dd$x1+dd$x2)/2
dd$sumx

#method2
dd <- data.frame(x1=c(2,2,6,4),x2=c(3,4,2,8))
dd <- transform(dd,sumx=x1+x2,meanx=(x1+x2)/2)
dd$sumx
#------------------------------------------------
getwd()
load("E:\\R\\cdc.Rdata")
str(cdc)
head(cdc)

table(cdc$exerany)
table(cdc$exerany) / length(cdc$exerany) # % normal

table(cdc$gender,cdc$exerany) #gender exererice
table(cdc$height)
summary(cdc$height)
cut(cdc$height,seq(45,95,by=5),right = F) # [)
table(cut(cdc$height,seq(45,95,by=5),right=F,labels=seq(1,length(seq(45,95,by=5))-1)))
cdc$h_group = cut(cdc$height,seq(45,95,by=5),right=F)
str(cdc)

#----------------static graph--------------------------------
#---------barplot------------
barplot(table(cdc$smoke100))
?barplot

barplot(table(cdc$smoke100), xlab = "NoSomke", ylab = "Smoke",main = "Smoker in Taiwan", sub = "2099",col = "steelblue")

#---------PIE------------
pie(table(cdc$smoke100))
pie(table(cdc$genhlth),col=rainbow(5))
#----------Add Percentage--------------
pct = round(table(cdc$genhlth)/length(cdc$genhlth)*100,1)
labels = paste(names(pct),pct,"%")
pie(table(cdc$genhlth),labels = labels)

gender_smokers = table(cdc$gender,cdc$smoke100)
mosaicplot(gender_smokers)
#---------HIST------------
par(mfrow=c(3,1)) #make graph to 3 row
hist(cdc$height,breaks = 20)
hist(cdc$height,breaks = 30)
hist(cdc$height,breaks = 50)
#---------STEM------------
#stem(cdc$age) #too large
tmp=sample(cdc$age,100)
stem(tmp)
stem(sample(cdc$age,100))#change anytime
#---------boxplot------------
boxplot(cdc$weight)
boxplot(cdc$weight, horizontal=TRUE)
par(mfrow=c(1,1))
boxplot(cdc$weight ~ cdc$gender)
boxplot(cdc$height ~ cdc$gender)
#---------BMI------------
bmi = (cdc$weight/cdc$height^2) * 703
boxplot(bmi ~ cdc$genhlth)
par(mfrow=c(2,1)) 
plot(cdc$weight, cdc$height)
plot(cdc$weight, cdc$wtdesire)
#---------describe the data------------
a = c(100,120,130,110,100,90,80,90,100,110)
mean(a)
b=c(a,10000)
mean(b) #1002.727

sort(a)
median(a)
table(c(1,4,4,3))
which.max(table(c(1,4,4,3)))
names(which.max(table(c(1,4,4,3))))

#-----range IQR variance stardard deviation-------------
a = c(173,162,150,160,155,168,171,185,175,178,182) 
sort(a)
range(a)
quantile(a,0.25)
quantile(a,0.5)
quantile(a,0.75)
IQR(a)
fivenum(a) #minimum, lower-hinge, median, upper-hinge, maximum

boxplot(a,horizontal = T)
b = c(a,226)
b
range(b)
IQR(b)
boxplot(b,horizontal = T)

var_a=var(cdc$weight)
sqrt(var_a)

#--------ggplot-(geom-point)-----------------
install.packages('ggplot2')
library('ggplot2')
g <- ggplot(cdc,aes(x=height,y=weight))
g+geom_point(aes(col=gender))

#--------ggplot-(geom_bar)-----------------
g=ggplot(cdc,aes(x=genhlth))
g+geom_bar()+ylab('Count')+ggtitle('USA cdc info')
g+geom_bar(fill="steelblue",color="blue")

#--------ggplot-(geom_bar)-----------------
g=ggplot(cdc,aes(x=genhlth,fill=gender))
g+geom_bar()
#g <- ggplot(cdc,aes(x=genhlth))
#g+geom_bar(aes(fill=gender))
#--------ggplot-(geom_bar)-----------------
g_bygrp <- ggplot(cdc,aes(x=exerany,fill=genhlth))
g_bygrp + geom_bar()
par(mfrow=c(1,1))

g_bygrp + geom_bar(position='stack')
g_bygrp + geom_bar(position='dodge')
g_bygrp + geom_bar(position='identity')

#--------ggplot-(geom_bar)----precounted-------------
precounted = as.data.frame(table(cdc$genhlth,dnn = c('genhlth')))
precounted
ggplot(precounted,aes(x=genhlth,y=Freq))+ geom_bar(stat='identity') #note!!

g <- ggplot(cdc,aes(x=genhlth,y=height))
g + geom_boxplot()

#--------ggplot-facet------------
g <- ggplot(cdc.aes(x=weight))
g2=g+geom_histogram()+facet_wrap(~genhlth)
#--------ggplot-output------------
#ggsave(filename = 'data_info.png',plot = g2)

#--------palette--------------------------
barplot(1:4, col = c(2, "red", rgb(1, 0, 0), "#FF0000"))

palette() #system palette
barplot(1:8, col = c(1,2,3,4,5,6,7,8))

palette(rainbow(7)) #rainbow palette to 7 types
barplot(1:7, col = c(1,2,3,4,5,6,7))

palette(rainbow(12)) #rainbow palette to 12 types
barplot(1:12, col = c(1,2,3,4,5,6,7,8,9,10,11,12))

palette(heat.colors(12)) #warm colors
barplot(1:12, col = c(1,2,3,4,5,6,7,8,9,10,11,12))

palette(terrain.colors(12))
barplot(1:12, col = c(1,2,3,4,5,6,7,8,9,10,11,12))

palette(topo.colors(12))
barplot(1:12, col = c(1,2,3,4,5,6,7,8,9,10,11,12))

palette(cm.colors(12))
barplot(1:12, col = c(1,2,3,4,5,6,7,8,9,10,11,12))


#--------palette--draw something------------------------
n <- 10
mycolor <- gray(0:10/10) #between 0:1
pie(rep(1,n),labels=mycolor,col=mycolor)

mycolor2 <- cm.colors(n)  #value
pie(rep(1,n),labels = mycolor2,col = mycolor2)


mycolor2 <- heat.colors(n)  #value
pie(rep(1,n),labels = mycolor2,col = mycolor2)

#--------layout-------------------------------------
?layout
#layout(mat, widths = rep.int(1, ncol(mat)),
#       heights = rep.int(1, nrow(mat)), respect = FALSE)

#--------show graph 2*2----------------------------------------
attach(mtcars)
opar <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))
plot(wt, mpg, main = "Scatterplot of wt vs. mpg")
plot(wt, disp, main = "Scatterplot of wt vs disp")
hist(wt, main = "Histogram of wt")
boxplot(wt, main = "Boxplot of wt")
par(opar)
detach(mtcars)

#--------show graph 1-above-2----------------------------------------
attach(mtcars)
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE)) #
hist(wt)
hist(mpg)
hist(disp)
detach(mtcars)
#--------show graph 1-above-2----------------------------------------
attach(mtcars)
layout(matrix(c(1, 2, 3, 3), 2, 2, byrow = TRUE))
hist(wt)
hist(mpg)
hist(disp)
detach(mtcars)

#--------show graph 1-above-2----------------------------------------
attach(mtcars)
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE), 
       widths = c(3, 1), heights = c(1, 2))
hist(wt)
hist(mpg)
hist(disp)
detach(mtcars)
#--------show--more----------------------------------------------------
opar <- par(no.readonly = TRUE)
#--------------point-----------------------------------------------
par(fig = c(0, 0.8, 0, 0.8))
plot(mtcars$wt, mtcars$mpg, xlab = "Miles Per Gallon", 
     ylab = "Car Weight")
#-------------upper-box----------------------------------------
par(fig = c(0, 0.8, 0.55, 1), new = TRUE)
boxplot(mtcars$wt, horizontal = TRUE, axes = FALSE)
#-------------upper-box----------------------------------------
par(fig = c(0.65, 1, 0, 0.8), new = TRUE)
boxplot(mtcars$mpg, axes = FALSE)

mtext("Enhanced Scatterplot", side = 3, outer = TRUE, 
      line = -3)
par(opar)
 #--chap4---leadership--------------------------------------------
manager <- c(1, 2, 3, 4, 5)

date <- c("10/24/08", "10/28/08", "10/1/08", "10/12/08", 
          "5/1/09")

gender <- c("M", "F", "F", "M", "F")

age <- c(32, 45, 25, 39, 99)

q1 <- c(5, 3, 3, 3, 2)
q2 <- c(4, 5, 5, 3, 2)
q3 <- c(5, 2, 5, 4, 1)
q4 <- c(5, 5, 5, NA, 2)
q5 <- c(5, 5, 2, NA, 1)

leadership <- data.frame(manager,date,gender,age,
                         q1,q2,q3,q4,q5,stringsAsFactors = F)


#-----redit-value-----------------------------------------------------
#----fisrt-type--losse-type------------------
#variable[condition] <- expression

str(leadership)

leadership$agecat[leadership$age > 75] <- "Elder"
leadership$agecat[leadership$age >= 55 & leadership$age <=75] <- "Middle age"
leadership$agecat[leadership$age <=55] <- "young"
str(leadership) #now has new age Value Type

#do it in another way  function => within()
leadership <- within(leadership,{
  agecat[age>75] <- "elder"
  agecat[age<=75 & age>=55] <- "Middle"
  agecat[age<55] <- "young"
})

#change var name    function => fix() //editor
fix(leadership)

names(leadership)

is.na(leadership[,6:10]) #watch q1~q5

leadership$age[leadership$age == 99] <- NA
leadership

#---NA-clear-------------------
x <- c(1,2,NA,3)
y <- sum(x,na.rm = T)
y
leadership
newData <- na.omit(leadership)
newData
#----process-Date----------------------------
myformat <- "%m/%d/%Y"
leadership$date <- as.Date(leadership$date,myformat)

#----practice-----
#----date---
today <- Sys.Date()
format(today,format="%B %d %Y")

today <- Sys.Date()
dob <- as.Date("1956-10-12")
difftime(today,dob,units = "weeks")
format(dob,format("%A"))
#---
set.seed(1234)
c <- matrix(runif(12),nrow=3)
c

#------------------------
set.seed(1234)
dic=c(1:6)
n=100
t1 = sample(dic,n,replace=T)
t2 = sample(dic,100,replace=T)
tx = t1+t2
tx
table(tx)
table(tx)/length(tx)
barplot(table(tx)/length(tx))

#-----
rnorm(n=100000,mean=3,sd=2)
hist(rnorm(n=100000,mean=3,sd=2))

par(mfrow=c(3,1))
xlimits = range(-10,10)
hist(rnorm(10000,mean = 0,sd = 1),xlim=xlimits)
hist(rnorm(10000,mean = 0,sd = 2),xlim=xlimits)
hist(rnorm(10000,mean = 0,sd = 3),xlim=xlimits)

hist(rnorm(10000,mean = 0,sd=1),xlim=xlimits)
hist(rnorm(10000,mean = 3,sd=1),xlim=xlimits)
hist(rnorm(10000,mean = 6,sd=1),xlim=xlimits)

par(mfrow=c(1,1))
curve(dnorm(x,mean=2,sd=1),-5,5)

#=====================
load("e:/r/ames.Rdata")
area = ames$Gr.Liv.Area

sample_means10 = rep(NA, 5000)
sample_means50 = rep(NA, 5000)
sample_means100 = rep(NA, 5000)

for (i in 1:5000){
  samp = sample(area, 10,replace=F)
  sample_means10[i] = mean(samp)
  samp = sample(area, 50)
  sample_means50[i] = mean(samp)
  samp = sample(area, 100)
  sample_means100[i] = mean(samp)
}

par(mfrow = c(3, 1))
xlimits = range(sample_means10)
hist(sample_means10, breaks = 20, xlim = xlimits)
hist(sample_means50, breaks = 20, xlim = xlimits)
hist(sample_means100, breaks = 20, xlim = xlimits)

#example1: Check Cola is 300ml?
#H0: mu = 300
#H1: mu =\= 300

pop_mean <- 300
pop_sd <- 10
coke = c(278,289,291,291,291,285,295,278,304,287,291,287,288,300,309,280,294,283,292,306)

fivenum(coke)
hist(coke)
sde <- pop_sd/sqrt(length(coke))

z <- (mean(coke)-pop_mean)/sde
z

p <- (1-pnorm(abs(z)))*2
p


###corvariance

##1
x = c(160,170,180)
y = c(64,68,72)
cov(x,y)
plot(x,y)

##2 mtcars
data(mtcars)
cov(mtcars)
cpr(mtcars)

##3
gdp = read.csv("E:\\R\\data\\gdp.csv",header = T)
gdp = gdp[1:15,] #ignore NA row
gdp$GDP = as.numeric(sub(",","",gdp$GDP))
gdp$Export = as.numeric(sub(",","",gdp$Export))
cor(gdp$Export,gdp$GDP)

#
#install.packages("C50")
library(C50)

data(churn)
str(churnTrain)
!names(churnTrain) %in% c("state", "area_code", "account_length")

variable.list = !names(churnTrain) %in% c("state", "area_code", "account_length")
churnTrain= churnTrain[,variable.list]

str(churnTrain)

#set training and testing set
set.seed(2365)
ind = sample(1:2,size = nrow(churnTrain),replace = T,prob = c(0.7,0.3))
trainSet= churnTrain[ind==1,]
testSet = churnTrain[ind==2,]
trainSet
testSet

#rpart
install.packages('rpart')
library('rpart')

churn.rp = rpart(churn ~.,data=trainSet)
churn.rp
summary(churn.rp)

plot(churn.rp)
plot(churn.rp,margin = 0.1,uniform = T)
text(churn.rp,all=T,use.n=T)
#prune
which.min(churn.rp$cptable[,"xerror"]) #7
churn.cp = churn.rp$cptable[which.min(churn.rp$cptable[,"xerror"]), "CP"]
churn.cp
prune.tree = prune(churn.rp,cp=churn.cp)

plot(prune.tree,margin = 0.1)
text(prune.tree,all = T,use.n = T)

predictions = predict(prune.tree,testSet,type = "class")
table(predictions,testSet$churn)


#confusion
#install.packages('caret')
#install.packages('e1071')
#install.packages('lattice')
#install.packages('ggplot2')
library('caret')
library('e1071')
confusionMatrix(table(predictions,testSet$churn))

#CTree
install.packages('party')
library('party')
ctree.model = ctree(churn ~.,data = trainSet)
plot(ctree.model)

daycharge.model= ctree(churn ~ total_day_charge + international_plan, data = trainSet)
plot(daycharge.model)

ctree.predict = predict(ctree.model,testSet)
table(ctree.predict,testSet$churn)

confusionMatrix(table(ctree.predict,testSet$churn))

#C5.0
install.packages("C50")
library('C50')
c50.model = C5.0(churn ~.,data = trainSet)
c = C5.0Control(minCases = 20)
C50.model = C5.0(churn~.,data = trainSet,control=c)
plot(c50.model)

#K fold

#caret cross-validation
#install.packages('caret')
library('caret')
control = trainControl(method = "repeatedcv",number = 10,repeats = 3)
model = train(churn ~.,data = trainSet,method="rpart",trControl=control)
model
predictions = predict(model,testSet)
table(predictions,testSet$churn)

#04/11
library(C50)
data(churn)
variable.list = !names(churnTrain) %in% c('state','area_code','account_length')
churnTrain=churnTrain[,variable.list]
str(churnTrain)

set.seed(2)
#把資料分成training data 和 testing data
ind<-sample(1:2, size=nrow(churnTrain), replace=T, prob=c(0.7, 0.3))
trainset=churnTrain[ind==1,]
testset=churnTrain[ind==2,]

churn.rp<-rpart(churn ~., data=trainset)
churn.rp
summary(churn.rp)

par(mfrow=c(1,1))
plot(churn.rp, uniform=TRUE,branch = 0.6, margin=0.1)
text(churn.rp, all=TRUE, use.n=TRUE)

min(churn.rp$cptable[,"xerror"])
which.min(churn.rp$cptable[,"xerror"])
churn.cp = churn.rp$cptable[which.min(churn.rp$cptable[,"xerror"]), "CP"]
prune.tree=prune(churn.rp, cp=churn.cp)

plot(prune.tree, margin=0.1)
text(prune.tree, all=TRUE, use.n=TRUE, cex=0.7)

predictions <-predict(prune.tree, testset,type = "class")
table(testset$churn, predictions)

library(caret)
library(e1071)
confusionMatrix(table(predictions, testset$churn))


library(caret)
control=trainControl(method="repeatedcv", number=10, repeats=3)
#control=trainControl(method="repeatedcv", number=10, repeats=3,classProbs = TRUE,summaryFunction = twoClassSummary)
model =train(churn~., data=trainset, method="rpart", trControl=control)

predictions = predict(model, testset)

table(predictions,testset$churn)
confusionMatrix(table(predictions,testset$churn))

#ROC
install.packages("ROCR")
library(ROCR)
predictions = predict(churn.rp,testset,type = "prob")

head(predictions)
pred.to.roc<-predictions[, 1]
head(pred.to.roc)
?prediction
pred.rocr<-prediction(pred.to.roc, testset$churn)
pred.rocr
?performance
perf.tpr.rocr<-performance(pred.rocr, measure="tpr",x.measure="fpr")
perf.rocr<-performance(pred.rocr, measure ="auc", x.measure="cutoff")
plot(perf.tpr.rocr,colorize=T,main=paste("AUC:",(perf.rocr@y.values)))

#trees to curve
#rpart
library('rpart')
churn.rp<-rpart(churn ~., data=trainset)

#ctree
#install.packages("party")
library('party')
ctree.model = ctree(churn ~ . , data = trainset)

#C5.0
library(C50)
c50.model = C5.0(churn ~., data=trainset)

rp.predict.prob = predict(churn.rp, testset,type='prob')
c50.predict.prob = predict(c50.model,testset,type='prob')
ctree.predict.prob = sapply(predict(ctree.model ,testset,type='prob'),function(e){unlist(e)[1]})
rp.prediction = prediction(rp.predict.prob[,1],testset$churn)
c50.prediction = prediction(c50.predict.prob[,1],testset$churn)
ctree.prediction = prediction(ctree.predict.prob,testset$churn)
rp.performance = performance(rp.prediction, "tpr","fpr")
c50.performance = performance(c50.prediction, "tpr","fpr")
ctree.performance = performance(ctree.prediction, "tpr","fpr")
plot(rp.performance,col='red')
plot(c50.performance, add=T,col='green')
plot(ctree.performance, add=T,col='blue')

#
x =c(0, 0, 1, 1, 1, 1)
y =c(1, 0, 1, 1, 0, 1)

#euclidean
dist(rbind(x,y),method = "euclidean")
rbind(x,y)
sqrt(2)
dist(rbind(x,y), method ="minkowski", p=2)

#city block
dist(rbind(x,y), method ="manhattan")
sum(abs(x-y))
dist(rbind(x,y), method ="minkowski", p=1)

z = c(1,1,1,1,1,1)
rbind(rbind(x,y),z)
dist(rbind(rbind(x,y),z),method ="euclidean")

#clustering
customer=read.csv('E:\\R\\teacherData\\data\\customer.csv',header=TRUE)
head(customer)
str(customer)

#scale
customer_s = scale(customer[,-1])
round(mean(customer_s[,-1]))
round(sd(customer_s[,-1]))

#hclust
?hclust
hc = hclust(dist(customer_s,method = "euclidean"),method="ward.D2")
plot(hc,hang =-0.01,cex=0.7)

hc3 = hclust(dist(customer),method = "single")
plot(hc3,hang =-0.01,cex=0.7)

#form up to down
install.packages('cluster')
library(cluster)
?diana
dv =diana(customer_s, metric ="euclidean")
summary(dv)
plot(dv)

?cutree
fit = cutree(hc,k=4)
fit

str(customer_s)
set.seed(22)
fit =kmeans(customer_s, centers=4)
?kmeans

barplot(t(fit$centers), beside =TRUE,xlab="cluster", ylab="value")

#iris by kmeans
set.seed(22)
data(iris)
iris_s=scale(iris[,-5])
head(iris_s)

#ask 3 set
fit = kmeans(iris_s,3)
fit
barplot(t(fit$centers),beside = T,xlab="cluster", ylab="value")
plot(iris, col=fit$cluster)
plot(iris$Petal.Length, iris$Petal.Width, col=fit$cluster)

head(iris)
fit

#evaluate model

set.seed(22)
km =kmeans(customer_s, 4)
kms=silhouette(km$cluster,dist(customer_s))
summary(kms)
plot(kms)

#
nk=2:10
sapply(nk, function(k){})
set.seed(22)
WSS =sapply(nk, function(k){kmeans(customer_s, centers=k)$tot.withinss})
WSS
plot(x=nk, y=WSS, type="l", xlab="number of k", ylab="within sum of squares")

#
install.packages("fpc")

library(fpc)
#install.packages("robustbase", repos="http://R-Forge.R-project.org")
nk=2:10
SW =sapply(nk, function(k){set.seed(22);cluster.stats(dist(customer), kmeans(customer, centers=k)$cluster)$avg.silwidth})

kmeans(customer, centers=2)
?cluster.stats
cluster.stats(dist(customer), kmeans(customer, centers=2)$cluster)

plot(x=nk, y=SW, type="l", xlab="number of clusers", ylab="average silhouette width")

nk[which.max(SW)]
#密度方法
install.packages("mlbench")
# mlbench package provides many methods to generate simulated data with different shapes and sizes.
#In this example, we generate a Cassini problem graph
library(mlbench)
#install.packages("fpc")
library(fpc)
set.seed(2)
p = mlbench.cassini(500)
plot(p$x)

?mlbench.cassini

ds = dbscan(data = dist(p$x),eps= 0.2, MinPts = 4, method="dist")
ds
plot(ds, p$x)
















