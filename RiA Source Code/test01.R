patientID <- c(1, 2, 3, 4)
age <- c(25, 34, 28, 52)
diabetes <- c("Type1", "Type2", "Type1", "Type1")
status <- c("Poor", "Improved", "Excellent", "Poor")
patientdata <- data.frame(patientID, age, diabetes, 
                          status)
patientdata

patientdata[1:2]
patientdata[1:3]

patientdata$age

table(patientdata$diabetes,patientdata$status)

diabetes


sum <- 0
for(i in 1:100){
  sum = sum+i;
}

sum

m1 <- matrix(1:4,byrow = T,nrow = 2)
m2 <- matrix(5:8,byrow = T,nrow =2)

m1+m2
m1-m2
m1*m2
m1/m2
m1 %*% m2

kevin=c(85,73)
marry=c(72,64)
jerry=c(59,66)

mat <- matrix(c(kevin,marry,jerry),nrow=3,byrow=TRUE)
mat
colnames(mat) <- c('first','second')
rownames(mat) <- c('kevin','marry','jerry')
mat

mat2 <- matrix(c(kevin,marry,jerry),nrow = 3,byrow = TRUE,dimnames = list(c('kevin','marry','jerry'),c('first','second')))
mat2

mat3 <- rbind(mat2,c(78,63))
rownames(mat3)[nrow(mat3)]="sam"
mat3

mat4 <- cbind(mat2,c(82,77,70))
colnames(mat4)[ncol(mat4)]='third'
mat4

rowSums(mat2)

df=data.frame(a=c(1,2,3,4,5),b=c(2,3,4,5,6))
df

head(iris,10)
iris[1:3,]

x=list(c(1,2,3,4),c(5,6,7,8))
lapply(x,sum)

x = list(c(1,2,3),c(2,3,4),c(3,4,5))
lapply(x,sum)

m1=matrix(1:4,byrow = T,nrow = 2)
m2=matrix(5:8,byrow = T,nrow = 2)

li=list(m1,m2)
lapply(li,mean)

mat = matrix(1:9,byrow = T,nrow = 3)
mat
nrow(mat)
for(i in seq_len(nrow(mat))){
  for(j in seq_len(ncol(mat))){
    print(mat[i,j]*mat[i,j])
  }
}
#sprintf("%s !! my name is %s and my number is %i", a, b, c)

for(i in 1:9){
  for(j in 1:9){
    k=i*j;
    print(k)
    
  }
}

a="Hello"
b="EKA"
c=123456

sprintf("%s !! my name is %s and my number is %i", a, b, c)

name <- c("Steve") 
age=22 
cat("\tHello my name is", name ,"and I am", age ,"years old.\n") 

sapply(1:9,function(x) x*1:9)

a = 1:9
b = t(a)
a
b
a %*% b



A=1:9
B=1:9
outer(A,B)

mat = matrix(1:81, byrow= TRUE,nrow=9)
for(i in seq_len(nrow(mat))){
  for(j in seq_len(ncol(mat))){
    mat[i,j]=i*j
    
  }
}
mat

mat = matrix(1:81,byrow = T,nrow = 9)
mat

for(i in seq_len(nrow(mat))){
  for(j in  seq_len(ncol(mat))){
    mat[i,j]=i*j;
  }
}
mat

x<-matrix(1:12,ncol=3)
x
apply(x,1,sum)

x <- cbind(x1=3, x2=c(2:1,4:5))
x

x=list(c(1,2,3,4),c(5,6,7,8))
sapply(x,sum)
lapply(x,sum)

sapply(1:9,function(x) x*1:9)

x=c(80,70,59,88,72,57)
t=c(1,1,2,1,1,2)
tapply(x,t,mean)







