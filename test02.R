x <- seq(1,8)
x
mean(x)
sd(x)
quantile(x,c(0.25,0.75))
rep(c(1,2,1), times=c(3,2,2))

#=============================================

matrix(1:9, byrow=TRUE, nrow=3)
matrix(1:9, nrow=3)
kevin = c(85,73)
marry = c(72,64)
jerry = c(59,66)
mat = matrix(c(kevin, marry, jerry), nrow=3, byrow= TRUE)
colnames(mat) = c('first', 'second')
rownames(mat) = c('kevin', 'marry', 'jerry')
mat

# basic
dim(mat)
nrow(mat)
ncol(mat)
t(mat) #transpose
mat[1,]
mat[,1]
mat[1:2,]
mat[c('kevin','jerry'),]
mat[c('kevin','jerry'),'first']
rowSums(mat)
colSums(mat)

# insert new value
mat2 = rbind(mat, c(78,63))
rownames(mat2)[nrow(mat2)] = 'sam'
mat2

mat3 = cbind(mat2,c(82,77,70,64))
colnames(mat3)[ncol(mat3)] = 'third'
mat3

rowMeans(mat3)
colMeans(mat3)


# arithmetic
m1 = matrix(1:4, byrow=TRUE, nrow=2)
m2 = matrix(5:8, byrow=TRUE, nrow=2)

m1 + m2
m1 - m2
m1 * m2
m1 / m2

m1 %*% m2


