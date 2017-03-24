patientID <- c(1, 2, 3, 4)
age <- c(25, 34, 28, 52)
diabetes <- c("Type1", "Type2", "Type1", "Type1")
status <- c("Poor", "Improved", "Excellent", "Poor")
diabetes <- factor(diabetes)
status <- factor(status, order = TRUE)
patientdata <- data.frame(patientID, age, diabetes, 
                          status)
str(patientdata)
summary(patientdata)


g <- "my first list"
h <- c(25,26,18,39)
j <- matrix(1:10,nrow = 5)
k <- c("one","two","three")
mylist <- list(title = g,ages=h,j,k)
mylist

mylist[["ages"]]

mydata <- data.frame(age=numeric(0),gender=character(0),weight=numeric(0))
mydata <- edit(mydata)

attach(mtcars)
plot(wt, mpg)
abline(lm(mpg ~ wt))
title("Regression of MPG on Weight")
detach(mtcars)

