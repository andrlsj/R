
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
#==================================

options(digits=3)
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





