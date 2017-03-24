kevin = c(85,73)
marry = c(72,64)
jerry = c(59,66)
mat = matrix(c(kevin, marry, jerry), nrow=3, byrow= TRUE)
mat

weight = c(0.4,0.6)
matAve = mat%*%weight
matAve

final=cbind(mat,matAve)
final

colnames(final) <- c("first","second","ave")
rownames(final) <- c("kevin","marry","jerry")
final

#================================================
weather= c("sunny","rainy", "cloudy", "rainy", "cloudy")
factor(weather)

weather= c("s","r", "c", "r", "c")
weather_factor = factor(weather)
levels(weather_factor) = c("cloudy","rainy","sunny")
weather_factor
#======================================
gender =c(0,1,0,1,0)
gender_factor = factor(gender)
gender_factor
levels(gender_factor) = c("Female","Male")
gender_factor
#=======================================
season = c(1,3,4,2,1)
seasonF = factor(season)
seasonF
levels(seasonF)=c("春","夏","秋","冬")
seasonF
#========================================
