getwd()

twInfo = read.csv("data/2330.csv", header=TRUE) #csv
str(twInfo)

twInfo$Date
twInfo$ndate <- as.Date(twInfo$Date)
str(twInfo) #date format

dataInPer <- twInfo[twInfo$ndate > "2014-03-01" &twInfo$ndate < "2014-08-31", ]
dataInPer

dataInPer[order(dataInPer$Close,decreasing = TRUE),][1,]
max(dataInPer$Close)
