download.file("https://d37djvu3ytnwxt.cloudfront.net/asset-v1:MITx+15.071x_3+1T2016+type@asset+block/pisa2009train.csv","pisa2009train.csv","curl")
pt = read.csv("pisa2009train.csv")
download.file("https://d37djvu3ytnwxt.cloudfront.net/asset-v1:MITx+15.071x_3+1T2016+type@asset+block/pisa2009test.csv","pisa2009test.csv","curl")
ptest = read.csv("pisa2009test.csv")
summary(pt)
table(pt$grade,pt$male)
tapply(pt$grade,pt$male==1,mean)
tapply(pt$readingScore,pt$male==1,mean)
summary(pt)
pt = na.omit(pt)
ptest  = na.omit(ptest)
summary(pt)
str(pt)
pt$raceeth = relevel(pt$raceeth, "White")
ptest$raceeth = relevel(ptest$raceeth, "White")
lmScore = lm(readingScore ~ .,data=pt)
summary(lmScore)
SSE = sum((lmScore$residuals)^2)
RMSE = sqrt(SSE/nrow(pt))
predTest = predict(lmScore,newdata = ptest)
summary(predTest)
SSE1 = sum((predTest - ptest$readingScore)^2)
RMSE1 = sqrt(SSE1/nrow(ptest))
SST1 = sum((mean(pt$readingScore) - ptest$readingScore)^2)
R12= 1-SSE1/SST1

