download.file("https://d37djvu3ytnwxt.cloudfront.net/asset-v1:MITx+15.071x_3+1T2016+type@asset+block/FluTrain.csv","flutrain.csv","curl")
flt =read.csv("flutrain.csv")
summary(flt)
which(flt$ILI>7.5)
which.max(flt$ILI)
which.max(flt$Queries)
flt[which.max(flt$Queries),]
flt[which.max(flt$Queries),c('Week','ILI')]
hist(flt$ILI)
plot(log(flt$ILI),flt$Queries)
plot(flt$Queries,log(flt$ILI))
ft1 = lm(log(ILI)~Queries,data=flt)
summary(ft1)
download.file("https://d37djvu3ytnwxt.cloudfront.net/asset-v1:MITx+15.071x_3+1T2016+type@asset+block/FluTest.csv","ft.csv","curl")
ftest = read.csv("ft.csv")
PredTest1 = exp(predict(ft1, newdata=ftest))
ftest[which(ftest$Week=="2012-03-11 - 2012-03-17"),]
ftest$Predictions = PredTest1
SSEpred = sum((PredTest1 - ftest$ILI)^2)
RMSEpred = sqrt(SSEpred/nrow(ftest))

#Zoo Packages

install.packages("zoo")
library(zoo)
ILILag2 = lag(zoo(flt$ILI), -2, na.pad=TRUE)
flt$ILILag2 = coredata(ILILag2)

summary(flt)
plot(flt$ILILag2,flt$ILI)
plot(log(flt$ILILag2),log(flt$ILI))
ft2 = lm(log(ILI) ~ Queries+log(ILILag2), data = flt )
summary(ft2)

ILILag2 = lag(zoo(ftest$ILI), -2, na.pad=TRUE)
ftest$ILILag2 = coredata(ILILag2)
summary(ftest)
#Fill in first two rows
ftest$ILILag2[1] = flt$ILI[nrow(flt)-1]
ftest$ILILag2[2] = flt$ILI[nrow(flt)]
ftpred=exp(predict(ft2,newdata = ftest))
ftest$pred = ftpred
ftest
SSEpred2 = sum((ftpred - ftest$ILI)^2)
RMSEpred2 = sqrt((SSEpred2)/nrow(ftest))
