
download.file(url='https://d37djvu3ytnwxt.cloudfront.net/asset-v1:MITx+15.071x_3+1T2016+type@asset+block/IBMStock.csv',destfile='IBMStock.csv', method='curl')
IBM = read.csv('IBMStock.csv')
download.file(url='https://d37djvu3ytnwxt.cloudfront.net/asset-v1:MITx+15.071x_3+1T2016+type@asset+block/GEStock.csv',destfile='GEStock.csv', method='curl')
GE = read.csv('GEStock.csv')
download.file(url='https://d37djvu3ytnwxt.cloudfront.net/asset-v1:MITx+15.071x_3+1T2016+type@asset+block/ProcterGambleStock.csv',destfile='ProcterGambleStock.csv', method='curl')
ProcterGamble = read.csv('ProcterGambleStock.csv')
download.file(url='https://d37djvu3ytnwxt.cloudfront.net/asset-v1:MITx+15.071x_3+1T2016+type@asset+block/CocaColaStock.csv',destfile='CocaColaStock.csv', method='curl')
CocaCola = read.csv('CocaColaStock.csv')
download.file(url='https://d37djvu3ytnwxt.cloudfront.net/asset-v1:MITx+15.071x_3+1T2016+type@asset+block/BoeingStock.csv',destfile='BoeingStock.csv', method='curl')
Boeing = read.csv('BoeingStock.csv')
abline(v=as.Date(c("2000-03-01")), lwd=2)

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")

GE$Date = as.Date(GE$Date, "%m/%d/%y")

CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")

ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")

Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")
plot(CocaCola$Date,CocaCola$StockPrice,type="l",col="red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice,col="blue")
abline(v=as.Date(c("2000-03-01")), lwd=2)

summary(CocaCola)

summary(ProcterGamble)
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], type="l", col="green", ylim=c(0,210))

lines(IBM$Date[301:432], IBM$StockPrice[301:432], type="l", col="purple", ylim=c(0,210))
lines(GE$Date[301:432], GE$StockPrice[301:432], type="l", col="blue", ylim=c(0,210))
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], type="l", col="black", ylim=c(0,210))
abline(v=as.Date(c("1997-09-01")), lwd=2)
abline(v=as.Date(c("1997-11-01")), lwd=2)

tapply(IBM$StockPrice,months(IBM$Date),mean)
mean(IBM$StockPrice)
tapply(GE$StockPrice,months(GE$Date),mean)

