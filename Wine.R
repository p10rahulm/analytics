download.file(url="https://d37djvu3ytnwxt.cloudfront.net/asset-v1:MITx+15.071x_3+1T2016+type@asset+block/wine.csv", destfile = "wine.csv", method = 'curl')
wine = read.csv("wine.csv")
download.file(url="https://d37djvu3ytnwxt.cloudfront.net/asset-v1:MITx+15.071x_3+1T2016+type@asset+block/wine_test.csv", destfile = "wine_test.csv", method = 'curl')
wine_test = read.csv("wine_test.csv")
str(wine)
model1=lm(Price ~ AGST,data=wine)
summary(model1)
model1$residuals
SSE = sum((model1$residuals)^2)
SSE
model2=lm(Price ~ AGST+HarvestRain,data=wine)
SSE = sum((model2$residuals)^2)
SSE
model3=lm(Price ~ AGST+HarvestRain+WinterRain+Age+FrancePop,data=wine)
SSE = sum((model3$residuals)^2)
SSE
summary(model3)
model4=lm(Price ~ AGST+HarvestRain+WinterRain+Age,data=wine)
SSE = sum((model4$residuals)^2)
SSE
summary(model4)
model5=lm(Price ~ HarvestRain+WinterRain,data=wine)
summary(model5)
model6=lm(Price ~ AGST+HarvestRain+WinterRain,data=wine)
summary(model4)
cor(wine$WinterRain,wine$Price)
cor(wine$Age,wine$FrancePop)
cor(wine)
 wineTest=read.csv("wine_test.csv")
str(wineTest)
summary(wineTest)
predictTest=predict(model4,wine_test)
predictTest
SST= sum((wineTest$Price-mean(wine$Price))^2)
SSE=sum((wineTest$Price-predictTest)^2)
1-SSE/SST