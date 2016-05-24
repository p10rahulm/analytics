download.file(url="https://d37djvu3ytnwxt.cloudfront.net/asset-v1:MITx+15.071x_3+1T2016+type@asset+block/climate_change.csv",destfile="climate_change.csv",method= "curl")
cc= read.csv("climate_change.csv")
str(cc)
tscc = subset(cc,Year<=2006)
tecc = subset(cc,Year>2006)
Tmodel1 = lm(Temp ~ MEI+CO2+CH4+N2O+CFC.11+CFC.12+TSI+Aerosols,data=tscc)
summary(Tmodel1)
corvar = tscc[c("MEI","CO2","CH4","N2O","CFC.11","CFC.12","Aerosols")]
cor(corvar)
model2 = lm(Temp ~ MEI+TSI+Aerosols+N2O, data=tscc)
summary(model2)
model3 = step(Tmodel1) # This is for checking stepwise what is the suitable model
summary(model3)
p = predict(model3,newdata=tecc)
summary(p)
SSE = sum((p-tecc$Temp)^2)
SST = sum((mean(tscc$Temp)-tecc$Temp)^2)
1-SSE/SST
