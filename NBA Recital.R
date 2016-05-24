download.file(url="https://d37djvu3ytnwxt.cloudfront.net/asset-v1:MITx+15.071x_3+1T2016+type@asset+block/NBA_train.csv",destfile="NBA_train.csv",method = "curl")
NBA = read.csv("NBA_train.csv")
download.file(url="https://d37djvu3ytnwxt.cloudfront.net/asset-v1:MITx+15.071x_3+1T2016+type@asset+block/NBA_test.csv",destfile="NBA_test.csv",method = "curl")

str(NBA)
table(NBA$W,NBA$Playoffs)
plot(NBA$W,NBA$Playoffs)
NBA$PTSdiff = NBA$PTS - NBA$oppPTS
plot(NBA$PTSdiff,NBA$W)
WinsReg = lm(W ~ PTSdiff,data = NBA)
summary(WinsReg)
PointsReg = lm(PTS ~ X2PA+X3PA+FTA+AST+ORB+DRB+TOV+STL+BLK, data=NBA)
summary(PointsReg)
SSE= sum((PointsReg$residuals)^2)
SSE
RMSE = sqrt(SSE/nrow(NBA))
RMSE
mean(NBA$PTS)
PointsReg2 = lm(PTS ~ X2PA+X3PA+FTA+AST+ORB+DRB+STL+BLK, data=NBA)
summary(PointsReg2)
PointsReg3 = lm(PTS ~ X2PA+X3PA+FTA+AST+ORB+STL+BLK, data=NBA)
summary(PointsReg3)
PointsReg4 = lm(PTS ~ X2PA+X3PA+FTA+AST+ORB+STL, data=NBA)
summary(PointsReg4)
 
SSE4 = sum((PointsReg4$residuals)^2)
RMSA4 = sqrt(SSE4/nrow(NBA))
RMSA4

regvar = NBA[c("X2PA","X3PA","FTA","AST", "ORB","STL")]
cor(regvar)
NBA_test = read.csv("NBA_test.csv")
PointsPrediction = predict(PointsReg4, newdata=NBA_test)
summary(PointsPrediction)
SSE
SSE = sum((PointsPrediction-NBA_test$PTS)^2)
SST = sum((mean(NBA$PTS)-NBA_test$PTS)^2)
R2 = 1-SSE/SST
R2
RMSE = sqrt(SSE/nrow(NBA_test))
RMSE
