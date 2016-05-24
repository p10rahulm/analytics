download.file(url="https://d37djvu3ytnwxt.cloudfront.net/asset-v1:MITx+15.071x_3+1T2016+type@asset+block/baseball.csv",destfile="baseball.csv",method="curl")
baseball = read.csv("baseball.csv")
str(baseball)
summary(baseball)
moneyball = subset(baseball,Year<2002)
str(moneyball)
moneyball$RD = moneyball$RS - moneyball$RA
plot(moneyball$RD,moneyball$W)
WinsReg = lm(W ~ RD,data=moneyball)
summary(WinsReg)
RunsReg = lm(RS ~ OBP+SLG+BA,data=moneyball)
summary(RunsReg)
RunsReg = lm(RS ~ OBP+SLG,data=moneyball)
RunsAll = lm(RA ~ OOBP+OSLG,data=moneyball)
summary(RunsAll)
teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2013 = c(97,97,92,93,92,96,94,96,92,90)
wins2012 = c(94,88,95,88,93,94,98,97,93,94)

cor(wins2013,teamRank)
cor(wins2012,teamRank)

teamRank