# Title     : Assignment 8
# Objective : TODO
# Created by: hyngsk
# Created on: 2020-12-03
# e-mail : hyngsk.o@gmail.com
# class : Finance Analytics (prof.TaeYeon. Kwon)
setwd("C:\\Users\\hyngsk\\PycharmProjects\\Rproject")

#--------------------------------------------------------------------------#
# 1. Fama-French ( MSFT capm2.csv )
# data = cp1 model={m1}
cp1 <- read.csv("MSFT capm2.csv")
head(cp1)

cp1$MSFT.rf <- cp1$MSFT - cp1$RF

ff.model <- lm(MSFT.rf ~ MKrf + SMB + HML, data = cp1)
summary(ff.model)

ff.model <- lm(MSFT.rf ~ MKrf + SMB + HML - 1, data = cp1)
summary(ff.model)

#--------------------------------------------------------------------------#
# 2. 잔차의 timeplot( Table6_1.csv )
# data = data2 model={m2}

data2 <- read.csv("Table6_1.csv")
summary(data2)

m2 <- lm(lnconsump ~ lndpi + lnwealth + interest, data = data2)
summary(m2)


data2$time <- seq(1947:2000) #time 변수 만들기#
par(mfrow = c(2, 2))
plot(m2$residuals ~ m2$fitted, main = "residual-fitted")
plot(m2$residuals ~ data2$time, main = "time plot of residual")
plot(m2$residuals ~ data2$time, type = "l", main = "time plot of residual type=l")
plot(m2$residuals ~ data2$time, type = "b", main = "time plot of residual type=b")

par(mfrow = c(1, 1))
plot(m2$residuals ~ data2$time, type = "l", main = "time plot of residual type=l")
abline(h = 0, col = "red")


#--------------------------------------------------------------------------#
# 3. ACF ( Table6_1.csv )
# data = data2, model={m2}
n <- dim(data2[2])
cor(m2$residuals[2:n], m2$residuals[1:n - 1])

acf(m2$residuals)
acf.value <- acf(m2$residuals)
acf.value
#--------------------------------------------------------------------------#
# 3. Durbin-Watson d-statistics ( Table6_1.csv )
# data = data2, model={m3}

install.packages("lmtest")
library(lmtest)
m3 <- lm(lnconsump ~ lndpi + lnwealth + interest, data = data2)
dwtest(m3)

#--------------------------------------------------------------------------#
# 4. Breusch-Godfrey Test ( Table6_1.csv )
# data = data2, model={m4}
m4 <- lm(lnconsump ~ lndpi + lnwealth + interest, data = data2)
bgtest(m4, order = 1)
bgtest(m4, order = 5)
bgtest(m4, order = 1, type = "F")


#--------------------------------------------------------------------------#
# 5. 차분 ( Table6_1.csv )
# data = data2, model={m5}

ynew <-data2$lnconsump[2:dim(data2)[1]] - data2$lnconsump[1:dim(data2)[1]-1]
ynew <- diff(data2$lnconsump, differences = 1)
# data2$ynew[2:54] <- ynew

lndpinew<- diff(data2$lndpi, differences = 1)

lnwealthnew <- diff(data2$lnwealth, differences = 1)

interestnew <- diff(data2$interest, differences = 1)

m5 <- lm(ynew ~ lndpinew + lnwealthnew + interestnew)
summary(m5)
plot(m5$residuals~ data2$year[2:54], type = "l")
acf(m5$residuals)
dwtest(m5)

#--------------------------------------------------------------------------#
# 6. FGLS ( Table6_1.csv )
# data = data2, model={m6}
library(nlme)
model.gls<-gls(lnconsump~lndpi+lnwealth+interest, data=data2, correlation=corARMA(p=1))
summary(model.gls)


#--------------------------------------------------------------------------#
# 6. 경제상황(macro economic variables)과 신규주택건설
# ( Table6_10.csv )
# data = data10, model={model, model_diff}
data10 <- read.csv(file = "Table6_10.csv")
summary(data10)

model <- lm(hstart ~ un + m2 + mgrate + primerate +rgdp, data = data10)

summary(model)

# 가정체크
dim(data10)
plot(model$residuals ~ model$fitted)
abline(h=0)
plot(model, 1)
plot(model, 2)

# log 변환
data10$lnhstart <- log(data10$hstart)
data10$lnun <- log(data10$un)
data10$lnm2 <- log(data10$m2)
data10$lnmgrate <- log(data10$mgrate)
data10$lnprimerate <- log(data10$primerate)
data10$lnrgdp <- log(data10$rgdp)
model2 <- lm(lnhstart ~ lnun + lnm2 + lnmgrate + lnprimerate +lnrgdp, data = data10)
summary(model2)
plot(model2, 1)
plot(model2, 2)

# 유의미하지않은 변수 제거
model <- lm(hstart ~ un  + mgrate + primerate , data = data10)
summary(model)

# 가정 5 시계열 오차항 자기상관
plot(model$residuals, type = "b")
abline(h=0, col = "red")

acf(model$residuals)
dwtest(model)
# 자기상관 문제 유

# 1차 차분
hstar_diff1 <- diff(data10$hstart, differences = 1)
un_diff1 <- diff(data10$un, differences = 1)
mgrate_diff1 <- diff(data10$mgrate, differences = 1)
primerate_diff1<- diff(data10$primerate, differences = 1)

model_diff <- lm(hstar_diff1~un_diff1 + mgrate_diff1 + primerate_diff1)
summary(model_diff)

model_diff <- lm(hstar_diff1~un_diff1  + primerate_diff1-1)
summary(model_diff)

plot(model_diff, 1)
 plot(model_diff, 2)

# 자기상관 다시 확인
plot(model_diff$residuals, type = "b")
abline(h=0, col = "red")
acf(model_diff$residuals)
dwtest(model_diff)

# 최종모형
model_diff <- lm(hstar_diff1~un_diff1  + primerate_diff1-1)
summary(model_diff)