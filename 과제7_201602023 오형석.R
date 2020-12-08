# Title     : Assignment 7
# Objective : TODO
# Created by: hyngsk
# Created on: 2020-11-26
# e-mail : hyngsk.o@gmail.com
# class : Finance Analytics (prof.TaeYeon. Kwon)
setwd("C:\\Users\\hyngsk\\PycharmProjects\\Rproject")

#--------------------------------------------------------------------------#
# 1. capm1 ( 2-15.csv )
# data = cp1 model={m1}
cp1 <- read.csv("Table2_15.csv")
head(cp1)
dim(cp1) # 240 °³¿ù
par(mfrow = c(2, 2))
m1 <- lm(y ~ x-1, data = cp1)
m1 # y =  1.156 x
summary(m1)
plot(m1)

#--------------------------------------------------------------------------#
# 1. capm 2 ( capm : calculated risk free.csv )
# data = cp2 model={m2}
cp2 <- read.csv("capm2_rf.csv")
names(cp2)

MSFT.model <- lm( MSFT.rr~ MKT.rr -1, data = cp2)
summary(MSFT.model)

GE.model <- lm( GE.rr~ MKT.rr -1, data = cp2)
summary(MSFT.model)

GM.model <- lm( GM.rr~ MKT.rr -1, data = cp2)
summary(MSFT.model)

IBM.model <- lm( IBM.rr~ MKT.rr -1, data = cp2)
summary(MSFT.model)

DIS.model <- lm( DIS.rr~ MKT.rr -1, data = cp2)
summary(MSFT.model)

XOM.model <- lm( XOM.rr~ MKT.rr -1, data = cp2)
summary(MSFT.model)



#--------------------------------------------------------------------------#
# 1. capm 3 ( capm : calculated risk free.csv )
# data = cp3 model={m3}
cp3 <- read.csv("MSFT capm2.csv")

par(mfrow = c(1,1))
plot(cp3$MSFT , type = "l")
# plot(cp3$MSFT ~ cp3$Date) # ERROR!
cp3.ts <- ts(cp3$MSFT, start = c(2004,2), frequency = 12)
plot(cp3.ts)
abline(h = 0, col = "red")

# Calculate CAPM
cp3$MSFT.rf <- cp3$MSFT - cp3$RF

capm.model <- lm(cp3$MSFT.rf ~ cp3$MKrf-1)
summary(capm.model)

# plus dummy about subprime mortgage

which.min(cp3$MSFT)
cp3$GFC <-0
cp3$GFC[42:200] <- 1

capm2.model <- lm(cp3$MSFT.rf ~ cp3$MKrf + as.factor(GFC), data = cp3)
summary(capm2.model)

capm2.model <- lm(cp3$MSFT.rf ~ cp3$MKrf * as.factor(GFC), data = cp3)
summary(capm2.model)

capm2.model <- lm(cp3$MSFT.rf ~ cp3$MKrf, data = cp3)
summary(capm2.model)