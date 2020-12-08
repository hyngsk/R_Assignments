# Title     : Assignment 5
# Objective : TODO
# Created by: hyngsk
# Created on: 2020-10-14
# e-mail : hyngsk.o@gmail.com
# class : Finance Analytics (prof.TaeYeon. Kwon)

# Example : ROE - IT 

# step 1
setwd("C:\\Users\\hyngsk\\PycharmProjects\\Rproject")
iroed <- read.csv("ITroe.csv")

# step 2 : checking data
head(iroed)

# step 3 : Graph
plot(ROE~IT, data = iroed)

# Estimation
# y(ROE) = -1.53 + 0.04x(IT) + ei
b1 <- cor(iroed$ROE, iroed$IT)*sd(iroed$ROE)/sd(iroed$IT)
b0<- mean(iroed$ROE)- b1*mean(iroed$IT)

# step 4 : Model fitting
model.iroed <- lm(ROE~IT, data = iroed)
summary(model.iroed)

# step 5 : Interpretation

# 사원 1인당 IT투자 금액이 1만원 더 많은 기업은,
# 평균적으로 ROE가 0.04 더 큽니다.
# Companies with more IT investments of 10,000 won per employee
# have an average ROE of 0.04 higher.

# Example : Wage data

wdata <- read.csv("Table1_1.csv")

head(wdata)

plot(wage~education, data = wdata)
plot(wage~age, data = wdata)

model.wage <- lm(wage~education + exper + age, data = wdata)
summary(model.wage)
# y = -9.59 + 1.41edu + 0.17exp

model.wage<- lm(wage~ education+ age, data = wdata)
summary(model.wage)

# y = -10.6 + 1.24edu + 0.18age + ei

# When people are of the same age, 
# the wages of those who have been educated for one more year
# increase by 1.24 on average.

# When a person's educational years are the same,
# the wages for a one-year-old person 
# increase by 0.18 on average.
