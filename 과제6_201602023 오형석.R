# Title     : Assignment 6
# Objective : TODO
# Created by: hyngsk
# Created on: 2020-11-16
# e-mail : hyngsk.o@gmail.com
# class : Finance Analytics (prof.TaeYeon. Kwon)
setwd("C:\\Users\\hyngsk\\PycharmProjects\\Rproject")

#--------------------------------------------------------------------------#
# 1. 시간당 임금함수 예제
# data = ex1 model={m1, m2, m3}
ex1 <- read.csv("Table1_1.csv")
# ************************************
# Simple Linear Regression model
# ************************************
par(mfrow = c(1, 1))
m1 <- lm(wage ~ education, data = ex1)
m1 # wage = 1.28 * education - 4.47 + e

summary(m1)
head(ex1)
# wage, education = {11.55 , 12}
Y = -4.47 + 1.28 * 12
Y
10.89 - 11.55
# residual : 10.89 - 11.55 = -0.66
## Assumptions checking ##
# scatter plot and regression line #
plot(wage ~ education, data = ex1)
abline(m1, col = "red")
## reidual plot ##
plot(m1, 1)
plot(m1, 2)
plot(m1, 3)
# 단순선형 회귀모형으로 교육과 임금 사이에 등분산 문제에 대해 확인

# ************************************
# Multiple Linear Regression model
# ************************************
m2 <- lm(wage ~ as.factor(female) +
  as.factor(nonwhite) +
  as.factor(union) +
  education +
  exper, data = ex1)
m2
# wage =  -7.1833 -3.0749 * female -1.5653 * nonwhite + 1.0960 * union
# + 1.3703 * education + 0.1666 * exper
summary(m2)
# 교육시간외 다른조건이 동일할 때 교육시간이 1시간 증가할수록
# 평균 시간당 임금은 1.37 달러 증가한다.
plot(m2, 1)
plot(m2, 2)
plot(m2, 3)
# 다중회귀모형에서도 잔차의 분산이 증가는 문제가 보이므로 로그 변환이 필요해 보인다.

# log transformation of wage
ex1$logwage <- log(ex1$wage)
m3 <- lm(logwage ~ as.factor(female) +
  as.factor(nonwhite) +
  as.factor(union) +
  education +
  exper, data = ex1)
m3


# wage = 0.90550 - 0.24915 * female - 0.13354 * nonwhite +
#  0.18020 * union +
#  0.09987 * education +
#  0.01276 * exper
summary(m3)
# 교육시간이 1시간 증가함에 따라 평균 시간당 임금은 9.99% 만큼 증가 한다
# 잔차 그래프
plot(m3, 1)

# compare
par(mfrow = c(1, 2))
plot(m2, 1)
plot(m3, 1)
plot(m2, 2)
plot(m3, 2)
hist(m2$residuals)
hist(m3$residuals)

#--------------------------------------------------------------------------#
# 2. Cobb-Douglas 함수
# Output - Labor + capital
# ln output ~ log L + log Cap
#  -> 잔차 plot 비교
#  -> 해석 (회귀계수 해석)
# data = ex2 model={m4, m5}
ex2 <- read.csv("Table2_1.csv")
ex2

m4 <- lm(output ~ labor + capital, data = ex2)
m4
# output = 233600 + 47.99 * labor + 9.952 * capital
summary(m4)
# intercept 가 유의하지 않음
# erase intercept
m4 <- lm(output ~ labor + capital - 1, data = ex2)
m4
# output = 48.326 * labor + 9.948 * capital
summary(m4)
#잔차 분석
par(mfrow=c(2,2))
plot(m4,1)
plot(m4, 2)
hist(m4$residuals)
# 노동력과 자본이 적다면 생산량이 다양하지 않을 것이고
# 노동력또는 자본이 많다면 생산량에 대해서도 다양할 것이기 때문에
# 보다 정확한 모델링을 위해 로그변환이 이루어져야 함.

# 노동력과 자본의 미세한 변화에 대한 생산량 변화를 알아보기위해 로그변환
# 순간탄력성 측정

m5 <- lm(lnoutput ~ lnlabor + lncapital, data = ex2)
m5
# ln output = 3.8876 + 0.4683 * ln labor + 0.5213 * ln capital
summary(m5)
#잔차 분석 & 비교
par(mfrow=c(2,3))
plot(m4,1)
plot(m4, 2)
hist(m4$residuals)
plot(m5,1)
plot(m5, 2)
hist(m5$residuals)

# 자본의 변회를 제외하고 노동력만 1% 증가했을때 0.47 % 증가함을 알 수 있다.
# 노동력의 변회를 제외하고 자본만 1% 증가했을때 0.52 % 증가함을 알 수 있다.

#--------------------------------------------------------------------------#
# 3. GDP성장함수 Table 2_5
# GDP ~ time
# ln GDP ~ time
#  -> 잔차 plot 비교
#  -> 해석 (회귀계수 해석)
# data = ex3 model={m6, m7}
ex3 <- read.csv("Table2_5.csv")
head(ex3)

m6 <- lm(rgdp ~ time, data = ex3)
m6 # rgdp = 1664 + 187 * time

summary(m6)
# 단순회귀모형이기 때문에 시간에 따른 GDP변화를 살펴보자
par(mfrow=c(1,1))
plot(ex3$rgdp ~ ex3$time)
# 시간이 지남에 따라 GDP는 증가한다.

# 등분산성 검정을 위해 잔차를 분석해보자.
plot(m6$residuals ~ ex3$time)
# 시간에 따른 잔차가 이차함수 형태를 띈다.
par(mfrow=c(2,3))
plot(m6,1)
plot(m6, 2)
hist(m6$residuals)
# 정규분포 또한 보이지 않는다.
# 잔차의 등분산성과 정규성을 만족하고자 y의 로그변환을 취함
# + 분석의 목표가 GDP가아닌 GDP의 '성장률'이기 때문에 반 탄력적 모형을 사용

# log transformation
m7 <- lm(lnrgdp ~ time, data = ex3)
# 잔차 분석 비교
par(mfrow=c(2,3))
plot(m6,1)
plot(m6, 2)
hist(m6$residuals)
plot(m7,1)
plot(m7, 2)
hist(m7$residuals)
# 로그변환으로 등분산성과 정규성이 만족된것을 볼 수 있다.

m7 # ln rgdp = 7.87566 + 0.03149 * time
summary(m7)
# 데이터에 따르면 시간이 1년 증가함에 따라 GDP는 0.03% 증가한다고 볼 수 있다.

#--------------------------------------------------------------------------#
# 4. engel~food expenditure model
# 음식 지출비용 ~ 전체 지출 액
# 음식 지출비용 ~ log 전체 지출 액
# data = ex4, model={m8, m9}
ex4 <- read.csv("Table2_8.csv")
ex4
head(ex4)
# sfdho = fdho / expend
# 음식지출비 비율 (%)= 음식지출비용($) / 총 지출 ($) * 100
# 하나의 지수의 역할
# 지출 대비 평균 음식지출비율이 어느정도 변화 되는지 모델링하고싶다.
m8 <- lm(sfdho ~ expend, data = ex4)
m8 # sfdho = 0.2109 -0.000002211 expend
# 유의하다.
par(mfrow=c(2,3))
plot(m8,1)
plot(m8, 2)
hist(m8$residuals)
# 잔차의 정규성과 등분산성이 만족된다.
# 하지만 지출의 단위가 굉장히 커
# 등분산성을 높여 보다 정확한 모델링을 하고자
# 단위를 줄일 필요성이 있어보인다.
summary(m8)
plot(sfdho ~ expend, data = ex4)
abline(m8, col = "red")
# 지출을 로그변환한다.
m9 <- lm(sfdho ~ lnexpend, data = ex4)
# 잔차 분석
par(mfrow=c(2,3))
plot(m8,1)
plot(m8, 2)
hist(m8$residuals)
plot(m9,1)
plot(m9, 2)
hist(m9$residuals)

# 지출에 로그를 취하니 지출의 단위가 줄은것을 볼 수 있다.
m9 # sfdho = 0.93039 - 0.07774 * ln expend
summary(m9)
# 지출이 100% 증가함에 따라 음식지출비율이 평균적으로 0.078%P 만큼 감소하는것을 알 수 있다.
# 지출이 1% 증가에 따라 음식지출비율이 평균적으로 0.00078%P 만큼 감소하는것을 알 수 있다.


#이번 과제를 통해 모델을 검증하는데에 있어
#잔차의 정규성과 등분산성검증, 또
#데이터의 단위에 따른 로그변환을 해 보면서
#로그변환이 필요할 때에 대해 생각해 보게 되었습니다.


