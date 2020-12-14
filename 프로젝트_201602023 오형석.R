# Title     : 주택과 구성원의 형태에 따른 가정 전력소비량의 영향요인 분석
# Objective : TODO
# Created by: hyngsk
# Created on: 2020-12-17
# e-mail : hyngsk.o@gmail.com
# class : Finance Analytics (prof.TaeYeon. Kwon)
library(car)
require(outliers)
library("ggplot2")

setwd("C:\\Users\\HOME\\PycharmProjects\\R_Assignments")
data <- read.csv("e.model2.csv", header = T, encoding = "UTF-8")
# 결측치 제거 2,520 x 12 -> 1,752 x 12
data <- na.omit(data)
names(data) <- c("id", "Econs", "area", "dKindOf", "dcompYear", "ExtWall", "floor", "room", "ddoUcheck", "NOF", "dEOM", "discity")
dim(data)
head(data)
summary(data)

# ------데이터 정의-------#
# 전력소비량
# 주택면적
# (더미)주택종류
# (더미)준공연도
# 거주층수
# 방수
# (더미)전월년 동월 전기요금 확인
# 가구원수
# (더미)월평균소득

# 1) 종속변수----------------------------------------
e <- data$Econs

# 2) 독립변수----------------------------------------
# 독립변수.주택형태
area <- data$area
floor <- data$floor
room <- data$room
d.KindOf <- data$dKindOf
d.compYear <- data$dcompYear
# 독립변수.가정형태
NOF <- data$NOF # number of family
d.doUcheck <- data$ddoUcheck
d.EOM <- data$dEOM

# 3) Data Frame 확인----------------------------------------
df <- data.frame(e, floor, area, room, d.compYear, NOF, d.doUcheck, d.EOM)
pairs(df)
# 로그 변환
df <- data.frame(log(e), floor, area, room, d.compYear, NOF, d.doUcheck, d.EOM)
pairs(df)


# 4) Modeling first model ----------------------------------------
m <- lm(log(e) ~ floor +
  room +
  area +
  as.factor(d.KindOf) +
  as.factor(d.compYear) +
  NOF +
  as.factor(d.doUcheck) +
  as.factor(d.EOM)
  , data = df)
m
summary(m)
# 5) check Multicollinearity----------------------------------------
vif(m)
#                          GVIF Df GVIF^(1/(2*Df))
#floor                 1.667735  1        1.291408
#room                  1.821171  1        1.349508
#area                  1.975273  1        1.405444
#as.factor(d.KindOf)   1.916495  2        1.176595
#as.factor(d.compYear) 1.426391  5        1.036153
#NOF                   1.380839  1        1.175091
#as.factor(d.doUcheck) 1.047466  3        1.007759
#as.factor(d.EOM)      1.533162  3        1.073820

# 6) Stepwise selection----------------------------------------
m <- step(m, scope = list(lower = ~1, upper = ~floor +
  room +
  area +
  as.factor(d.KindOf) +
  as.factor(d.compYear) +
  NOF +
  as.factor(d.doUcheck) +
  as.factor(d.EOM)), direction = "both")

# 7) Final model----------------------------------------
summary(m)
vif(m)
#                        GVIF Df GVIF^(1/(2*Df))
#floor               1.603613  1        1.266338
#room                1.798559  1        1.341104
#area                1.842632  1        1.357436
#as.factor(d.KindOf) 1.650481  2        1.133451
#NOF                 1.119173  1        1.057910

# 잔차의 독립성 (Independency of Standard Residuals)----------------------------------------
m.residuals <- residuals(m)
durbinWatsonTest(m.residuals)

# 잔차의 등분산성 (Homogeneity of Variance Test)----------------------------------------
plot(rstandard(m))
par(mfrow = c(1,2))
plot(m,1)
plot(m,2)

# 잔차의 정규성 (Normality of Standard Residuals)----------------------------------------
par(mfrow = c(1,1))
hist(m.residuals)
shapiro.test(m.residuals)
# 	Shapiro-Wilk normality test
#
# data:  m.residuals
# W = 0.85937, p-value < 2.2e-16
# 정규성 검정의 귀무가설을 채택 -> 정규분포와 차이가 없다.