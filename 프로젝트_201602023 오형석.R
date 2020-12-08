# Title     : 인천광역시 주택 특성별 전력수요량 모델
# Objective : TODO
# Created by: hyngsk
# Created on: 2020-12-08
# e-mail : hyngsk.o@gmail.com
# class : Finance Analytics (prof.TaeYeon. Kwon)
library(car)
#setwd("C:\\Users\\hyngsk\\PycharmProjects\\Rproject")
setwd("C:\\Users\\HOME\\PycharmProjects\\R_Assignments")

data <- read.csv("e.model2.csv", header = T, encoding = "UTF-8")
data <- na.omit(data)
data
# 전력소비량/주택면적
# (더미)주택종류
# (더미)준공연도
# 외벽수
# 거주층수
# 방수
# (더미)전월년 동월 전기요금 확인
# 가구원수
# (더미)월평균소득

names(data) <- c("id", "Econs", "area", "dKindOf", "dcompYear", "ExtWall", "floor", "room", "ddoUcheck", "NOF", "dEOM")
data


e <- data$Econs #/ data$area

ExtWall <- data$ExtWall
floor <- data$floor
room <- data$room
NOF <- data$NOF # number of family

d.KindOf <- data$dKindOf
d.compYear <- data$dcompYear
d.doUcheck <- data$ddoUcheck
d.EOM <- data$dEOM
# default is "apartment"
det <- ifelse(data$dKindOf == 1, 1, 0) # 단독주택 더미
row <- ifelse(data$dKindOf == 2, 1, 0) # 연립/다세대 주택 더미

# default is "2010 ~"
cYear1 <- ifelse(data$dcompYear == 1, 1, 0) # ~ 1970
cYear2 <- ifelse(data$dcompYear == 2, 1, 0) # 1971 ~ 1979
cYear3 <- ifelse(data$dcompYear == 3, 1, 0) # 1980 ~ 1989
cYear4 <- ifelse(data$dcompYear == 4, 1, 0) # 1990 ~ 1999
cYear5 <- ifelse(data$dcompYear == 5, 1, 0) # 2000 ~ 2009

# default is "always"
doUcheck1 <- ifelse(data$ddoUcheck == 2, 1, 0) # sometime
doUcheck2 <- ifelse(data$ddoUcheck == 3, 1, 0) # rarely
doUcheck3 <- ifelse(data$ddoUcheck == 4, 1, 0) # never

#default is " ? ~ 200" // Earning Of a Month
EOM1 <- ifelse(data$dEOM == 2, 1, 0) # 200 ~ 400
EOM2 <- ifelse(data$dEOM == 3, 1, 0) # 400 ~ 600
EOM3 <- ifelse(data$dEOM == 4, 1, 0) # 600 ~ ?

# df <- data.frame(total, buildingArea, numOfBeds, numOfHome)
df <- data.frame(e, ExtWall, floor, room, NOF, det, row,d.KindOf, d.compYear, d.doUcheck, d.EOM, cYear1, cYear2, cYear3, cYear4, cYear5, doUcheck1, doUcheck2, doUcheck3, EOM1, EOM2, EOM3)
df
#pairs(df)

result.lm <- lm(log(e) ~ ExtWall +
  floor +
  room +
  NOF +
  as.factor(d.compYear)+
  as.factor(d.EOM)+
  as.factor(d.KindOf)+
  as.factor(d.doUcheck)
  , data = df
)

vif(result.lm)
residuals(result.lm)[1:4]
fitted.values(result.lm)[1:4]
summary(result.lm)
#names(result.lm)

plot(result.lm, 2)
hist(result.lm$residuals)

result.lm
cor(df)
summary(result.lm)

confint(result.lm)
sm <- step(result.lm)
summary(sm)
plot(sm,5)
hist(sm$residuals)
