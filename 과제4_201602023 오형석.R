# Title     : Assignment 4
# Objective : TODO
# Created by: hyngsk
# Created on: 2020-10-05
# e-mail : hyngsk.o@gmail.com
# class : Finance Analytics (prof.TaeYeon. Kwon)

# Example : Make a scatterplot of the relationship between body
# weight and pack weight for a group of hikers.

# 몸무게가 가방의 크기를 결정하니? 몸무게 : x, 가방의 크기 : y

body.w <- c(120, 187, 109, 103, 131, 165, 158, 116)
bpack.w <- c(26, 30, 26, 24, 29, 35, 31, 28)

plot(bpack.w ~ body.w, col = "red", pch = 15)
# 두 변수 간 관계의 방향, 형태, 강도를 통해 산포도의 전반적인 패턴을 설명
# 이찰현상 중 중요한 것은 이탈값으로 전반적인 패턴 밖에 위치하는 개체의 값
# 강도
cor(bpack.w, body.w)
cor(bpack.w[-2], body.w[-2])

# Y=16.2649 + 0.0908x
lm(bpack.w ~ body.w)
plot(bpack.w ~ body.w)
abline(lm(bpack.w ~ body.w), col = "red")

# 중요@@
model1 <- lm(bpack.w ~ body.w)
summary(model1)

# 여러 시각화 옵션
plot(bpack.w ~ body.w)
abline(h = 30, col = "red")
abline(v = 140, col = "blue")
points(169, 28)

# 연습문제
# 데이터 불러오기
setwd("C:\\Users\\hyngsk\\PycharmProjects\\Rproject")
# 데이터 가공
indexfinger <- read.csv("indexfinger.csv")
names(indexfinger) <- c("sex", "length", "width")
indexfinger.M <- indexfinger[indexfinger$sex == "M",]
indexfinger.F <- indexfinger[indexfinger$sex == "F",]
# 시각화
plot(width ~ length, data = indexfinger, type = "n")
points(width ~ length,
       data = indexfinger.M, pch = 24, col = "blue")
points(width ~ length,
       data = indexfinger.F, pch = 15, col = "red")
legend("topleft", legend = c("Male", "Female"),
       col = c("blue", "red"), pch = c(24, 15))
# 회귀 모델링
AllIndexFinger <- lm(indexfinger$width ~ indexfinger$length)
MaleIndexFinger <- lm(indexfinger.M$width ~ indexfinger.M$length)
FemaleIndexFinger<-lm(indexfinger.F$width ~ indexfinger.F$length)
# 회귀선 그리기
abline(MaleIndexFinger, col = "blue")
abline(FemaleIndexFinger, col = "red")
abline(AllIndexFinger , col = "green")
# 계량 모델 요약
summary(AllIndexFinger)
summary(MaleIndexFinger)
summary(FemaleIndexFinger)



