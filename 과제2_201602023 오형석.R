# Title     : Assignment 2
# Objective : TODO
# Created by: hyngsk
# Created on: 2020-09-16
# e-mail : hyngsk.o@gmail.com
# class : Finance Analytics (prof.TaeYeon. Kwon)

a <- c(3, 6, 9)
!a > 4


a[a > 4]
a[a != 3]

a > 4 & a < 9

a[a > 4 | a == 3]


# data input output
setwd("C:\\Users\\hyngsk\\PycharmProjects\\Rproject")
data1 <- read.csv("test.csv")
data1
data1[1,]
data1[10,]

names(data1)
col(data1)
head(data1)
dim(data1)

data2 <- data1$exam2
data2

mean(data1$exam)
mean(data1[, 2])
var(data1$exam)

hist(data1$exam, col = "cyan")

plot(data1$exam2 ~ data1$exam)
plot(data1$exam, data1$exam2,
     main = "scatter plot",
     xlab = "mid",
     ylab = "final",
     col = "red",
     pch = 11,
     lty = 2,
     type = "p")

setwd("C:\\Users\\hyngsk\\PycharmProjects\\Rproject")

indexfinger <- read.csv("indexfinger.csv")
indexfinger
names(indexfinger) <- c("sex", "length", "width")
indexfinger
plot(indexfinger$width ~ indexfinger$length)

indexfinger.M <- indexfinger[indexfinger$sex == "M",]
indexfinger.M
indexfinger.F <- indexfinger[indexfinger$sex == "F",]
indexfinger.F

par(mfrow = c(2, 1))
plot(indexfinger.M$width ~ indexfinger.M$length,
     col = "blue",
     pch = 15,
     main = "Male finger",
     xlab = "Male len",
     ylab = "Male wid")
plot(indexfinger.F$width ~ indexfinger.F$length,
     col = "red",
     pch = 15,
     main = "Female finger",
     xlab = "Female len",
     ylab = "Female wid")


par(mfrow = c(2, 1))
hist(indexfinger.M$width,
     main = "Male Finger",
     col = "blue")
hist(indexfinger.F$width,
     main = "Male Finger",
     col = "red")


par(mfrow = c(1, 1))
plot(width ~ length, data = indexfinger, type = "n")

points(width ~ length,
       data = indexfinger.M, pch = 24, col = "blue")
points(width ~ length,
       data = indexfinger.F, pch = 15, col = "red")

legend("topleft", legend = c("Male", "Female"),
       col = c("blue", "red"), pch = c(24, 15))

