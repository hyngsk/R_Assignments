# Title     : Assignment 3
# Objective : TODO
# Created by: hyngsk
# Created on: 2020-09-16
# e-mail : hyngsk.o@gmail.com
# class : Finance Analytics (prof.TaeYeon. Kwon)

# install package
# install.packages("xlsx")

x <- 3
if (x > 2) y <- 2 * x else y <- 3 * x
y <- ifelse(x > 2, 2 * x, 3 * x)
x <- -3
if (x <- 1) {
  y <- -1
}else if (x < 0) {
  y <- 0
} else {
  y <- 1
}
print(y)

x <- rep(1, 100)
x
for (i in 1:100) {
  x[i] <- i
}
x


x <- rep(1, 100)
x
for (i in 2:100) {
  x[i] <- x[i - 1] + i
}
x


a <- rep(1, 10)
a[1] <- 1
a[2] <- 1
for (i in 3:10) {
  a[i] <- a[i - 1] + a[i - 2]
}
a


weight <- c(60, 72, 57, 90, 95, 72)
height <- c(1.75, 1.80, 1.65, 1.90, 1.74, 1.91)

BMI <- weight / height^2

BMI

BMI_ID <- rep(0, length(weight))

for (i in seq_along(weight)) {
  if (BMI[i] <= 18.5) {
    BMI_ID[i] <- 1
  }else if (BMI[i] <= 24.9) {
    BMI_ID[i] <- 2
  }else if (BMI[i] <= 29.9) {
    BMI_ID[i] <- 3
  }else if (BMI[i] <= 30) {
    BMI_ID[i] <- 4
  }else {
    BMI_ID[i] <- 5
  }
}
BMI_ID

data2 <- BMI[BMI_ID == 3 | BMI_ID == 4]
data1 <- BMI[BMI_ID == 1 | BMI_ID == 5]
data1
data2

data <- data.frame(weight, height, BMI, BMI_ID)
data

data2 <- data[BMI_ID == 3 | BMI_ID == 4,]
data1 <- data[BMI_ID == 1 | BMI_ID == 5,]
data1
data2

## while ##
x <- 1
while (x < 5) {
  x <- x + 1
  if (x == 3)
    # break
    next
  print(x)
}

# function
func1 <- function(x = 10) {
  return(x^2 + 10)
}

func1()

TY2 <- function(x1, x2) {
  y1 = x1^2 + 10
  y2 = x2^3 + 10
  list(y1 = y1, y2 = y2)
}

AAA <- TY2(10, 10)
AAA
AAA$y1
AAA$y2
# 사용자 정의 함수 실습
cal_balance <- function(year, money, rate) {
  bal <- 0
  for (n in 1:year) {
    bal <- bal + money * (1 + rate / 100)^(n - 1)
  }
  return(bal)
}
cal_balance(10, 400, 5)


cal_bal2 <- function(n, R, r) {
  bal <- rep(0, n - 1)
  for (i in 1:n) {
    bal[i] <- R * (1 + r)^(i - 1)
  }
  return(sum(bal))
}
cal_bal2(10, 400, 0.05)


cal_bal3 <- function(n, R, r) {
  bal = (R * ((1 + r)^n - 1))/r
  return(bal)
}
cal_bal3(10,400,0.05)






