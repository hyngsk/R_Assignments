# Title     : TODO
# Objective : TODO
# Created by: hyngsk
# Created on: 2020-09-16


# basic caculations
print(1+5)
print(2.5/10)
print(3.24* 100)
print(3**3)
print(3^3)
print(5/2)
print(5%/%2)
print(5%%2)
print(3-8.2)

# Object(variable, Vector, Matrix, DataSet)
a = 10
a1 <- 10
print(a)
print(a1)


b = c(1:10)
b1 <- c(1,5,2,5)
b2 = c(b, b1)
print(b)
print(b1)
print(b2)
length(b2)
b2[12]
b2[10:14]
b2[-c(11,14,5,2)]

m = matrix(b2, nrow = 14, ncol = 3)
print(m)
m[10:11,2:3]

ID=c(1231, 1224, 1215)
Gender=c("male", "female", "male")
name= c("louis", "alex","sophia")
df = data.frame(ID, Gender, name)

print(df)

# Internal Function for calculation

mean(c(11.1, 10.6, 6.3, 8.8, 10.7, 11.2, 8.9, 12.2))
median(c(11.1, 10.6, 6.3, 8.8, 10.7, 11.2, 8.9, 12.2))
var(c(11.1, 10.6, 6.3, 8.8, 10.7, 11.2, 8.9, 12.2))
summary(c(11.1, 10.6, 6.3, 8.8, 10.7, 11.2, 8.9, 12.2))
sd(c(11.1, 10.6, 6.3, 8.8, 10.7, 11.2, 8.9, 12.2))
x = c(0:10, 50)

xm = mean(x)
c(xm, mean(x,trim = 0.10))

# Example

solar.radiation = c(11.1, 10.6, 6.3, 8.8, 10.7, 11.2, 8.9, 12.2)
solar.radiation.mean = mean(solar.radiation)
solar.radiation.median = median(solar.radiation)
solar.radiation.var = var(solar.radiation)

solar.radiation.plus10 = solar.radiation + 10
print(solar.radiation.plus10)


# 4. check var() function

k = solar.radiation.plus10
sum((k-mean(k))^2)/length(k)
sum((k-mean(k))^2)/(length(k)-1)



