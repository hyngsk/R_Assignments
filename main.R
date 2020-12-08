# Title     : TODO
# Objective : TODO
# Created by: hyngsk
# Created on: 2020-09-15
dataset = read.csv("C:\\Users\\hyngsk\\PycharmProjects\\Rproject\\ICTdata.csv", header = TRUE)

#
#InovIndex : 과학기술혁신역량지수
#
#UnivEngineer : 대학연구개발인력(명)
#UnivResearcher : 대학연구원(명) *
#UnivResCost : 대학 연구개발비(억원)*
#ResInvest : 대학 연구시설장비 투자액(억원)
#JointUse : 장비공동활용현황(억원)*
#ExclusUse : 장비단독활용현황 (억원)

head(dataset)
str(dataset)
dataset
y <- dataset$InovIndex
x1 <- dataset$UnivEngineer
x2 <- dataset$UnivResearcher
x3 <- dataset$UnivResCost
x4 <- dataset$ResInvest
x5 <- dataset$JointUse
x6 <- dataset$ExclusUse


lm(x6 ~ y)
plot(x6 ~ y, col = "red", pch = 15)
abline(lm(x6 ~ y))


df <- data.frame(x1, x2, x3, x4, x5, x6, y)
result.lm <- lm(formula = y ~ x1 + x2 + x3 + x4 + x5 + x6, data = df)
names(result.lm)
residuals(result.lm)[1:3]
fitted.values(result.lm)[1:3]
result.lm
summary(result.lm)


# 0R0JKVITP2JWI6AY3JQ0
현재가계저축CSI[FMDA]


# 데이터 가공

#---------------------------------------------------------------------------------
dset <- read.csv("Manpower_Of_ICT.csv")
head(dset)
names(dset) <- c("year", "RateOfHire", "Export", "Take", "numOfCom", "people", "idx")
dset
head(dset)
str(dset)

(dset$Export * 1000) / (dset$Take * 1000000 * 0.00088) * 100
y <- (dset$Export * 1000) / (dset$Take * 1000000 * 0.00088) * 100
x1 <- dset$Take / dset$people
x2 <- dset$numOfCom
x3 <- dset$idx
x4 <- dset$numOfCom
x1
x2
x3
x4
z <- x2 / x3
z
summary(lm(y ~ x1))
plot(y ~ x1, col = "red", pch = 15)
abline(lm(y ~ x1))

df <- data.frame(y, x2, x3)
result.lm <- lm(y ~ x2 + x3, data = df)
names(result.lm)
residuals(result.lm)[1:3]
fitted.values(result.lm)[1:3]
result.lm
pairs(df)
vif(result.lm)
summary(result.lm)


#---------------------------------------------------------------------------------
library(car)
setwd("C:\\Users\\hyngsk\\PycharmProjects\\Rproject\\중소기업")

a <- read.csv("HireInfo.csv")
b <- read.csv("DomesticSale.csv")
c <- read.csv("ExportPerform.csv")
d <- read.csv("ProductionPerform.csv")
e <- read.csv("FacilityLev.csv")

names(a) <- c("time", "HireInfo")
names(b) <- c("time", "DomesticSale")
names(c) <- c("time", "ExportPerform")
names(d) <- c("time", "ProductionPerform")
names(e) <- c("time", "FacilityLev")

hireInfo <- a$HireInfo
domesticSale <- b$DomesticSale # 65.07
exportPerform <- c$ExportPerform # 72.37
productionPerform <- d$ProductionPerform # 66.15
facilityLev <- e$FacilityLev # 88.63

df <- data.frame(hireInfo, domesticSale, exportPerform, productionPerform, facilityLev)
pairs(df)
vif(result.lm)

result.lm <- lm(hireInfo ~
                  domesticSale + exportPerform + facilityLev, data = df)
residuals(result.lm)[1:4]
fitted.values(result.lm)[1:4]

summary(result.lm)
#names(result.lm)

result.lm

summary(result.lm)

#---------------------------------------------------------------------------------
#서울 행복도
library(car)
setwd("C:\\Users\\hyngsk\\PycharmProjects\\Rproject")

data <- read.csv("SeoulHappiness.csv", header = T)
names(data) <- c("year", "region", "happy", "debt", "sLive")
names(data) <- c(
  "y", "r", "happiness", "debt", "sLive", "sEconomic", "sSocial", "sEdu", "death", "greanAvg", "a",
  "b", "c", "d", "popM",
  "Hospital", "sitOfHos", "GRDP", "1House",
  "apart", "manyfloor", "manyfam", "nobody")
Happiness <- data$happiness

debt <- data$debt
sLive <- data$sLive
sEconomic <- data$sEconomic
sEdu <- data$sEdu
sSocial <- data$sSocial
death <- data$death
grdp <- data$GRDP
popM <- data$popM
hos <- data$Hospital
apart <- data$apart

df <- data.frame(Happiness, debt, sLive, sEconomic, sEdu, sSocial, death, grdp, apart)

pairs(df)

result.lm <- lm(Happiness ~ sLive +
  sEconomic +
  sSocial +
  death +
  apart +
  grdp, data = df)
vif(result.lm)
residuals(result.lm)[1:4]
fitted.values(result.lm)[1:4]

summary(result.lm)
#names(result.lm)

result.lm

summary(result.lm)

#---------------------------------------------------------------------------------

library(car)
setwd("C:\\Users\\HOME\\PycharmProjects\\R_Assignments")

data <- read.csv("e.model.csv", header = T, encoding = "UTF-8")
data
names(data) <- c("region", "e", "pop", "house","popM", "DistGFA", "OfficeGFA", "BuildingGFA", "area", "TotalHos",
                 "TotalNOB", "No.GHos", "NOB.GHos", "No.Hos", "NOB.Hos", "No.Clinic", "NOB.Clinic",
                 "No.Apart", "No.Detch", "No.M.Fam", "No.Row", "water"
)
data

e <- data$e/data$area

pop <- data$pop
house <- data$house
popM <- data$popM
DistGFA <- data$DistGFA/data$BuildingGFA
OfficeGFA <- data$OfficeGFA/data$BuildingGFA
BuildingGFA <- data$BuildingGFA
area <- data$area

TotalHos <- data$TotalHos
TotalNOB <- data$TotalNOB

No.GHos <- data$No.GHos
NOB.GHos <- data$NOB.GHos
No.Hos <- data$No.Hos
NOB.Hos <- data$NOB.Hos
No.Clinic <- data$No.Clinic
NOB.Clinic <- data$NOB.Clinic

No.Apart <- data$No.Apart
No.Detch <- data$No.Detch
No.M.Fam <- data$No.M.Fam
No.Row <- data$No.Row
water <- data$water

summary(data)

# df <- data.frame(total, buildingArea, numOfBeds, numOfHome)
df <- data.frame(e, pop,house, popM, DistGFA, OfficeGFA, BuildingGFA, area, TotalHos,
                 TotalNOB, No.GHos, NOB.GHos, No.Hos, NOB.Hos, No.Clinic, NOB.Clinic,
                 No.Apart, No.Detch, No.M.Fam, No.Row, water)
df
pairs(df)

result.lm <- lm(e ~ #pop +
  house +
    #popM +
  DistGFA +
  OfficeGFA
  #BuildingGFA +
  #area +
#  TotalHos +
#  TotalNOB
  #NO.GHos +
  #NOB.GHos +
  #NO.Hos +
  #NOB.Hos +
  #NO.Clinic +
  #NOB.Clinic +
#  No.Apart
#  +No.Detch
#  +No.M.Fam +
#  No.Row
#  water
  , data = df
)
vif(result.lm)
residuals(result.lm)[1:4]
fitted.values(result.lm)[1:4]
summary(result.lm)
#names(result.lm)
plot(result.lm, 1)

result.lm
cor(df)
summary(result.lm)
