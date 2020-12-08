# Title     : TODO
# Objective : TODO
# Created by: hyngsk
# Created on: 2020-10-05

#-- Title : [R3.3] 예측분석 - 단순 회귀분석 및 다중 회귀분석
#-- Reference : hrd-net
#-- Key word : R 회귀 분석 선형 회귀 선형회귀 회귀모델 절편 기울기 result.lm 회귀선 pred cor predict F-검정
#                  설명력 r-squared petal sepal 회귀방정식 회귀 방정식 summary lm linear model read.csv


# ************************************************
# -- 회귀분석
#    특정 변수(독립변수)가 다른 변수(종속변수)에 어떠한 영향을 미치는가 분석
# ************************************************


# ************************************************
# -- 1. 단순회귀분석
#       독립변수와 종속변수가 1개인 경우
# ************************************************

# 연구가설 : 제품 적절성은  제품 만족도에 정(正)의 영향을 미친다.
# 연구모델 : 제품적절성(독립변수) -&gt; 제품 만족도(종속변수)

# ------------------------------
# -- (1) 단순선형회귀 모델 생성
#        형식) lm(y ~ x 변수, data) # x:독립, y 종속, data=dataset
#        lm() 함수 -&gt; x변수를 대상으로 y변수 값 유추
# ------------------------------
product = read.csv("C:\\Users\\hyngsk\\PycharmProjects\\Rproject\\product.csv", header = TRUE)
head(product) # 친밀도 적절성 만족도(등간척도 - 5점 척도)
str(product) # 'data.frame':  264 obs. of  3 variables:
product
col(product)
# names(indexfinger) <- c("sex", "length", "width")
names(product)<-c('friendship', 'proper', 'satisfaction')
dim(product)
y = product$satisfaction # 종속변수
x = product$friendship # 독립변수
df = data.frame(x, y)
head(df) #   x y
# 1 3 3
# 2 3 2
# -- 회귀모델 생성
result.lm = lm(formula = y ~ x, data = df)
result.lm
# 계수 확인
# 1.926(절편)     0.399(기울기)

head(df, 1)
# 3(X) 3(Y)

# -- 회귀방정식 = 1차 함수
#    Y = 절편 + 기울기 * X
Y = 1.926 + 0.399 * 3
Y
# 3.123

# -- 잔차(오차)
3 - 3.123
# -0.123

# ------------------------------
# -- (2) 선형회귀 분석 결과 보기
# ------------------------------
summary(result.lm)

# &lt;회귀모델 분석순서&gt;
# 1. 모델의 유의성 검정 : F-검정 P값
# 2. 모델의 설명력 = 상관계수^2
# 3. x변수의 유의성 검정 : X P값

# ------------------------------
# -- (3) 단순선형회귀 시각화
# ------------------------------

# -- x,y 산점도 그리기
plot(formula = y ~ x, data = df)

# -- 회귀분석
result.lm = lm(formula = y ~ x, data = df)

# -- 회귀선
abline(result.lm, col = 'red')


# ************************************************
# -- 2. 다중회귀분석
# ************************************************
# 여러 개의 독립변수 -&gt; 종속변수에 미치는 영향 분석
# 연구가설 : 음료수 제품의 적절성(x1)과 친밀도(x2)는 제품 만족도(y)에 정의 영향을 미친다.
# 연구모델 : 제품 적절성(x1), 제품 친밀도(x2) -&gt; 제품 만족도(y)

# ------------------------------
# -- (1) 적절성 + 친밀도 -&gt; 만족도
# ------------------------------
y = product$satisfaction # 종속변수
x1 = product$friendship # 독립변수1
x2 = product$proper # 독립변수2

df = data.frame(x1, x2, y)

result.lm = lm(formula = y ~ x1 + x2, data = df)
# result.lm = lm(formula=y ~ ., data=df)
names(result.lm)
# "coefficients", "residuals" "fitted.values"

# -- 잔차 확인
residuals(result.lm)[1:2]
# -0.6959802 -1.0107567

# -- 적합값(예측값)
fitted.values(result.lm)[1:2]
# 3.695980 3.010757

# -- 계수 확인
result.lm
# 0.66731(절편)  0.09593(X1)  0.68522(X2)

# -- 회귀방정식 : Y(종속변수) = 상수 + 베타1.x1 + 베타2.x2...
#    Y = 절편 + 기울기1*X1 + 기울기2*X2
Y = 0.66731 + 0.09593 * x1 + 0.68522 * x2
head(df, 1)
# 3(x1)  4(x2) 3(y)= 관측치
Y = 0.66731 + 0.09593 * 3 + 0.68522 * 4
Y
# 3.69598(예측치)

# -- 잔차(오차)
3 - 3.69598
# -0.69598

summary(result.lm)
# &lt;회귀모델 분석순서&gt;
# 1. 모델의 유의성 검정
# 2. 모델의 설명력 = 상관계수^2
# 3. x변수의 유의성 검정

# ------------------------------
# -- (2) 학습데이터와 검증데이터 분석
# ------------------------------

# -- 단계1 : 7:3비율 데이터 샘플링
dataset = read.csv("C:\\Users\\hyngsk\\PycharmProjects\\Rproject\\product.csv", header = TRUE)

names(dataset)<-c('friendship', 'proper', 'satisfaction')

dataset
dim(dataset)
# 264   3

idx = sample(1:nrow(dataset), 0.7 * nrow(dataset))
idx
# 1 ~ 264 -&gt; 70%

# -- 단계2 : 학습데이터와 검정데이터 생성
train = dataset[idx,]
# result중 70%
dim(train)
# [1] 184   3
train
# 학습데이터

test = dataset[-idx,]
# result중 나머지 30%
dim(test)
# [1] 80  3
test
# 검정 데이터

# -- 단계3 : 회귀모델 생성 : train set 이용
result.lm = lm(formula = satisfaction ~ proper + friendship, data = train)
summary(result.lm)
# 학습데이터 분석 -&gt; p-value: &lt; 2.2e-16

# -- 단계4 : 모델 평가 : predict(model, testset)
pred = predict(result.lm, test)
# 1) 예측치 생성

cor(pred, test$satisfaction)
# 2) 모델 평가
# 0.7563656 -&gt; 76%

# ------------------------------
# -- (3) predict()함수
#        회귀분석 결과를 대상으로 회귀방정식을 적용한 새로운 값 예측(Y값)
#        형식) predict(model, test) test에 x변수(회귀분석결과) 값 존재해야함
# ------------------------------

# -- iris 데이터셋 대상으로 다음과 같이 다중회귀분석을 수행하시오.

# -- 조건1) 학습데이터(train),검증데이터(test)를 7 : 3 비율로 셈플링
idx = sample(1:nrow(iris), 0.7 * nrow(iris))
train_iris = iris[idx,]
test_iris = iris[-idx,]
dim(train_iris)
# 105   5
dim(test_iris)
# 45  5

# -- 조건2) y변수 : Sepal.Length, x변수 : Sepal.Width, Petal.Length, Petal.Width)
# -- 조건3) 1차분석 : train 데이터로 분석, 2차 분석 : test 데이터로 분석
model = lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
           data = train_iris)
model
# 계수

# -- 조건4) 회귀선이 모델에 적합한지 검정
summary(model)
# 1. F-검정 : 모델의 유의성 : 유의함
# 2. 설명력(R-squared) : 0.857
# 3. x변수 유의성 : 모든 변수 유의함(Petal.Width - 음의 영향)

# -- 조건5) 모델 평가
pred = predict(model, test_iris)
# 예측치
cor(pred, test_iris$Sepal.Length)
# 0.9330729 -&gt; 93%
