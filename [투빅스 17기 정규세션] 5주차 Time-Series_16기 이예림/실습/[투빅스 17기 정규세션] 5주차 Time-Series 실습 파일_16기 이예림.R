## 데이터 분석에 필요한 패키지 설치하기 ##

install.packages("forecast")
library(forecast)

## 데이터 불러오기 & 시도표 확인하기 ##

data <- read.csv("C:/Users/82108/Downloads/2022_프로젝트/투빅스 정규세션 과제/[투빅스 17기 정규세션] 5주차 Time-Series_16기 이예림/실습/finaltest.csv")
Y <- ts(data)
plot.ts(Y)

diff1_Y <- diff(Y, differences = 1)
plot.ts(diff1_Y)

diff12_Y <- diff(diff1_Y, lag = 12)
plot.ts(diff12_Y)

## 모형 식별 ##

par(mfrow=c(2,1))
acf(diff12_Y, main = "ACF")
pacf(diff12_Y, main = "PACF")


## 모수 추정 ##

fit1 <- arima(Y, c(1, 1, 1), seasonal = list(order = c(1, 1, 0), period = 12))
fit2 <- arima(Y, c(0, 1, 1), seasonal = list(order = c(1, 1, 0), period = 12))
fit3 <- arima(Y, c(2, 1, 1), seasonal = list(order = c(1, 1, 0), period = 12))
fit1
fit2
fit3

## 모형 적합성 진단 ##

tsdiag(fit2)

## 모형 확정 및 예측 ##

diff12_Y.forecasts <- forecast(fit2, h = 12)
diff12_Y.forecasts
plot(diff12_Y.forecasts)
