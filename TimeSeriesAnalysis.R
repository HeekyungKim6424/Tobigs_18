#install.packages("TTR")
#install.packages("forecast")
#("tseries")

library(TTR)
library(forecast)
library(tseries)


library(dplyr)
library(data.table)

data<-read.table('C:/Users/user/Desktop/programing/Tobigs/Week5_Time Series Analysis/과제/Electric_Production.csv',sep=",", header = TRUE)
data$Date=as.ts(data$DATE)


#이건 시도해본 코드...
# data1=data[,-c(1)]
# 
# 
# plot(data1$IPG2211A2N)
# library(ggplot2)
# 
# #dev.off()
# ggplot(data, aes(x=DATE,y=IPG2211A2N,group=1)) + geom_line(color="black", size=1)+scale_y_continuous(expand = c(0,0)) #파이썬으로 그렸을때는 분명 추세가 보였는데 r로 그리니깐 추세가 안보인다....ggplot으로 그리면 안되겠다. 그냥 나와있는대로 해보자


data.ts<-ts(data$IPG2211A2N)
plot(data.ts) #이제 좀 추세선이 보인다.... 근데 그럼 date는 어떻게 되는거지...
#그래프를 그려보니 추세와, 점점갈수록 폭이 넓어지고, 주기성도 있는것 같다.

#일단 차분을 하기 전에 로그변환을 하여 변동 폭이 점점 넓어지는것을 막아준다
log_df_ts <- log(data.ts)
plot(log_df_ts)

#차분 해주기
data.diff1=diff(log_df_ts,differences = 1)
plot(data.diff1)
data.diff2=diff(log_df_ts,differences = 2)
plot(data.diff2)
data.diff3=diff(log_df_ts,differences = 3)
plot(data.diff3)
#아까 위에 그래프보다 나아진것 같다...
#차분은 대부분 많이 해봤자 2차까지 하는데 혹시 몰라서 3차 해봤더니....좀 아닌것 같다.
#차분은 2차!!

acf(data.diff2,lag.max = 20)#20으로 해도 점점 폭이 줄긴 하는데 boundary 안으로는 안들어오는것 같다.
acf(data.diff2,lag.max = 40)
acf(data.diff2,lag.max = 60) #줄어들 기미가 안보이는데...

pacf(data.diff2,lag.max = 20) #AR(13)? #로그변환 해줬는데 AR(7)도 될것 같다. 아니면 AR(12)?? 

auto.arima(data.diff2) #ARIMA(1,0,0)...거짓말 아마 아까 차분 말고 뭘 더 해줬어야 했나보다...
#그래서 로그변환 해줬는데 딱히 변화가 있지는 않은것 같다.....ARIMA(2,0,2)

#결론: MA모델은 acf가 0으로 줄어들 기미가 안보이고,
#      AR모델은 pacf가 7 아니면 12 일것 같은데...
#      학교에서 AR MA 모델 배울때 대부분 1차나 2차에서 끝난다고 기억하는데, 너무 값이 많아져서 별로인것 같다.
#      AR(12)는 주기성인가,?
#      auto.arima에서 ARIMA(2,0,2)가 제일 설득력 있는 모델인것 같다.

Arima(data.diff2,order=c(2,0,2)) #AICc=-1356.84
Arima(data.diff2,order=c(2,1,2)) #AICc=-1112.2
Arima(data.diff2,order=c(2,2,2)) #AICc=-800.75
Arima(data.diff2,order=c(3,2,2)) #AICc=-942.29
Arima(data.diff2,order=c(3,0,2)) #AICc=-1362.16 #최종모델 선택!
Arima(data.diff2,order=c(3,0,3)) #AICc=-1358.62
Arima(data.diff2,order=c(3,1,2)) #AICc=-1260.6

#예측
train=subset(data.diff2,end=length(data.diff2)-10)
test=subset(data.diff2,start=length(data.diff2)-9)
plot(train)

fit1=Arima(train,order=c(3,0,2))
fit1 %>% forecast(h=9)%>% autoplot()+autolayer(test) #와 너무 신기하다!!
