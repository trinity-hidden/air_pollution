
library(forecast)
library(dplyr)
library(lubridate)
library(doBy)

seoul<-read.csv("seoul_gu2014_201709.csv",header=T,stringsAsFactors=F)
str(seoul)
seoul<-seoul %>% mutate(년도=year(as.Date(측정일))) %>% mutate(달=month(as.Date(측정일)))
seoul_ag<-aggregate(PM10~측정소명+년도+달,seoul,mean,na.rm=T,na.action=NULL)
seoul_ag<-orderBy(data=seoul_ag,formula=~측정소명+년도+달)

###각 지역을 2014,2015,2016년 데이터를 통해 2017년 9월까지 추정하기


gu<-unique(seoul_ag$측정소명)
b<-c()
for (i in gu){
  assign(paste("part_",i,sep=''),seoul_ag[seoul_ag$측정소명==i,])
  a<-paste("part_",i,sep='')
  b<-append(b,a)
}

## 강서구 양천구(b[c(4,19)])를 제외하고는 모두 c(0,0,0,0,12,0,1)
# 강서구 양천구 c(0,0,1,0,12,0,1)


for (i in b){
  PM<-get(i)[get(i)$년도 %in% c(2014,2015,2016),]
  time<-ts(PM$PM10,frequency=12,start=c(2014,1))
  if(i!='part_강서구' & i != 'part_양천구'){
    fit<-Arima(time,order=c(0,0,0),seasonal=list(order=c(0,1,0),period=12))
  }
  else{
    fit<-Arima(time,order=c(0,0,0),seasonal=list(order=c(1,1,0),period=12))
  }
  pred<-forecast(fit,h=9)
  df_gu<-get(i)[get(i)$년도== '2017',] %>% mutate(as.numeric(예측=pred$mean)) %>% mutate(오차=PM10-예측)
  assign(paste("pre_",i,sep=''),df_gu)
}


pred_gu<-paste("pre_",b,sep="")
er<-c()
for (i in pred_gu){
  a<-mean(abs(get(i)$오차))
  er<-append(er,a)
}
mean(er)
summary(er)
boxplot(er)
hist(er)

er1<-c()
for (i in pred_gu){
  a1<-get(i)$오차
  er1<-append(er1,a1)
}

hist(er1)
summary(er1)
boxplot(er1)
mean(er1)

er1<-c()
for (i in pred_gu){
  a1<-get(i)$오차
  er1<-append(er1,a1)
}
str(er1)
summary(er1)
boxplot(er1)
hist(er1)



all_gu<-rbind(get(pred_gu[1]),get(pred_gu[2]),get(pred_gu[3]),get(pred_gu[4]),get(pred_gu[5]),get(pred_gu[6]),get(pred_gu[7]),get(pred_gu[8]),
              get(pred_gu[9]),get(pred_gu[10]),get(pred_gu[11]),get(pred_gu[12]),get(pred_gu[13]),get(pred_gu[14]),get(pred_gu[15]),get(pred_gu[16]),
              get(pred_gu[17]),get(pred_gu[18]),get(pred_gu[19]),get(pred_gu[20]),get(pred_gu[21]),get(pred_gu[22]),get(pred_gu[23]),get(pred_gu[24]),
              get(pred_gu[25]))



abs_mean<-function(x){
  return(mean(abs(x),na.rm=T))
}

ag_gu <- aggregate(오차~측정소명,all_gu,abs_mean,na.action=na.omit)
ag_gu <- ag_gu[order(ag_gu$오차,decreasing=T),]
ag_mon <- aggregate(오차~달,all_gu,abs_mean,na.action=na.omit)
ag_mon <- ag_mon[order(ag_mon$오차,decreasing=T),]
ag_mongu <- aggregate(오차~측정소명+달,all_gu,abs_mean,na.action=na.omit)
ag_mongu <- ag_mongu[order(ag_mongu$오차,decreasing=T),]






