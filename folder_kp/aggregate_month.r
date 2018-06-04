
library(forecast)
library(dplyr)
library(lubridate)
library(doBy)

seoul<-read.csv("seoul_gu2014_201709.csv",header=T,stringsAsFactors=F)
str(seoul)
seoul<-seoul %>% mutate(�⵵=year(as.Date(������))) %>% mutate(��=month(as.Date(������)))
seoul_ag<-aggregate(PM10~�����Ҹ�+�⵵+��,seoul,mean,na.rm=T,na.action=NULL)
seoul_ag<-orderBy(data=seoul_ag,formula=~�����Ҹ�+�⵵+��)

###�� ������ 2014,2015,2016�� �����͸� ���� 2017�� 9������ �����ϱ�


gu<-unique(seoul_ag$�����Ҹ�)
b<-c()
for (i in gu){
  assign(paste("part_",i,sep=''),seoul_ag[seoul_ag$�����Ҹ�==i,])
  a<-paste("part_",i,sep='')
  b<-append(b,a)
}

## ������ ��õ��(b[c(4,19)])�� �����ϰ�� ��� c(0,0,0,0,12,0,1)
# ������ ��õ�� c(0,0,1,0,12,0,1)


for (i in b){
  PM<-get(i)[get(i)$�⵵ %in% c(2014,2015,2016),]
  time<-ts(PM$PM10,frequency=12,start=c(2014,1))
  if(i!='part_������' & i != 'part_��õ��'){
    fit<-Arima(time,order=c(0,0,0),seasonal=list(order=c(0,1,0),period=12))
  }
  else{
    fit<-Arima(time,order=c(0,0,0),seasonal=list(order=c(1,1,0),period=12))
  }
  pred<-forecast(fit,h=9)
  df_gu<-get(i)[get(i)$�⵵== '2017',] %>% mutate(����=pred$mean) %>% mutate(����=PM10-����)
  assign(paste("pre_",i,sep=''),df_gu)
}


pred_gu<-paste("pre_",b,sep="")
er<-c()
for (i in pred_gu){
  a<-mean(abs(get(i)$����))
  er<-append(er,a)
}
mean(er)
summary(er)
boxplot(er)
hist(er)

er1<-c()
for (i in pred_gu){
  a1<-get(i)$����
  er1<-append(er1,a1)
}

hist(er1)
summary(er1)
boxplot(er1)
mean(er1)

er1<-c()
for (i in pred_gu){
  a1<-get(i)$����
  er1<-append(er1,a1)
}
str(er1)
summary(er1)
boxplot(er1)
hist(er1)

