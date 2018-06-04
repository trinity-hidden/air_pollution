
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
  df_gu<-get(i)[get(i)$�⵵== '2017',] %>% mutate(as.numeric(����=pred$mean)) %>% mutate(����=PM10-����)
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



all_gu<-rbind(get(pred_gu[1]),get(pred_gu[2]),get(pred_gu[3]),get(pred_gu[4]),get(pred_gu[5]),get(pred_gu[6]),get(pred_gu[7]),get(pred_gu[8]),
              get(pred_gu[9]),get(pred_gu[10]),get(pred_gu[11]),get(pred_gu[12]),get(pred_gu[13]),get(pred_gu[14]),get(pred_gu[15]),get(pred_gu[16]),
              get(pred_gu[17]),get(pred_gu[18]),get(pred_gu[19]),get(pred_gu[20]),get(pred_gu[21]),get(pred_gu[22]),get(pred_gu[23]),get(pred_gu[24]),
              get(pred_gu[25]))



abs_mean<-function(x){
  return(mean(abs(x),na.rm=T))
}

ag_gu <- aggregate(����~�����Ҹ�,all_gu,abs_mean,na.action=na.omit)
ag_gu <- ag_gu[order(ag_gu$����,decreasing=T),]
ag_mon <- aggregate(����~��,all_gu,abs_mean,na.action=na.omit)
ag_mon <- ag_mon[order(ag_mon$����,decreasing=T),]
ag_mongu <- aggregate(����~�����Ҹ�+��,all_gu,abs_mean,na.action=na.omit)
ag_mongu <- ag_mongu[order(ag_mongu$����,decreasing=T),]






