


library(lubridate)
library(dplyr)
library(doBy)
setwd("c:/R")

make_smoke<-function(x){
  y<-read.csv(x,header=T,stringsAsFactors=T)
  y<-y[,c(1,3,4,9)]
  y<-y[substr(as.character(y$지역),1,2)=="서울",]
  y$측정일시<-substr(as.character(y$측정일시),9,10)
  y$측정소명[y$측정소명=='도산대로']<-'강남구'
  y$측정소명[y$측정소명=='천호대로']<-'강동구'
  y$측정소명[y$측정소명=='공항대로']<-'강서구'
  y$측정소명[y$측정소명=='화랑로']<-'노원구'
  y$측정소명[y$측정소명=='홍릉로']<-'동대문구'
  y$측정소명[y$측정소명=='동작대로 중앙차로']<-'동작구'
  y$측정소명[y$측정소명=='신촌로']<-'마포구'
  y$측정소명[y$측정소명=='강남대로']<-'서초구'
  y$측정소명[y$측정소명=='강변북로']<-'성동구'
  y$측정소명[y$측정소명=='정릉로']<-'성북구'
  y$측정소명[y$측정소명=='영등포로']<-'영등포구'
  y$측정소명[y$측정소명=='한강대로']<-'용산구'
  y$측정소명[y$측정소명=='종로']<-'종로구'
  y$측정소명[y$측정소명=='청계천로']<-'중구'
  return(y)
}

a<-make_smoke("2014년 1분기.csv")
b<-make_smoke("2014년 2분기.csv")
c<-make_smoke("2014년 3분기.csv")
d<-make_smoke("2014년 4분기.csv")
e<-make_smoke("2015년1분기-1.csv")
f<-make_smoke("2015년2분기-1.csv")
g<-make_smoke("2015년3분기-1.csv")
h<-make_smoke("2015년4분기-1.csv")
i<-make_smoke("2016년 1분기.csv")
j<-make_smoke("2016년 2분기.csv")
k<-make_smoke("2016년 3분기.csv")
l<-make_smoke("2016년 4분기.csv")
m<-make_smoke("2017_first.csv")
n<-make_smoke("2017_second.csv")
o<-make_smoke("2017_third.csv")

seoul_gu<-rbind(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)
seoul_gu$지역 <- as.character(seoul_gu$지역)
seoul_gu$지역 <- as.factor(seoul_gu$지역)
seoul_gu$측정소명<-as.character(seoul_gu$측정소명)
seoul_gu$측정소명<-as.character(seoul_gu$측정소명)


ag_hour<-aggregate(PM10~측정소명+측정일시,seoul_gu,mean,na.rm=T,na.action=NULL)
ag_hour_ord<-orderBy(data=ag_hour,formula=~측정소명+측정일시)

library(ggplot2)

### 각 구별 시간대별 미세먼지
ggplot(data=ag_hour_ord,aes(x=측정일시,y=PM10,group=측정소명,col=측정소명))+geom_line()+facet_wrap(~측정소명)

ggplot(data=ag_hour_ord[ag_hour_ord$측정소명=='중랑구',],aes(x=측정일시,y=PM10,fill=측정소명))+
  geom_bar(stat="identity")+coord_polar()

ggplot(data=ag_hour_ord[ag_hour_ord$측정소명=='중랑구',],aes(x=측정일시,y=PM10,col=측정소명,group=측정소명))+
  geom_line()



###  각 구별 na 갯수 
sum_na<-function(x){
  sum(is.na(x))
}

number_na<-aggregate(PM10~측정소명,seoul_gu,sum_na,na.action=NULL)








######## 각 구별 월 평균 미세먼지 분포

seoulmon<-read.csv("seoul_gu2014_201709.csv",header=T,stringsAsFactors=T)

monseoul <- seoulmon %>% mutate(월=month(as.Date(측정일))) 

agmonth<-aggregate(PM10~측정소명+월,monseoul,mean,na.rm=T,na.action=NULL)

agmonth$월<-as.factor(agmonth$월)

ggplot(agmonth,aes(x=월,y=PM10,fill=측정소명))+geom_bar(stat="identity")+facet_wrap(~측정소명)


##### 각 구별 미세먼지 평균 (기간 201401--201709)
seoulmon<-read.csv("seoul_gu2014_201709.csv",header=T,stringsAsFactors=T)
monseoul <- seoulmon %>% mutate(월=month(as.Date(측정일))) 
aggu<-aggregate(PM10~측정소명,monseoul,mean,na.rm=T,na.action=NULL)

ggplot(data=aggu,aes(x=측정소명,y=PM10,fill=측정소명))+geom_bar(stat="identity")
boxplot(aggu$PM10)

