


library(lubridate)
library(dplyr)
library(doBy)
setwd("c:/R")

make_smoke<-function(x){
  y<-read.csv(x,header=T,stringsAsFactors=T)
  y<-y[,c(1,3,4,9)]
  y<-y[substr(as.character(y$����),1,2)=="����",]
  y$�����Ͻ�<-substr(as.character(y$�����Ͻ�),9,10)
  y$�����Ҹ�[y$�����Ҹ�=='������']<-'������'
  y$�����Ҹ�[y$�����Ҹ�=='õȣ���']<-'������'
  y$�����Ҹ�[y$�����Ҹ�=='���״��']<-'������'
  y$�����Ҹ�[y$�����Ҹ�=='ȭ����']<-'�����'
  y$�����Ҹ�[y$�����Ҹ�=='ȫ����']<-'���빮��'
  y$�����Ҹ�[y$�����Ҹ�=='���۴�� �߾�����']<-'���۱�'
  y$�����Ҹ�[y$�����Ҹ�=='���̷�']<-'������'
  y$�����Ҹ�[y$�����Ҹ�=='�������']<-'���ʱ�'
  y$�����Ҹ�[y$�����Ҹ�=='�����Ϸ�']<-'������'
  y$�����Ҹ�[y$�����Ҹ�=='������']<-'���ϱ�'
  y$�����Ҹ�[y$�����Ҹ�=='��������']<-'��������'
  y$�����Ҹ�[y$�����Ҹ�=='�Ѱ����']<-'��걸'
  y$�����Ҹ�[y$�����Ҹ�=='����']<-'���α�'
  y$�����Ҹ�[y$�����Ҹ�=='û��õ��']<-'�߱�'
  return(y)
}

a<-make_smoke("2014�� 1�б�.csv")
b<-make_smoke("2014�� 2�б�.csv")
c<-make_smoke("2014�� 3�б�.csv")
d<-make_smoke("2014�� 4�б�.csv")
e<-make_smoke("2015��1�б�-1.csv")
f<-make_smoke("2015��2�б�-1.csv")
g<-make_smoke("2015��3�б�-1.csv")
h<-make_smoke("2015��4�б�-1.csv")
i<-make_smoke("2016�� 1�б�.csv")
j<-make_smoke("2016�� 2�б�.csv")
k<-make_smoke("2016�� 3�б�.csv")
l<-make_smoke("2016�� 4�б�.csv")
m<-make_smoke("2017_first.csv")
n<-make_smoke("2017_second.csv")
o<-make_smoke("2017_third.csv")

seoul_gu<-rbind(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)
seoul_gu$���� <- as.character(seoul_gu$����)
seoul_gu$���� <- as.factor(seoul_gu$����)
seoul_gu$�����Ҹ�<-as.character(seoul_gu$�����Ҹ�)
seoul_gu$�����Ҹ�<-as.character(seoul_gu$�����Ҹ�)


ag_hour<-aggregate(PM10~�����Ҹ�+�����Ͻ�,seoul_gu,mean,na.rm=T,na.action=NULL)
ag_hour_ord<-orderBy(data=ag_hour,formula=~�����Ҹ�+�����Ͻ�)

library(ggplot2)

### �� ���� �ð��뺰 �̼�����
ggplot(data=ag_hour_ord,aes(x=�����Ͻ�,y=PM10,group=�����Ҹ�,col=�����Ҹ�))+geom_line()+facet_wrap(~�����Ҹ�)

ggplot(data=ag_hour_ord[ag_hour_ord$�����Ҹ�=='�߶���',],aes(x=�����Ͻ�,y=PM10,fill=�����Ҹ�))+
  geom_bar(stat="identity")+coord_polar()

ggplot(data=ag_hour_ord[ag_hour_ord$�����Ҹ�=='�߶���',],aes(x=�����Ͻ�,y=PM10,col=�����Ҹ�,group=�����Ҹ�))+
  geom_line()



###  �� ���� na ���� 
sum_na<-function(x){
  sum(is.na(x))
}

number_na<-aggregate(PM10~�����Ҹ�,seoul_gu,sum_na,na.action=NULL)








######## �� ���� �� ��� �̼����� ����

seoulmon<-read.csv("seoul_gu2014_201709.csv",header=T,stringsAsFactors=T)

monseoul <- seoulmon %>% mutate(��=month(as.Date(������))) 

agmonth<-aggregate(PM10~�����Ҹ�+��,monseoul,mean,na.rm=T,na.action=NULL)

agmonth$��<-as.factor(agmonth$��)

ggplot(agmonth,aes(x=��,y=PM10,fill=�����Ҹ�))+geom_bar(stat="identity")+facet_wrap(~�����Ҹ�)


##### �� ���� �̼����� ��� (�Ⱓ 201401--201709)
seoulmon<-read.csv("seoul_gu2014_201709.csv",header=T,stringsAsFactors=T)
monseoul <- seoulmon %>% mutate(��=month(as.Date(������))) 
aggu<-aggregate(PM10~�����Ҹ�,monseoul,mean,na.rm=T,na.action=NULL)

ggplot(data=aggu,aes(x=�����Ҹ�,y=PM10,fill=�����Ҹ�))+geom_bar(stat="identity")
boxplot(aggu$PM10)

