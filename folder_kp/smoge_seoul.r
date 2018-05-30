library(lubridate)
library(dplyr)

make_smoke<-function(x){
  y<-read.csv(x,header=T,stringsAsFactors=T)
  y<-y[,c(1,2,3,4,9,11)]
  y<-y[substr(as.character(y$����),1,2)=="����",]
  y<-y %>% mutate(������=as.Date(as.character(y$�����Ͻ�),format=('%Y%m%d')))
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
  y<-aggregate(PM10~�����Ҹ�+������,y,mean,na.rm=T,na.action=NULL)
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

time1<-ts(seoul_gu[seoul_gu$�����Ҹ�=='������',c(3)])
plot(time1)






surface_2014<-read.csv("SURFACE_ASOS_108_HR_2014_2014_2018.csv",header=T,stringsAsFactors=F)
surface_2015<-read.csv("SURFACE_ASOS_108_HR_2015_2015_2018.csv",header=T,stringsAsFactors=F)
surface_2016<-read.csv("SURFACE_ASOS_108_HR_2016_2016_2017.csv",header=T,stringsAsFactors=F)
surface_2017<-read.csv("SURFACE_ASOS_108_HR_2017_2017_2018.csv",header=T,stringsAsFactors=F)
getwd()

surface2014_2017<-rbind(surface_2014,surface_2015,surface_2016,surface_2017)


surface_1<-surface[,c(1,2,3,4,6,7,8,9,10,11,12,19,24,25,26,27,28)]
surface<-read.csv("surface2014_2017.csv",header=T,stringsAsFactors=T)


# �� a<-c() �� �ϸ� �����޼����� ���� �ʴ°�?
mostfreq<-function(x){
  a<-c()
  a<-sort(table(x),decreasing=T)[1]
  b<-c()
  b<-names(a)
  return(b)
}
sort(table(surface_1$ǳ��.16����.),decreasing=T)[1]

surface_2<-surface_1 %>% mutate(��=as.Date(surface_1$�Ͻ�))
surface_2

surface_2$����.�������.<-as.character(surface_2$����.�������.)

surface_4<-aggregate(.~��,surface_3,mean,na.rm=T,na.action=NULL)

surface_3<-surface_2[,-c(1,2,3,6,12)]

surface_5<-aggregate(cbind(ǳ��.16����.,����.�������.)~��,surface_2,mostfreq,na.action=NULL)

surface_5$ǳ��.16����.<-as.factor(surface_5$ǳ��.16����.)
surface_5$����.�������.<-as.factor(surface_5$����.�������.)

surface_6<-merge(surface_4,surface_5,all=T,by='��')


seoulgu<-read.csv("seoul_gu2014_201709.csv",header=T,stringsAsFactors=F)
surface<-read.csv("surface_day.csv",header=T,stringsAsFactors=F)

seoul<-merge(surface,seoulgu,all=FALSE,by.x='��',by.y='������')

seouldata<-seoul[,-c(2,17)]
write.csv(seouldata,"seoul_data.csv")








seouldata<-seoul[,-c(2,17)]
write.csv(seouldata,"seoul_data.csv")


seoulomit<-na.omit(seouldata)
cor<-cor(seoulomit[,-c(1,14,15,16)])
library(corrplot)

attach(seoulomit)
����.�������.<-as.factor(����.�������.)
�����Ҹ�<-as.factor(�����Ҹ�)
ǳ��.16����.<-as.factor(ǳ��.16����.)
detach(seoulomit)


seoulomit$����.�������.<-as.factor(seoulomit$����.�������.)
seoulomit$�����Ҹ�<-as.factor(seoulomit$�����Ҹ�)
seoulomit$ǳ��.16����.<-as.factor(seoulomit$ǳ��.16����.)

seoulomit<-seoulomit %>% mutate('��'=month(seoulomit$��))

seoulomit$��<-as.factor(seoulomit$��)


seoul<-read.csv("seoulomit_month.csv",header=T,stringsAsFactors=T)


tree_seoul<-rpart(PM10~.,data=seoul[,-c(1,2)],cp=0.01)

plot(tree_seoul)
