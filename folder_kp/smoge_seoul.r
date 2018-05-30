library(lubridate)
library(dplyr)

make_smoke<-function(x){
  y<-read.csv(x,header=T,stringsAsFactors=T)
  y<-y[,c(1,2,3,4,9,11)]
  y<-y[substr(as.character(y$지역),1,2)=="서울",]
  y<-y %>% mutate(측정일=as.Date(as.character(y$측정일시),format=('%Y%m%d')))
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
  y<-aggregate(PM10~측정소명+측정일,y,mean,na.rm=T,na.action=NULL)
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

time1<-ts(seoul_gu[seoul_gu$측정소명=='강남구',c(3)])
plot(time1)






surface_2014<-read.csv("SURFACE_ASOS_108_HR_2014_2014_2018.csv",header=T,stringsAsFactors=F)
surface_2015<-read.csv("SURFACE_ASOS_108_HR_2015_2015_2018.csv",header=T,stringsAsFactors=F)
surface_2016<-read.csv("SURFACE_ASOS_108_HR_2016_2016_2017.csv",header=T,stringsAsFactors=F)
surface_2017<-read.csv("SURFACE_ASOS_108_HR_2017_2017_2018.csv",header=T,stringsAsFactors=F)
getwd()

surface2014_2017<-rbind(surface_2014,surface_2015,surface_2016,surface_2017)


surface_1<-surface[,c(1,2,3,4,6,7,8,9,10,11,12,19,24,25,26,27,28)]
surface<-read.csv("surface2014_2017.csv",header=T,stringsAsFactors=T)


# 왜 a<-c() 를 하면 오류메세지가 뜨지 않는가?
mostfreq<-function(x){
  a<-c()
  a<-sort(table(x),decreasing=T)[1]
  b<-c()
  b<-names(a)
  return(b)
}
sort(table(surface_1$풍향.16방위.),decreasing=T)[1]

surface_2<-surface_1 %>% mutate(일=as.Date(surface_1$일시))
surface_2

surface_2$운형.운형약어.<-as.character(surface_2$운형.운형약어.)

surface_4<-aggregate(.~일,surface_3,mean,na.rm=T,na.action=NULL)

surface_3<-surface_2[,-c(1,2,3,6,12)]

surface_5<-aggregate(cbind(풍향.16방위.,운형.운형약어.)~일,surface_2,mostfreq,na.action=NULL)

surface_5$풍향.16방위.<-as.factor(surface_5$풍향.16방위.)
surface_5$운형.운형약어.<-as.factor(surface_5$운형.운형약어.)

surface_6<-merge(surface_4,surface_5,all=T,by='일')


seoulgu<-read.csv("seoul_gu2014_201709.csv",header=T,stringsAsFactors=F)
surface<-read.csv("surface_day.csv",header=T,stringsAsFactors=F)

seoul<-merge(surface,seoulgu,all=FALSE,by.x='일',by.y='측정일')

seouldata<-seoul[,-c(2,17)]
write.csv(seouldata,"seoul_data.csv")








seouldata<-seoul[,-c(2,17)]
write.csv(seouldata,"seoul_data.csv")


seoulomit<-na.omit(seouldata)
cor<-cor(seoulomit[,-c(1,14,15,16)])
library(corrplot)

attach(seoulomit)
운형.운형약어.<-as.factor(운형.운형약어.)
측정소명<-as.factor(측정소명)
풍향.16방위.<-as.factor(풍향.16방위.)
detach(seoulomit)


seoulomit$운형.운형약어.<-as.factor(seoulomit$운형.운형약어.)
seoulomit$측정소명<-as.factor(seoulomit$측정소명)
seoulomit$풍향.16방위.<-as.factor(seoulomit$풍향.16방위.)

seoulomit<-seoulomit %>% mutate('월'=month(seoulomit$일))

seoulomit$월<-as.factor(seoulomit$월)


seoul<-read.csv("seoulomit_month.csv",header=T,stringsAsFactors=T)


tree_seoul<-rpart(PM10~.,data=seoul[,-c(1,2)],cp=0.01)

plot(tree_seoul)
