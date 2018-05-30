```{r}

y2014_1 <- read.csv("c:/2014/2014년 1분기.csv", header = T, stringsAsFactors = F)
head(y2014_1)

str(y2014_1)
sum(!is.na(y2014_1$PM25))                   # PM25의 na 값이 아닌 갯수 확인
y2014_1$PM25 <- NULL                       # PM25 열 제거
unique(y2014_1$지역)                       # 지역 확인
y2014_1 <- y2014_1[y2014_1$지역 =="서울", ]      # 지역에서 '서울'만 추림
unique(y2014_1$주소)        
a <- strsplit(y2014_1$주소, " ")
sum(is.na(a))                              #  na갯수 확인

for(i in 1:length(a)){
            y2014_1$주소[i] <-  a[[i]][2]
}

unique(y2014_1$주소)
y2014_1$주소 <- gsub("강서로45다길", "강서구", y2014_1$주소)


y2014_1$측정일시 <- substr(y2014_1$측정일시, 1, 8)

y2014_1 <- y2014_1[ ,c('측정일시', '주소', 'PM10')]
colnames(y2014_1) <- c('날짜', '구별', '미세먼지')

df<-aggregate(미세먼지~날짜+구별, y2014_1, mean)
str(df)

df$날짜 <- as.Date(df$날짜, "%Y%m%d")
df


```

```{r}
library(ggplot2)

df1<- df[df$구별 %in% c("강남구","서초구","송파구"), ]
df1
tail(df1)
ggplot(df1, aes(날짜, 미세먼지, linetype=구별, color = 구별))+ geom_line()
```

```{r}

gn <- df[df$구별 =="강남구", c("미세먼지")]
gn <- ts(gn, start= as.integer(min(df$날짜)), end = max(df$날짜))
gn
plot(gn)

```



