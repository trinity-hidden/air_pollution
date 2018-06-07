##### R 로 코드 별 속도   

time1 <- Sys.time()
a<-c()  # 타입과 크기 정할수 있으면 정하는게 좋음 
for (i in 1:100000){
  x<-i^2
  a<-append(a,x)
}
roop_nontype_append<-(Sys.time() - time1)
# 40초

time2 <- Sys.time()
a<-integer()
for (i in 1:100000){
  x<-i^2
  a<-append(a,x)
}
roop_typeinteger_append<-(Sys.time() - time2)
# 20초

time3 <- Sys.time()
a<-integer(100000)
for (i in 1:100000){
  a[i]<-i^2
  
}
roop_typeinteger_index<-(Sys.time() - time3)
#1.5 초 

time4 <- Sys.time()
a<-c()
for (i in 1:100000){
  a[i]<-i^2
  
}
roop_nontype_index<-(Sys.time() - time4)
# 5.4초 


time5 <- Sys.time()
a<-integer()
for (i in 1:100000){
  a[i]<-i^2
  
}
roop_typeinteger_index<-(Sys.time() - time5)
# 2.9초 


time<-Sys.time()
x<-c(1:100000)
x<-x^2
vector_1 <-Sys.time()-time

# 0.04 초

time6<-Sys.time()
x<-matrix(c(1:100000),100)
x<-apply(x,1,mod)
apply_matrix<-Sys.time() - time6

# 0.05 초 

mod<-function(x){
  return(x^2)
}


####### 굳이 for 문을 써야 된다면 담아주어야할 변수의 타입과 크기를 정해줄수 있다면 정해주는것이 좋다

