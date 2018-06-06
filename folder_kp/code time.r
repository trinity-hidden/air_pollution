##### R �� �ڵ� �� �ӵ�   

time1 <- Sys.time()
a<-c()  # Ÿ�԰� ũ�� ���Ҽ� ������ ���ϴ°� ���� 
for (i in 1:100000){
  x<-i^2
  a<-append(a,x)
}
roop_nontype_append<-(Sys.time() - time1)
# 40��

time2 <- Sys.time()
a<-integer()
for (i in 1:100000){
  x<-i^2
  a<-append(a,x)
}
roop_typeinteger_append<-(Sys.time() - time2)
# 20��

time3 <- Sys.time()
a<-integer(100000)
for (i in 1:100000){
  a[i]<-i^2
  
}
roop_typeinteger_index<-(Sys.time() - time3)
#1.5 �� 

time4 <- Sys.time()
a<-c()
for (i in 1:100000){
  a[i]<-i^2
  
}
roop_nontype_index<-(Sys.time() - time4)
# 5.4�� 


time5 <- Sys.time()
a<-integer()
for (i in 1:100000){
  a[i]<-i^2
  
}
roop_typeinteger_index<-(Sys.time() - time5)
# 2.9�� 


time<-Sys.time()
x<-c(1:100000)
x<-x^2
vector_1 <-Sys.time()-time

# 0.04 ��

time6<-Sys.time()
x<-matrix(c(1:100000),100)
x<-apply(x,1,mod)
apply_matrix<-Sys.time() - time6

# 0.05 �� 

mod<-function(x){
  return(x^2)
}


####### ���� for ���� ��� �ȴٸ� ����־���� ������ Ÿ�԰� ũ�⸦ �����ټ� �ִٸ� �����ִ°��� ����

