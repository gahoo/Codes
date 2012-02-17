Hypergeometric<-function(x,m,k,n){
  choose(k,x)*choose(m-k,n-x)/choose(m,n)
}

HypergeometricCDF<-function(x,M,K,N){
  sum=0
  for(i in 1:x){
    sum<-sum+hype(i,M,K,N)
  }
  sum
}
#######################################
# New Version
# More Concise
Hypergeometric<-function(i,m1,m2,N){
  choose(m1,i)*choose(N-m1,m2-i)/choose(N,m2)
}

HypergeometricCDF<-function(pars=NULL,x,m1,m2,N){
  if(!is.null(pars)){
    x=pars[1]
    m1=pars[2]
    m2=pars[3]
    N=pars[4]
  }
  sum(sapply(x:min(m1,m2),Hypergeometric,m1,m2,N))
}
#######################################
# Coidx
cooperationindex<-function(pars=NULL,a,b,c,d){
  if(!is.null(pars)){
    a=pars[1]
    b=pars[2]
    c=pars[3]
    d=pars[4]
  }
  m=a+b
  n=c+d
  r=a+c
  s=b+d
  (a*d-b*c)/sqrt(m*n*r*s)
}
#######################################
# Randomize Matrix
# !Needs modify, otherwise NaN
runint<-function(maximum){
  as.integer(runif(1,0,maximum))
}

randMatrix<-function(num,maximum){
  a=as.integer(runif(num,0,maximum))
  m=maximum-a
  b=sapply(m,runint)
  m=m-b
  c=sapply(m,runint)
  d=m-c
  cbind(a,b,c,d)
}

##################
time_test<-function(num,maximum){
  mm=randMatrix(num,maximum)
  co_time<-system.time(coidx<-apply(mm,1,cooperationindex))[[1]]
  hy_time<-system.time(hypecdf<-apply(mm,1,HypergeometricCDF))[[1]]
  c(co_time,hy_time)
}


################
# Test
cooperationindex(a=303,b=1012,c=787,d=3174)
Hypergeometric(i=20,m1=60,m2=80,N=800)
HypergeometricCDF(x=02,m1=60,m2=80,N=800)
mm=randMatrix(100,1000)
apply(mm,1,sum)
system.time(coidx<-apply(mm,1,cooperationindex))
system.time(hypecdf<-apply(mm,1,HypergeometricCDF))
cbind(mm,coidx,hypecdf)

tobetest<-c(1:20)*500
result<-sapply(tobetest,time_test,1000);result
plot(tobetest,result[1,])
plot(tobetest,result[2,])
plot(result[1,],result[2,])