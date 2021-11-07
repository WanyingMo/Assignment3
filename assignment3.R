#Binomial
bi<-rbinom(1000,100,0.5)
mubi2=var(bi)+mean(bi)*mean(bi)
pbi=sqrt((mean(bi)-mubi2)/(100-100*100))

#Geometric
geo<-rgeom(1000,0.5)
mean=mean(geo)+1
muge=var(geo)+mean*mean
pge=(-1+sqrt(1+8*muge))/(2*muge)

#Poisson
pois<-rpois(1000,0.5)
mupo=var(pois)+mean(pois)*mean(pois)
lambda=(-1+sqrt(1+4*mupo))/2

#Uniform
uni<-runif(1000,0,1)
a=mean(uni)-sqrt(3*var(uni))
b=mean(uni)+sqrt(3*var(uni))

#Normal
no<-rnorm(1000, mean = 0, sd = 1)
muno=mean(no)
sig2=sqrt(var(no))

#Exponential
exp<-rexp(100,1)
muex=var(exp)+mean(exp)*mean(exp)
rate=sqrt(muex/2)

#Beta
beta<-rbeta(1000,0.5,1)
ex_2=var(beta)+mean(beta)*mean(beta)
m=mean(beta)
alpha=(m*m-ex_2*m)/(ex_2-m*m)
beta=(1-m)*alpha/m

#Multinomial
m<-c(1,2,3)
p<-c(0.1,0.3,0.6)
mul<-sample(m,1000,replace=TRUE,p)
nmul=as.numeric(table(mul))
pmul=c(nmul[1]/1000,nmul[2]/1000,nmul[3]/1000)

#Multivariate Normal
library(MASS)
mean=c(1,2,3)
sigma=matrix(c(1,0,0,0,1,0,0,0,1),nrow=3,ncol=3)
muln<-mvrnorm(1000,mean,sigma)
mumn<-c(sum(muln[1:1000,1])/1000,sum(muln[1:1000,2])/1000,sum(muln[1:1000,3])/1000)
mumn<-as.matrix(unlist(mumn))
vamn2<-matrix(c(0,0,0,0,0,0,0,0,0),nrow=3,ncol=3)
for (i in 1:1000){
vamn<-as.matrix(unlist(muln[i,1:3]))-mumn
vamn1<-t(as.matrix(unlist(muln[i,1:3]))-mumn)
vamn2<-vamn2+vamn%*%vamn1
}

list1<-list(distribution="Binomial",p=pbi)
list2<-list(distribution="Geometric",p=pge)
list3<-list(distribution="Poisson",lambda=lambda)
list4<-list(distribution="Uniform",a=a,b=b)
list5<-list(distribution="Normal",mu=muno,sigma=sig2)
list6<-list(distribution="Exponential",rate=rate)
list7<-list(distribution="Beta",alpha=alpha,beta=beta)
list8<-list(distribution="Multinomial",p=pmul)
list9<-list(distribution="Multivariate Normal",mu=mumn,covariance_matrix=vamn2/1000)
print(c(list1,list2,list3,list4,list5,list6,list7,list8,list9))
