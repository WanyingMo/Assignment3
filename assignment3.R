moment_estimator<-function(data,distribution){
#Binomial
if (distribution=="Binomial"){
  bi<-rbinom(data[1],data[2],data[3])
  mubi2=var(bi)+mean(bi)*mean(bi)
  pbi=sqrt((mean(bi)-mubi2)/(data[2]-data[2]*data[2]))
  return(pbi)
}
  
else if(distribution=="Geometric"){
  geo<-rgeom(data[1],data[2])
  mean=mean(geo)+1
  muge=var(geo)+mean*mean
  pge=(-1+sqrt(1+8*muge))/(2*muge)
  return(pge)
}

else if(distribution=="Poisson"){
  pois<-rpois(data[1],data[2])
  mupo=var(pois)+mean(pois)*mean(pois)
  lambda=(-1+sqrt(1+4*mupo))/2
  return(lambda)
}
  
else if(distribution=="Uniform"){
  uni<-runif(data[1],data[2],data[3])
  a=mean(uni)-sqrt(3*var(uni))
  b=mean(uni)+sqrt(3*var(uni))
  return(c(a,b))
}
  
else if(distribution=="Normal"){
  no<-rnorm(data[1], mean = data[2], sd = data[3])
  muno=mean(no)
  sig2=sqrt(var(no))
  return(c(muno,sig2))
}
  
else if(distribution=="Exponential"){
  exp<-rexp(data[1],data[2])
  muex=var(exp)+mean(exp)*mean(exp)
  rate=sqrt(muex/2)
  return(rate)
}
  
else if(distribution=="Beta"){ 
  #Beta
  beta<-rbeta(data[1],data[2],data[3])
  ex_2=var(beta)+mean(beta)*mean(beta)
  m=mean(beta)
  alpha=(m*m-ex_2*m)/(ex_2-m*m)
  beta=(1-m)*alpha/m
  return(c(alpha,beta))
}
  
else if(distribution=="Multinormial"){ 
  m<-data[[1]]
  p<-data[[2]]
  mul<-sample(m,1000,replace=TRUE,p)
  nmul=as.numeric(table(mul))
  pmul=c(nmul[1]/1000,nmul[2]/1000,nmul[3]/1000)
  return(pmul)
}

else if(distribution=="Mul_Normal"){
  library(MASS)
  mean=data[[1]]
  sigma=data[[2]]
  muln<-mvrnorm(1000,mean,sigma)
  mumn<-c(sum(muln[1:1000,1])/1000,sum(muln[1:1000,2])/1000,sum(muln[1:1000,3])/1000)
  mumn<-as.matrix(unlist(mumn))
  vamn2<-matrix(c(0,0,0,0,0,0,0,0,0),nrow=3,ncol=3)
  for (i in 1:1000){
    vamn<-as.matrix(unlist(muln[i,1:3]))-mumn
    vamn1<-t(as.matrix(unlist(muln[i,1:3]))-mumn)
    vamn2<-vamn2+vamn%*%vamn1
  }
  return(list(mumn,vamn2/1000))
}
}
  moment_estimator(c(1000,100,0.5),"Binomial")
  moment_estimator(c(1000,0.5),"Geometric")
  moment_estimator(c(1000,0.5),"Poisson")  
  moment_estimator(c(1000,0,1),"Uniform")  
  moment_estimator(c(1000,0,2),"Normal")  
  moment_estimator(c(100,1),"Exponential")  
  moment_estimator(c(1000,0.5,1),"Beta")  
  moment_estimator(list(c(1,2,3),c(0.1,0.3,0.6)),"Multinormial")
  moment_estimator(list(c(1,2,3),matrix(c(1,0,0,0,1,0,0,0,1),nrow=3,ncol=3)),"Mul_Normal")  
