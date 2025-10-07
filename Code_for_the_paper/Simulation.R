#devtools::install_github('HonghyokKim/HEAT')
library(HEAT)
data("seouldat")

library(splines)
library(tsModel)
seouldat$o3_mv01<-runMean(seouldat$o3,0:1)
seouldat$pm10_mv01<-runMean(seouldat$pm10,0:1)
seouldat$t_mean_lag_1<-Lag(seouldat$t_mean,k=1)
seouldat$t_mean_lag_2<-Lag(seouldat$t_mean,k=2)
seouldat$t_mean_lag_3<-Lag(seouldat$t_mean,k=3)


###Only summer data
a_dat<-subset(seouldat,month %in% (6:8))

gen.model<-glm(na_death~ns(t_mean,5)+ns(t_mean_lag_1,5)+ns(t_mean_lag_2,5)+ns(t_mean_lag_3,5)+
                   ns(rh_mean,4)+ns(pm10_mv01,3)+ns(o3_mv01,3)+
                   ns(dayofyear,20)*as.factor(year)+as.factor(weekday),
                 data=a_dat,family=poisson)

plot.para <- termplot(gen.model, se = TRUE, plot = FALSE)

par(mfrow=c(2,2))
plot(plot.para$t_mean$x,plot.para$t_mean$y,type="n",ylim=c(-0.3,0.3))
lines(plot.para$t_mean$x,plot.para$t_mean$y,col="red")
lines(plot.para$t_mean$x,plot.para$t_mean$y-1.96*plot.para$t_mean$se,col="grey")
lines(plot.para$t_mean$x,plot.para$t_mean$y+1.96*plot.para$t_mean$se,col="grey")

plot(plot.para$t_mean_lag_1$x,plot.para$t_mean_lag_1$y,type="n",ylim=c(-0.3,0.3))
lines(plot.para$t_mean_lag_1$x,plot.para$t_mean_lag_1$y,col="red")
lines(plot.para$t_mean_lag_1$x,plot.para$t_mean_lag_1$y-1.96*plot.para$t_mean_lag_1$se,col="grey")
lines(plot.para$t_mean_lag_1$x,plot.para$t_mean_lag_1$y+1.96*plot.para$t_mean_lag_1$se,col="grey")

plot(plot.para$t_mean_lag_2$x,plot.para$t_mean_lag_2$y,type="n",ylim=c(-0.3,0.3))
lines(plot.para$t_mean_lag_2$x,plot.para$t_mean_lag_2$y,col="red")
lines(plot.para$t_mean_lag_2$x,plot.para$t_mean_lag_2$y-1.96*plot.para$t_mean_lag_2$se,col="grey")
lines(plot.para$t_mean_lag_2$x,plot.para$t_mean_lag_2$y+1.96*plot.para$t_mean_lag_2$se,col="grey")

plot(plot.para$t_mean_lag_3$x,plot.para$t_mean_lag_3$y,type="n",ylim=c(-0.3,0.3))
lines(plot.para$t_mean_lag_3$x,plot.para$t_mean_lag_3$y,col="red")
lines(plot.para$t_mean_lag_3$x,plot.para$t_mean_lag_3$y-1.96*plot.para$t_mean_lag_3$se,col="grey")
lines(plot.para$t_mean_lag_3$x,plot.para$t_mean_lag_3$y+1.96*plot.para$t_mean_lag_3$se,col="grey")




###Preparation
###Create lag variables before creating a daily binary indicator to the presense of a heatwave
newdata<-createlag(data=seouldat,vari=t_mean,maxlag=2,cutoff=quantile(seouldat$t_mean,0.73),piecewise=TRUE)

###Create the binary indicator (Heatwave definition: the 99th percentile temperature as the intensity thresshold, at least two consecutive days as the duration threshold)
newdata$HW<-createHW(newdata,t_mean,hw_th=quantile(seouldat$t_mean,0.99),hw_dr=2)

###Create variables to adjust for temperature
V<-createV(data=newdata,vari=t_mean,maxlag=1,hw_th=quantile(seouldat$t_mean,0.99),hw_dr=2)
newdata<-cbind(newdata,V)
HT_ADJ1<-createADJforHT(data=newdata,vari=t_mean,HWvari=HW,maxlag=2,adj=1)
HT_ADJ2<-createADJforHT(data=newdata,vari=t_mean,HWvari=HW,maxlag=2,adj=2)
ADJ1.NS<-createADJforHT(data=newdata,vari=t_mean,cutoff=quantile(seouldat$t_mean,0.73),HWvari=HW,maxlag=2,adj=1,piecewise=FALSE)
ADJ2.NS<-createADJforHT(data=newdata,vari=t_mean,cutoff=quantile(seouldat$t_mean,0.73),HWvari=HW,maxlag=2,adj=2,piecewise=FALSE)
newdata<-cbind(newdata,HT_ADJ1)
newdata<-cbind(newdata,HT_ADJ2)
newdata<-cbind(newdata,ADJ1.NS)
newdata<-cbind(newdata,ADJ2.NS)


###Create variabels to estimate lag effects of heatwaves separately.
FHW<-createFHW(data=newdata,vari=t_mean,HWvari=HW,maxlag=1,hw_th=quantile(seouldat$t_mean,0.99),hw_dr=2)
newdata<-cbind(newdata,FHW)

newdata$HT_ADJ3_lag.1<-newdata$HT_ADJ2_lag.1-newdata$HT_lag.1_V
newdata$ADJ3_lag.1<-newdata$ADJ2_lag.1-newdata$HT_lag.1_V
library(tsModel)
newdata$o3_mv01<-runMean(newdata$o3,0:1)
newdata$pm10_mv01<-runMean(newdata$pm10,0:1)



cutoff<-quantile(seouldat$t_mean,0.73)
newdata$t_mean_Lower<-ifelse(newdata$t_mean<cutoff,newdata$t_mean-cutoff,0)
newdata$t_mean_lag_1_Lower<-ifelse(newdata$t_mean_lag_1<cutoff,newdata$t_mean_lag_1-cutoff,0)
newdata$t_mean_lag_2_Lower<-ifelse(newdata$t_mean_lag_2<cutoff,newdata$t_mean_lag_2-cutoff,0)

newdata$t_mean_Higher<-ifelse(newdata$t_mean>=cutoff,newdata$t_mean-cutoff,0)
newdata$t_mean_lag_1_Higher<-ifelse(newdata$t_mean_lag_1>=cutoff,newdata$t_mean_lag_1-cutoff,0)
newdata$t_mean_lag_2_Higher<-ifelse(newdata$t_mean_lag_2>=cutoff,newdata$t_mean_lag_2-cutoff,0)
newdata$t_mean_lagm1_Higher<-ifelse(newdata$t_mean_lagm1>=cutoff,newdata$t_mean_lagm1-cutoff,0)



###Only summer data
sim.dat<-subset(newdata,month %in% (6:8))


nsim<-100 ## 10,000 was used for the online supplementary materials.
###RESULT Table
NewMethod<-matrix(NA,nrow=nsim,ncol=12)
TAMethod<-matrix(NA,nrow=nsim,ncol=6)

####SIMULATION
set.seed(123)

for (ii in seq(nsim)) {
  Y_exp<-rpois(length(predict(gen.model,type="response")),lambda=predict(gen.model.2,type="response"))
  sim.dat$Y<-Y_exp
  
  
  library(splines)
  ###Model 3. This should provide the result presented in Table 1 (All deaths)
  fit.nolag<-glm(Y_exp~HW+t_mean_LT+t_mean_LT_lag.1+HT_ADJ1_lag.0+HT_ADJ1_lag.1+HT_lag.1_V+
                   ns(rh_mean,4)+ns(pm10_mv01,3)+ns(o3_mv01,3)+
                   ns(dayofyear,20)*as.factor(year)+as.factor(weekday),
                 data=sim.dat,family=quasipoisson)
  
  ###Model 3 + Lag. This should provide the result presented in Table 1 (All deaths)
  fit.withlag1<-glm(Y_exp~HW+FHW_1+t_mean_LT+t_mean_LT_lag.1+HT_ADJ2_lag.0+HT_ADJ3_lag.1+HT_lag.1_V+
                      ns(rh_mean,4)+ns(pm10_mv01,3)+ns(o3_mv01,3)+
                      ns(dayofyear,20)*as.factor(year)+as.factor(weekday),
                    data=sim.dat,family=quasipoisson)
  
  
  
  ###Model 3. This should provide the result presented in Table 1 (All deaths)
  fit.nolag.ns<-glm(Y_exp~HW+ns(ADJ1_lag.0,5)+ns(ADJ1_lag.1,5)+HT_lag.1_V+
                      ns(rh_mean,4)+ns(pm10_mv01,3)+ns(o3_mv01,3)+
                      ns(dayofyear,20)*as.factor(year)+as.factor(weekday),
                    data=sim.dat,family=quasipoisson)
  
  ###Model 3 + Lag. This should provide the result presented in Table 1 (All deaths)
  fit.withlag1.ns<-glm(Y_exp~HW+FHW_1+ns(ADJ2_lag.0,5)+ns(ADJ3_lag.1,5)+HT_lag.1_V+
                         ns(rh_mean,4)+ns(pm10_mv01,3)+ns(o3_mv01,3)+
                         ns(dayofyear,20)*as.factor(year)+as.factor(weekday),
                       data=sim.dat,family=quasipoisson)
  
  
  
  NewMethod[ii,1:3]<-(c(coef(fit.nolag)[2],
                        coef(fit.nolag)[2]-1.96*sqrt(vcov(fit.nolag)[2,2]),
                        coef(fit.nolag)[2]+1.96*sqrt(vcov(fit.nolag)[2,2])))
  
  NewMethod[ii,4:6]<-c(sum(coef(fit.withlag1)[2:3]),
                       sum(coef(fit.withlag1)[2:3])-1.96*sqrt(sum(vcov(fit.withlag1)[2:3,2:3])),
                       sum(coef(fit.withlag1)[2:3])+1.96*sqrt(sum(vcov(fit.withlag1)[2:3,2:3])))
  
  
  NewMethod[ii,7:9]<-(c(coef(fit.nolag.ns)[2],
                        coef(fit.nolag.ns)[2]-1.96*sqrt(vcov(fit.nolag.ns)[2,2]),
                        coef(fit.nolag.ns)[2]+1.96*sqrt(vcov(fit.nolag.ns)[2,2])))
  
  NewMethod[ii,10:12]<-c(sum(coef(fit.withlag1.ns)[2:3]),
                         sum(coef(fit.withlag1.ns)[2:3])-1.96*sqrt(sum(vcov(fit.withlag1.ns)[2:3,2:3])),
                         sum(coef(fit.withlag1.ns)[2:3])+1.96*sqrt(sum(vcov(fit.withlag1.ns)[2:3,2:3])))
  
  
  
  ###Model 1
  TA1.fit<-glm(Y_exp~HW+
                 ns(rh_mean,4)+ns(pm10_mv01,3)+ns(o3_mv01,3)+
                 ns(dayofyear,20)*as.factor(year)+as.factor(weekday),
               data=sim.dat,family=quasipoisson)
  
  ###Model 2  
  TA2.fit<-glm(Y_exp~HW+t_mean_Lower+t_mean_Higher+t_mean_lag_1_Lower+t_mean_lag_1_Higher+
                 ns(rh_mean,4)+ns(pm10_mv01,3)+ns(o3_mv01,3)+
                 ns(dayofyear,20)*as.factor(year)+as.factor(weekday),
               data=sim.dat,family=quasipoisson)
  
  TAMethod[ii,1:3]<-(c(coef(TA1.fit)[2],
                       coef(TA1.fit)[2]-1.96*sqrt(vcov(TA1.fit)[2,2]),
                       coef(TA1.fit)[2]+1.96*sqrt(vcov(TA1.fit)[2,2])))
  
  TAMethod[ii,4:6]<-(c(coef(TA2.fit)[2],
                       coef(TA2.fit)[2]-1.96*sqrt(vcov(TA2.fit)[2,2]),
                       coef(TA2.fit)[2]+1.96*sqrt(vcov(TA2.fit)[2,2])))
  
  print(ii)
}

dev.off()

par(mgp=c(2,0.7,0),mar=c(3.5,3.5,2,0.5))
plot(NA,NA,type="n",xaxt="n",yaxt="n",xlim=c(0.5,6.5),ylim=c(-10,60), xlab="",ylab="Percent increase",bty="l")
abline(h=seq(-10,60,by=10),col="grey",lty=2)
abline(h=0,col="grey")
arrows(3,(exp(mean(NewMethod[,2]))-1)*100,3,(exp(mean(NewMethod[,3]))-1)*100,code=0)
points(3,(exp(mean(NewMethod[,1]))-1)*100,pch=16)
arrows(4,(exp(mean(NewMethod[,5]))-1)*100,4,(exp(mean(NewMethod[,6]))-1)*100,code=0)
points(4,(exp(mean(NewMethod[,4]))-1)*100,pch=16)
arrows(5,(exp(mean(NewMethod[,8]))-1)*100,5,(exp(mean(NewMethod[,9]))-1)*100,code=0)
points(5,(exp(mean(NewMethod[,7]))-1)*100,pch=16)
arrows(6,(exp(mean(NewMethod[,11]))-1)*100,6,(exp(mean(NewMethod[,12]))-1)*100,code=0)
points(6,(exp(mean(NewMethod[,10]))-1)*100,pch=16)
arrows(1,(exp(mean(TAMethod[,2]))-1)*100,1,(exp(mean(TAMethod[,3]))-1)*100,code=0)
points(1,(exp(mean(TAMethod[,1]))-1)*100,pch=16)
arrows(2,(exp(mean(TAMethod[,5]))-1)*100,2,(exp(mean(TAMethod[,6]))-1)*100,code=0)
points(2,(exp(mean(TAMethod[,4]))-1)*100,pch=16)
axis(2,at=seq(-10,60,by=10),seq(-10,60,by=10))
axis(1,at=c(1,2,3,4,5,6),c("Traditional\n Approach #1","Traditional\n Approach #2","Novel Approach","Novel Approach with FHW","Novel Approach\n(spline adjustment)","Novel Approach with FHW\n(spline adjustment)"),padj=1)




