
#devtools::install_github('HonghyokKim/HEAT')
library(HEAT)
data("seouldat")

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


library(tsModel)
newdata$o3_mv01<-runMean(newdata$o3,0:1)
newdata$pm10_mv01<-runMean(newdata$pm10,0:1)

###Only summer data
a_dat<-subset(newdata,month %in% (6:8))



library(splines)

###Model 3. This should provide the result presented in the manuscript
fit.nolag<-glm(na_death~HW+t_mean_LT+t_mean_LT_lag.1+HT_ADJ1_lag.0+HT_ADJ1_lag.1+HT_lag.1_V+
                ns(rh_mean,4)+ns(pm10_mv01,3)+ns(o3_mv01,3)+
                ns(dayofyear,20)*as.factor(year)+as.factor(weekday),
              data=a_dat,family=quasipoisson)


round(
  (exp(c(coef(fit.nolag)[2],
         coef(fit.nolag)[2]-1.96*sqrt(vcov(fit.nolag)[2,2]),
         coef(fit.nolag)[2]+1.96*sqrt(vcov(fit.nolag)[2,2])))-1)*100,1)


a_dat$HT_ADJ3_lag.1<-a_dat$HT_ADJ2_lag.1-a_dat$HT_lag.1_V
###Model 3 + Lag. This should provide the result presented in the manuscript
fit.withlag1<-glm(na_death~HW+FHW_1+t_mean_LT+t_mean_LT_lag.1+HT_ADJ2_lag.0+HT_ADJ3_lag.1+HT_lag.1_V+
                 ns(rh_mean,4)+ns(pm10_mv01,3)+ns(o3_mv01,3)+
                 ns(dayofyear,20)*as.factor(year)+as.factor(weekday),
               data=a_dat,family=quasipoisson)

round(
  (exp(c(sum(coef(fit.withlag1)[2:3]),
         sum(coef(fit.withlag1)[2:3])-1.96*sqrt(sum(vcov(fit.withlag1)[2:3,2:3])),
         sum(coef(fit.withlag1)[2:3])+1.96*sqrt(sum(vcov(fit.withlag1)[2:3,2:3]))))-1)*100,1)




###Model 3 with natural cubic spline adjustment for T. This should provide the result presented in the manuscript
fit.nolag.ns<-glm(na_death~HW+ns(ADJ1_lag.0,5)+ns(ADJ1_lag.1,5)+HT_lag.1_V+
                 ns(rh_mean,4)+ns(pm10_mv01,3)+ns(o3_mv01,3)+
                 ns(dayofyear,20)*as.factor(year)+as.factor(weekday),
               data=a_dat,family=quasipoisson)

round(
  (exp(c(coef(fit.nolag.ns)[2],
         coef(fit.nolag.ns)[2]-1.96*sqrt(vcov(fit.nolag.ns)[2,2]),
         coef(fit.nolag.ns)[2]+1.96*sqrt(vcov(fit.nolag.ns)[2,2])))-1)*100,1)

a_dat$ADJ3_lag.1<-a_dat$ADJ2_lag.1-a_dat$HT_lag.1_V
###Model 3 + Lag  with natural cubic spline adjustment for T. This should provide the result presented in the manuscript
fit.withlag1.ns<-glm(na_death~HW+FHW_1+ns(ADJ2_lag.0,3)+ns(ADJ3_lag.1,3)+HT_lag.1_V+
                    ns(rh_mean,4)+ns(pm10_mv01,3)+ns(o3_mv01,3)+
                    ns(dayofyear,20)*as.factor(year)+as.factor(weekday),
                  data=a_dat,family=quasipoisson)

round(
  (exp(c(sum(coef(fit.withlag1.ns)[2:3]),
         sum(coef(fit.withlag1.ns)[2:3])-1.96*sqrt(sum(vcov(fit.withlag1.ns)[2:3,2:3])),
         sum(coef(fit.withlag1.ns)[2:3])+1.96*sqrt(sum(vcov(fit.withlag1.ns)[2:3,2:3]))))-1)*100,1)

