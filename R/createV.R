#' createV
#'
#' This function create vector V to adjust for temperature
#' @param data dataset to be used.
#' @param vari the name of temperature variable to be used in the dataset (e.g., t_mean; not "t_mean").
#' @param maxlag the maximum lag period.
#' @param hw_th the intensity threshold for the definition of a heatwave (e.g., temperature exceeding the 99th percentile of temperature distribution).
#' @param hw_dr the duration threshold for the definition of a heatwave (e.g., at least two consecutive days). 
#' @export
#' @examples
#' createV(data=data,vari=t_mean,maxlag=1,hw_th=quantile(data$t_mean,0.99),hw_dr=2)

createV<-function(data,vari,maxlag,hw_th,hw_dr) {
  vari<-deparse(substitute(vari))
  runs<-rle(data[,vari] > hw_th)
  HT_lag.1_V<-data[,paste0(vari,"_HT_lag.1")]
  day1st<-(cumsum(runs$lengths) - runs$lengths + 1)[runs$values & runs$lengths >= hw_dr]
  HT_lag.1_V[seq(nrow(data))[-day1st]]<-0
  
  if(maxlag>=2) {
    HT_lag.2_V<-data[,paste0(vari,"_HT_lag.2")]
    day2nd<-(cumsum(runs$lengths) - runs$lengths + 1)[runs$values & runs$lengths >= hw_dr]+1
    HT_lag.2_V[seq(nrow(data))[-c(day1st,day2nd)]]<-0
  }
  if(maxlag>=3) {
    HT_lag.3_V<-data[,paste0(vari,"_HT_lag.3")]
    day3rd<-(cumsum(runs$lengths) - runs$lengths + 1)[runs$values & runs$lengths >= hw_dr]+2
    HT_lag.3_V[seq(nrow(data))[-c(day1st,day2nd,day3rd)]]<-0
  }
  if(maxlag>=4) {
    HT_lag.4_V<-data[,paste0(vari,"_HT_lag.4")]
    day4th<-(cumsum(runs$lengths) - runs$lengths + 1)[runs$values & runs$lengths >= hw_dr]+3
    HT_lag.4_V[seq(nrow(data))[-c(day1st,day2nd,day3rd,day4th)]]<-0
  }
  if(maxlag>=5) {
    HT_lag.5_V<-data[,paste0(vari,"_HT_lag.5")]
    day5th<-(cumsum(runs$lengths) - runs$lengths + 1)[runs$values & runs$lengths >= hw_dr]+4
    HT_lag.5_V[seq(nrow(data))[-c(day1st,day2nd,day3rd,day4th,day5th)]]<-0
  }
  if(maxlag>=6) {
    HT_lag.6_V<-data[,paste0(vari,"_HT_lag.6")]
    day6th<-(cumsum(runs$lengths) - runs$lengths + 1)[runs$values & runs$lengths >= hw_dr]+5
    HT_lag.6_V[seq(nrow(data))[-c(day1st,day2nd,day3rd,day4th,day5th,day6th)]]<-0
  }
  if(maxlag==7) {
    HT_lag.7_V<-data[,paste0(vari,"_HT_lag.7")]
    day7th<-(cumsum(runs$lengths) - runs$lengths + 1)[runs$values & runs$lengths >= hw_dr]+6
    HT_lag.7_V[seq(nrow(data))[-c(day1st,day2nd,day3rd,day4th,day5th,day6th,day7th)]]<-0
  }
  
  if(maxlag==1) {
    return(data.frame(HT_lag.1_V=HT_lag.1_V))
  }
  if(maxlag==2) {
    return(data.frame(HT_lag.1_V=HT_lag.1_V,HT_lag.2_V=HT_lag.2_V))
  }
  if(maxlag==3) {
    return(data.frame(HT_lag.1_V=HT_lag.1_V,HT_lag.2_V=HT_lag.2_V,HT_lag.3_V=HT_lag.3_V))
  }  
  if(maxlag==4) {
    return(data.frame(HT_lag.1_V=HT_lag.1_V,HT_lag.2_V=HT_lag.2_V,HT_lag.3_V=HT_lag.3_V,HT_lag.4_V=HT_lag.4_V))
  } 
  if(maxlag==5) {
    return(data.frame(HT_lag.1_V=HT_lag.1_V,HT_lag.2_V=HT_lag.2_V,HT_lag.3_V=HT_lag.3_V,HT_lag.4_V=HT_lag.4_V,HT_lag.5_V=HT_lag.5_V))
  }  
  if(maxlag==6) {
    return(data.frame(HT_lag.1_V=HT_lag.1_V,HT_lag.2_V=HT_lag.2_V,HT_lag.3_V=HT_lag.3_V,HT_lag.4_V=HT_lag.4_V,HT_lag.5_V=HT_lag.5_V,HT_lag.6_V=HT_lag.6_V))
  }  
  if(maxlag==7) {
    return(data.frame(HT_lag.1_V=HT_lag.1_V,HT_lag.2_V=HT_lag.2_V,HT_lag.3_V=HT_lag.3_V,HT_lag.4_V=HT_lag.4_V,HT_lag.5_V=HT_lag.5_V,HT_lag.6_V=HT_lag.6_V,HT_lag.7_V=HT_lag.7_V))
  } 
}


