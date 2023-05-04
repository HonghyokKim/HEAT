#' createFHW
#'
#' This function create lag variables of a binary heatwave indicator variable.
#' @param data dataset to be used.
#' @param vari the name of temperature variable to be used in the dataset (e.g., t_mean; not "t_mean").
#' @param HWvari the name of the binary variable to indicate the presence of a heatwave (e.g., HW; not "HW")
#' @param hw_th the intensity threshold for the definition of a heatwave (e.g., temperature exceeding the 99th percentile of temperature distribution)
#' @param hw_dr the duration threshold for the definition of a heatwave (e.g., at least two consecutive days).  
#' @param LEHRNH Lag Effects on the Health Risk on the Next Heatwave (LEHRNH). If TRUE, the lag effects of the previous heatwave on the health risk on the next heatwave (if they exist) are subsumed into the effect of the lag0 heatwave indicator variable. Default is FALSE
#' @export
#' @examples
#' createFHW(data=data,vari=t_mean,HWvari=HW,maxlag=1,hw_th=quantile(data$t_mean,0.99),hw_dr=2,LEHRNH=FALSE)

createFHW<-function(data,vari,HWvari,maxlag,hw_th,hw_dr,LEHRNH=FALSE) {
  vari<-deparse(substitute(vari))
  runs<-rle(data[,vari] > hw_th)
  afterHW_1st<-(cumsum(runs$lengths) + 1)[runs$values & runs$lengths >= hw_dr]
  afterHW_2nd<-(cumsum(runs$lengths) + 1)[runs$values & runs$lengths >= hw_dr]+1
  afterHW_3rd<-(cumsum(runs$lengths) + 1)[runs$values & runs$lengths >= hw_dr]+2
  afterHW_4th<-(cumsum(runs$lengths) + 1)[runs$values & runs$lengths >= hw_dr]+3
  afterHW_5th<-(cumsum(runs$lengths) + 1)[runs$values & runs$lengths >= hw_dr]+4
  afterHW_6th<-(cumsum(runs$lengths) + 1)[runs$values & runs$lengths >= hw_dr]+5
  afterHW_7th<-(cumsum(runs$lengths) + 1)[runs$values & runs$lengths >= hw_dr]+6
  
  FHW_1<-FHW_2<-FHW_3<-FHW_4<-FHW_5<-FHW_6<-FHW_7<-rep(0,nrow(data))
  FHW_1[afterHW_1st]<-1
  FHW_2[afterHW_2nd]<-1
  FHW_3[afterHW_3rd]<-1
  FHW_4[afterHW_4th]<-1
  FHW_5[afterHW_5th]<-1
  FHW_6[afterHW_6th]<-1
  FHW_7[afterHW_7th]<-1
  
  if(LEHRNH==TRUE) {
    HWvari<-deparse(substitute(HWvari))
    FHW_1<-ifelse(data[,HWvari]==1,0,FHW_1)
    FHW_2<-ifelse(data[,HWvari]==1,0,FHW_2)
    FHW_3<-ifelse(data[,HWvari]==1,0,FHW_3)
    FHW_4<-ifelse(data[,HWvari]==1,0,FHW_4)
    FHW_5<-ifelse(data[,HWvari]==1,0,FHW_5)
    FHW_6<-ifelse(data[,HWvari]==1,0,FHW_6)
    FHW_7<-ifelse(data[,HWvari]==1,0,FHW_7)
  }
  
  if(maxlag==1) {
    return(data.frame(FHW_1=FHW_1))
  }
  if(maxlag==2) {
    return(data.frame(FHW_1=FHW_1,FHW_2=FHW_2))
  }
  if(maxlag==3) {
    return(data.frame(FHW_1=FHW_1,FHW_2=FHW_2,FHW_3=FHW_3))
  }
  if(maxlag==4) {
    return(data.frame(FHW_1=FHW_1,FHW_2=FHW_2,FHW_3=FHW_3,FHW_4=FHW_4))
  }
  if(maxlag==5) {
    return(data.frame(FHW_1=FHW_1,FHW_2=FHW_2,FHW_3=FHW_3,FHW_4=FHW_4,FHW_5=FHW_5))
  }
  if(maxlag==6) {
    return(data.frame(FHW_1=FHW_1,FHW_2=FHW_2,FHW_3=FHW_3,FHW_4=FHW_4,FHW_5=FHW_5,FHW_6=FHW_6))
  }
  if(maxlag==7) {
    return(data.frame(FHW_1=FHW_1,FHW_2=FHW_2,FHW_3=FHW_3,FHW_4=FHW_4,FHW_5=FHW_5,FHW_6=FHW_6,FHW_7=FHW_7))
  }
}
