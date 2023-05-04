#' createHW
#'
#' This function create a binary variable to indicate the presense of a heatwave
#' @param data dataset to be used.
#' @param vari the name of temperature variable to be used in the dataset (e.g., t_mean; not "t_mean").
#' @param hw_th the intensity threshold for the definition of a heatwave (e.g., temperature exceeding the 99th percentile of temperature distribution)
#' @param hw_dr the duration threshold for the definition of a heatwave (e.g., at least two consecutive days).  
#' @export
#' @examples
#' createHW(data=data,vari=t_mean,hw_th=quantile(data$t_mean,0.99),hw_dr=2)

createHW<-function(data,vari,hw_th,hw_dr) {
  vari<-deparse(substitute(vari))
  if(hw_dr==1) {
    HW<- ifelse(data_seoul$t_mean>hw_th,1,0)
  }
  
  if(hw_dr==2) {
    lagv<-data[,paste0(vari,"_lag")]
    HW<- ifelse(data[,vari]>hw_th & lagv[,1]>hw_th,1,
                ifelse(data[,vari]>hw_th & data[,paste0(vari,"_lagm1")]>hw_th,1,0))
  }
  
  if(hw_dr==3) {
    lagv<-data[,paste0(vari,"_lag")]
    HW<- ifelse(data[,vari]>hw_th & lagv[,1]>hw_th & lagv[,2]>hw_th,1,
                ifelse(data[,vari]>hw_th & data[,paste0(vari,"_lagm1")]>hw_th & lagv[,1]>hw_th,1,
                       ifelse(data[,vari]>hw_th & data[,paste0(vari,"_lagm1")]>hw_th & data[,paste0(vari,"_lagm2")]>hw_th,1,0)))
  }
  
  if(hw_dr==4) {
    lagv<-data[,paste0(vari,"_lag")]
    HW<- ifelse(data[,vari]>hw_th & lagv[,1]>hw_th & lagv[,2]>hw_th & lagv[,3]>hw_th,1,
                ifelse(data[,vari]>hw_th & data[,paste0(vari,"_lagm1")]>hw_th & lagv[,1]>hw_th & lagv[,2]>hw_th,1,
                       ifelse(data[,vari]>hw_th & data[,paste0(vari,"_lagm1")]>hw_th & data[,paste0(vari,"_lagm2")]>hw_th & lagv[,1]>hw_th,1,
                              ifelse(data[,vari]>hw_th & data[,paste0(vari,"_lagm1")]>hw_th & data[,paste0(vari,"_lagm2")]>hw_th & data[,paste0(vari,"_lagm3")]>hw_th,1,0))))
  }
  
  if(hw_dr==5) {
    lagv<-data[,paste0(vari,"_lag")]
    HW<- ifelse(data[,vari]>hw_th & lagv[,1]>hw_th & lagv[,2]>hw_th & lagv[,3]>hw_th & lagv[,4]>hw_th,1,
                ifelse(data[,vari]>hw_th & data[,paste0(vari,"_lagm1")]>hw_th & lagv[,1]>hw_th & lagv[,2]>hw_th & lagv[,3]>hw_th,1,
                       ifelse(data[,vari]>hw_th & data[,paste0(vari,"_lagm1")]>hw_th & data[,paste0(vari,"_lagm2")]>hw_th & lagv[,1]>hw_th & lagv[,2]>hw_th,1,
                              ifelse(data[,vari]>hw_th & data[,paste0(vari,"_lagm1")]>hw_th & data[,paste0(vari,"_lagm2")]>hw_th & data[,paste0(vari,"_lagm3")]>hw_th & lagv[,1]>hw_th,1,
                                     ifelse(data[,vari]>hw_th & data[,paste0(vari,"_lagm1")]>hw_th & data[,paste0(vari,"_lagm2")]>hw_th & data[,paste0(vari,"_lagm3")]>hw_th & data[,paste0(vari,"_lagm4")]>hw_th,1,0)))))
  }
  return(HW)
}

