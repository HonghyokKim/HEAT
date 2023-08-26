#' createADJforHT
#'
#' This function creates variables for adjustment for confounding by temperature.
#' @param data dataset to be used.
#' @param vari the name of temperature variable to be used in the dataset (e.g., t_mean; not "t_mean").
#' @param HWvari the name of the binary variable to indicate the presence of a heatwave (e.g., HW; not "HW")
#' @param maxlag the maximum lag period to adjust for temperature
#' @param cutoff the value to distinguish low temperatures and high temperatures, like optimal temperature. Default is NULL. Only to be used if piecewise=FALSE
#' @param adj if=1 (default), variables for adjustment for temperature including lag effects of heatwaves. if=2, variables for adjustment for temperature excluding lag effects of heatwaves.
#' @param piecewise if TRUE (default), piecewise temperature variables for adjustment; if FALSE, non-piecewise temperature variables for adjustment
#' @export
#' @examples
#' createADJforHT(data=newdata,vari=t_mean,HWvari=HW,maxlag=2,adj=1)

createADJforHT<-function(data,vari,HWvari,maxlag,cutoff=NULL,adj=1,piecewise=TRUE) {
  vari<-deparse(substitute(vari))
  HWvari<-deparse(substitute(HWvari))
  
  if(piecewise==TRUE) {
  if(adj==2) {
    result_ADJ2<-data.frame(HT_ADJ2_lag.0=ifelse(data[,HWvari]==1,0,data[,paste0(vari,"_HT")]))
    
    if(maxlag==1){
      result_ADJ2$HT_ADJ2_lag.1<-tsModel::Lag(result_ADJ2$HT_ADJ2_lag.0,k=1)
    }
    if(maxlag>=2){
      result_ADJ2<-data.frame(HT_ADJ2_lag.0=ifelse(data[,HWvari]==1,0,data[,paste0(vari,"_HT")]),
                              HT_ADJ2_lag=tsModel::Lag(result_ADJ2$HT_ADJ2_lag.0,k=c(1:maxlag)))
    }
    return(result_ADJ2)
  }
  
  if(adj==1) {
    result_ADJ1<-data.frame(HT_ADJ1_lag.0=ifelse(data[,HWvari]==1,0,data[,paste0(vari,"_HT")]))
    
    if(maxlag>=1) {
      result_ADJ1$HT_ADJ1_lag.1<-ifelse(data[,HWvari]==1,0,data[,paste0(vari,"_HT_lag.1")])
    }
    if(maxlag>=2) {
      result_ADJ1$HT_ADJ1_lag.2<-ifelse(data[,HWvari]==1,0,data[,paste0(vari,"_HT_lag.2")])
    }
    if(maxlag>=3) {
      result_ADJ1$HT_ADJ1_lag.3<-ifelse(data[,HWvari]==1,0,data[,paste0(vari,"_HT_lag.3")])
    }
    if(maxlag>=4) {
      result_ADJ1$HT_ADJ1_lag.4<-ifelse(data[,HWvari]==1,0,data[,paste0(vari,"_HT_lag.4")])
    }
    if(maxlag>=5) {
      result_ADJ1$HT_ADJ1_lag.5<-ifelse(data[,HWvari]==1,0,data[,paste0(vari,"_HT_lag.5")])
    }
    if(maxlag>=6) {
      result_ADJ1$HT_ADJ1_lag.6<-ifelse(data[,HWvari]==1,0,data[,paste0(vari,"_HT_lag.6")])
    }
    if(maxlag>=7) {
      result_ADJ1$HT_ADJ1_lag.7<-ifelse(data[,HWvari]==1,0,data[,paste0(vari,"_HT_lag.7")])
    }
    return(result_ADJ1)
  }
  }

  if(piecewise==FALSE) {
    if(adj==2) {
      result_ADJ2<-data.frame(ADJ2_lag.0=ifelse(data[,HWvari]==1,0,data[,paste0(vari)]-cutoff))
      
      if(maxlag==1){
        result_ADJ2$ADJ2_lag.1<-tsModel::Lag(result_ADJ2$ADJ2_lag.0,k=1)
      }
      if(maxlag>=2){
        result_ADJ2<-data.frame(ADJ2_lag.0=ifelse(data[,HWvari]==1,0,data[,paste0(vari)]-cutoff),
                                ADJ2_lag=tsModel::Lag(result_ADJ2$ADJ2_lag.0,k=c(1:maxlag)))
      }
      return(result_ADJ2)
    }
    
    if(adj==1) {
      result_ADJ1<-data.frame(ADJ1_lag.0=ifelse(data[,HWvari]==1,0,data[,paste0(vari)]-cutoff))
      
      if(maxlag>=1) {
        result_ADJ1$ADJ1_lag.1<-ifelse(data[,HWvari]==1,0,data[,paste0(vari,"_lag")][,1]-cutoff)
      }
      if(maxlag>=2) {
        result_ADJ1$ADJ1_lag.2<-ifelse(data[,HWvari]==1,0,data[,paste0(vari,"_lag")][,2]-cutoff)
      }
      if(maxlag>=3) {
        result_ADJ1$ADJ1_lag.3<-ifelse(data[,HWvari]==1,0,data[,paste0(vari,"_lag")][,3]-cutoff)
      }
      if(maxlag>=4) {
        result_ADJ1$ADJ1_lag.4<-ifelse(data[,HWvari]==1,0,data[,paste0(vari,"_lag")][,4]-cutoff)
      }
      if(maxlag>=5) {
        result_ADJ1$ADJ1_lag.5<-ifelse(data[,HWvari]==1,0,data[,paste0(vari,"_lag")][,5]-cutoff)
      }
      if(maxlag>=6) {
        result_ADJ1$ADJ1_lag.6<-ifelse(data[,HWvari]==1,0,data[,paste0(vari,"_lag")][,6]-cutoff)
      }
      if(maxlag>=7) {
        result_ADJ1$ADJ1_lag.7<-ifelse(data[,HWvari]==1,0,data[,paste0(vari,"_lag")][,7]-cutoff)
      }
      return(result_ADJ1)
    }
  }
}

