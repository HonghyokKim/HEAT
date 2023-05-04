#' createlag
#'
#' This function create lag variables 
#' @param data dataset to be used.
#' @param vari the name of temperature variable to be used in the dataset (e.g., t_mean; not "t_mean").
#' @param maxlag the maximum lag period
#' @param cutoff the value to distinguish low temperatures and high temperatures, like optimal temperature. Default is NULL. Only to be used if piecewise=T  
#' @param piecewise create piecewise variables for low temperatures and high temperatures. Default is FALSE
#' @export
#' @examples
#' createlag(data=data,vari=t_mean,maxlag=5,cutoff=quantile(data$t_mean,0.70),piecewise=TRUE)

createlag<-function(data,vari,maxlag,cutoff=NULL,piecewise=FALSE) {
  if(maxlag<=1) {
    print("maxlag needs to be higher than or equal to 2") 
  }
  else {
    vari<-deparse(substitute(vari))
    data[,paste0(vari,"_lagm1")]<-tsModel::Lag(data[,vari],k=-1)
    data[,paste0(vari,"_lagm2")]<-tsModel::Lag(data[,vari],k=-2)
    data[,paste0(vari,"_lagm3")]<-tsModel::Lag(data[,vari],k=-3)
    data[,paste0(vari,"_lagm4")]<-tsModel::Lag(data[,vari],k=-4)
    data[,paste0(vari,"_lag")]<-tsModel::Lag(data[,vari],k=1:maxlag)
    
    if(piecewise==TRUE) {
    data[,paste0(vari,"_LT")]<-ifelse(data[,vari]<cutoff,data[,vari]-cutoff,0)
    data[,paste0(vari,"_LT_lag.",seq(1:maxlag))]<-ifelse(data[,paste0(vari,"_lag")]<cutoff,data[,paste0(vari,"_lag")]-cutoff,0)
    data[,paste0(vari,"_HT")]<-ifelse(data[,vari]>=cutoff,data[,vari]-cutoff,0)
    data[,paste0(vari,"_HT_lag.",seq(1:maxlag))]<-ifelse(data[,paste0(vari,"_lag")]>=cutoff,data[,paste0(vari,"_lag")]-cutoff,0)
    data[,paste0(vari,"_HT_lagm1")]<-ifelse(data[,paste0(vari,"_lagm1")]>=cutoff,data[,paste0(vari,"_lagm1")]-cutoff,0)
    data[,paste0(vari,"_HT_lagm2")]<-ifelse(data[,paste0(vari,"_lagm2")]>=cutoff,data[,paste0(vari,"_lagm2")]-cutoff,0)
    data[,paste0(vari,"_HT_lagm3")]<-ifelse(data[,paste0(vari,"_lagm3")]>=cutoff,data[,paste0(vari,"_lagm3")]-cutoff,0)
    data[,paste0(vari,"_HT_lagm4")]<-ifelse(data[,paste0(vari,"_lagm4")]>=cutoff,data[,paste0(vari,"_lagm4")]-cutoff,0)
    }
    
    return(data)
  }
}
