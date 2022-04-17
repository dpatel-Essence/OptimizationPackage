#' Compute Fees
#'
#' A set of functions that 
#' @param mv_spend,fees spend and fees
#' 
#' 
#' @keywords compute fees
#' @export
#' @examples
#' compute_fees() among others


#####Section 1: Get fees from MV number
compute_fees <- function(mv_spend,fees){
  if(mv_spend==0){return(0)}
  else{
    index_full <- which(mv_spend-fees[,"Upper Bound"]>=0)
    index_inc <- which(mv_spend-fees[,"Upper Bound"]<0)[1]
    fees_full <- ((fees[,"Upper Bound"]-fees[,"Lower Bound"])*fees[,"Fee %"])[index_full]
    fees_inc <- (mv_spend-fees[index_inc,"Lower Bound"])*fees[index_inc,"Fee %"]
    return(sum(fees_full,fees_inc))
  }
}

#####Section 2: Incremental spend in MV, CV, CV + Fee to incremental MV, CV, and CV + Fee
####Incremental MV
mv_inc_fun <- function(mv_curr,inc,cv_perc,fees){
  mv_inc <- inc
  cv_inc <- mv_inc*cv_perc
  if(which(fees[,"Upper Bound"]-(mv_curr+mv_inc)>=0)[1]==which(fees[,"Upper Bound"]-(mv_curr)>=0)[1]){
    fee_inc <- mv_inc*fees[which(fees[,"Upper Bound"]-(mv_curr+mv_inc)>=0)[1],"Fee %"]
  }
  else{
    fee_inc <- compute_fees(mv_spend=mv_curr+mv_inc,fees=fees)-compute_fees(mv_spend=mv_curr,fees=fees)
  }
  
  return(c(mv_inc,cv_inc,cv_inc+fee_inc))
}

####Incremental CV
cv_inc_fun <- function(mv_curr,inc,cv_perc,fees){
  mv_inc <- inc/cv_perc
  cv_inc <- inc
  if(which(fees[,"Upper Bound"]-(mv_curr+mv_inc)>=0)[1]==which(fees[,"Upper Bound"]-(mv_curr)>=0)[1]){
    fee_inc <- mv_inc*fees[which(fees[,"Upper Bound"]-(mv_curr+mv_inc)>=0)[1],"Fee %"]
  }
  else{
    fee_inc <- compute_fees(mv_spend=mv_curr+mv_inc,fees=fees)-compute_fees(mv_spend=mv_curr,fees=fees)
  }
  
  return(c(mv_inc,cv_inc,cv_inc+fee_inc))
}

####Incremental CV + Fees
####Helper
cvfee_inc_diff <- function(par,mv_curr,inc,cv_perc,fees){
  perc <- par
  cv_inc <- inc*perc
  mv_inc <- cv_inc/cv_perc
  fee_inc <- compute_fees(mv_spend=mv_curr+mv_inc,fees=fees)-compute_fees(mv_spend=mv_curr,fees=fees)
  return((inc-(cv_inc+fee_inc))^2)
}

####Actual function
cvfee_inc_fun <- function(mv_curr,inc,cv_perc,fees){
  if(which(fees[,"Upper Bound"]-(mv_curr+inc/cv_perc)>=0)[1]==which(fees[,"Upper Bound"]-(mv_curr)>=0)[1]){
    perc <- cv_perc/(cv_perc+fees[which(fees[,"Upper Bound"]-(mv_curr+inc/cv_perc)>=0)[1],"Fee %"])
  }
  else{
    opt <- optim(par=0.5,fn=cvfee_inc_diff,mv_curr=mv_curr,inc=inc,cv_perc=cv_perc,fees=fees,method="Brent",lower=0,upper=1)
    if(opt$convergence!=0){stop("CV + Fees to MV had a convergence error.")}
    perc <- opt$par
  }
  cv_inc <- inc*perc
  mv_inc <- cv_inc/cv_perc
  
  return(c(mv_inc,cv_inc,inc))
}