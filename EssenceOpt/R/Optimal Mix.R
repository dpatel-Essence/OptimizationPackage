#' Optimal Mix
#'
#' This function is used to get optimal mix for other functions
#' @param CheckExamples this has full parameter list
#' 
#' @keywords opt mix
#' @export
#' @examples
#' opt_mix(diag_mat,diag_mat_name,cm_mat,diag_cm_list,fee_list,process_data_fun,cost_per_type=1,num_periods,
#' basis="CV + Fee",gran_mult,B_max=100000000,store_inc=1000,precision=10,
#' out_fun=out_fun_standard,maxim_fun=maxim_fun_standard,out_fun_args=NULL,
#' maxim_fun_args=NULL)


opt_mix <- function(diag_mat,diag_mat_name,cm_mat,diag_cm_list,fee_list,process_data_fun,cost_per_type=1,num_periods,
                    basis="CV + Fee",gran_mult,B_max=100000000,store_inc=1000,precision=10,
                    out_fun=out_fun_standard,maxim_fun=maxim_fun_standard,out_fun_args=NULL,
                    maxim_fun_args=NULL){
  #####Define precision name
  precision_name <- paste(precision,"dollars")
  
  #####Define incremental function
  if(basis=="MV"){inc_fun <- mv_inc_fun}
  else if(basis=="CV"){inc_fun <- cv_inc_fun}
  else{inc_fun <- cvfee_inc_fun}
  
  #####Check B_max & store_inc are relative to the precision
  if(B_max%%store_inc!=0){stop("Storage increment must be a factor of the max budget.")}
  if(B_max%%precision!=0){stop(paste("Max budget must be rounded to the nearest ",precision_name,".",sep=""))}
  if(store_inc%%precision!=0){stop(paste("Storage increment must be rounded to the nearest ",precision_name,".",sep=""))}
  
  #####Get per period increments & max budget
  period_inc <- precision/num_periods
  period_store_inc <- store_inc/num_periods
  period_B_max <- B_max/num_periods
  
  #####Define iterations and storage location
  tot_iter <- round(period_B_max/period_inc)
  store_iter <- round(period_B_max/period_store_inc)
  store_iter_vec <- round(period_store_inc/period_inc)*c(0:store_iter)
  
  #####Set up matrices
  sp_mat <- matrix(0,nrow=store_iter+1,ncol=nrow(diag_mat),dimnames=list(paste("Iteration",0:store_iter),diag_mat[,"Segment"]))
  mv_mat <- matrix(0,nrow=store_iter+1,ncol=nrow(diag_mat),dimnames=list(paste("Iteration",0:store_iter),diag_mat[,"Segment"]))
  out_mat <- matrix(0,nrow=store_iter+1,ncol=nrow(diag_mat),dimnames=list(paste("Iteration",0:store_iter),diag_mat[,"Segment"]))
  out_inc_mat <- matrix(0,nrow=store_iter+1,ncol=nrow(diag_mat),dimnames=list(paste("Iteration",0:store_iter),diag_mat[,"Segment"]))
  maxim_mat <- matrix(0,nrow=store_iter+1,ncol=nrow(diag_mat),dimnames=list(paste("Iteration",0:store_iter),diag_mat[,"Segment"]))
  maxim_inc_mat <- matrix(0,nrow=store_iter+1,ncol=nrow(diag_mat),dimnames=list(paste("Iteration",0:store_iter),diag_mat[,"Segment"]))
  
  #####Add columns in the constraint matrix to hold the current values of the budget, outcome, and cost per variables
  cm_cols <- c("Max Budget Current","Max Cost Per Budget Current","Outcome Current","Max Cost Per Current")
  for(col in cm_cols){
    cm_mat$"New Column" <- rep(0,nrow(cm_mat))
    colnames(cm_mat)[ncol(cm_mat)] <- col
  }
  
  #####ID the constraint matrix column numbers that correspond to the basis
  sp_nums <- rep(0,nrow(cm_mat))
  sp_nums[which(cm_mat[,"Max Budget Value Basis"]=="MV")] <- 1
  sp_nums[which(cm_mat[,"Max Budget Value Basis"]=="CV")] <- 2
  sp_nums[which(cm_mat[,"Max Budget Value Basis"]=="CV + Fee")] <- 3
  
  mcp_nums <- rep(0,nrow(cm_mat))
  mcp_nums[which(cm_mat[,"Max Cost Per Value Basis"]=="MV")] <- 1
  mcp_nums[which(cm_mat[,"Max Cost Per Value Basis"]=="CV")] <- 2
  mcp_nums[which(cm_mat[,"Max Cost Per Value Basis"]=="CV + Fee")] <- 3
  
  #####Define initial values of spend, outcome value, optimization value, and tactic vector
  old_mv <- sp_mat[1,]
  old_out <- do.call(what=out_fun,args=c(list("sp"=old_mv,"diag_mat"=diag_mat),out_fun_args))
  old_maxim <- do.call(what=maxim_fun,args=c(list("sp"=old_mv,"diag_mat"=diag_mat),maxim_fun_args))
  tact_ind <- 1:nrow(diag_mat)
  
  #####Define initial values of the storage index and incremental storage spend, outcome value, optimization value
  store_index <- 1
  store_sp_inc <- vector("numeric",nrow(diag_mat))
  store_mv_inc <- vector("numeric",nrow(diag_mat))
  store_out_inc <- vector("numeric",nrow(diag_mat))
  store_maxim_inc <- vector("numeric",nrow(diag_mat))
  
  #####Define iteration & repeat
  iter <- 0
  repeat{
    ####Increase the iteration
    iter <- iter+1
    
    ####Print budget
    if(iter%%100==0){print(paste(diag_mat_name,round(period_inc*iter*num_periods),sep=" | "))}
    
    ####Set up incremental vectors
    mv_inc <- rep(-Inf,nrow(diag_mat))
    cv_inc <- rep(-Inf,nrow(diag_mat))
    cvfee_inc <- rep(-Inf,nrow(diag_mat))
    out_inc <- rep(-Inf,nrow(diag_mat))
    maxim_inc <- rep(-Inf,nrow(diag_mat))
    
    ####Loop through each possibility
    for(j in tact_ind){
      ###Get the current MV (old plan spend in MV) and the fee schedule
      mv_curr <- (sum(old_mv[which(diag_mat[,"Plan Name"]==diag_mat[j,"Plan Name"])])*gran_mult)
      fees <- fee_list[[which(names(fee_list)==diag_mat[j,"Fee Schedule"])]]
      
      ###Derive the new incremental spends
      new_sp_inc <- do.call(inc_fun,list("mv_curr"=mv_curr,"inc"=(period_inc*gran_mult),"cv_perc"=diag_mat[j,"CV %"],"fees"=fees))/gran_mult
      
      ###Get the new spend, outcome, and optimization values
      new_mv <- old_mv
      new_mv[j] <- new_mv[j]+new_sp_inc[1]
      new_out <- do.call(what=out_fun,args=c(list("sp"=new_mv,"diag_mat"=diag_mat),out_fun_args))
      new_maxim <- do.call(what=maxim_fun,args=c(list("sp"=new_mv,"diag_mat"=diag_mat),maxim_fun_args))
      
      ###Get the new incremental outcome and optimization values
      new_out_inc <- new_out[j]-old_out[j]
      new_maxim_inc <- new_maxim[j]-old_maxim[j]
      
      ###Update the constraints in a temporary matrix
      ##Make a copy and extract the constraints
      new_cm_index <- diag_cm_list[[j]]
      new_cm_mat <- cm_mat[new_cm_index,,drop=F]
      
      ##Update the matrix
      #Budgets and outcomes
      new_cm_mat[,"Max Budget Current"] <- new_cm_mat[,"Max Budget Current"]+new_sp_inc[sp_nums[new_cm_index]]
      new_cm_mat[,"Max Cost Per Budget Current"] <- new_cm_mat[,"Max Cost Per Budget Current"]+new_sp_inc[mcp_nums[new_cm_index]]
      new_cm_mat[,"Outcome Current"] <- new_cm_mat[,"Outcome Current"]+new_out_inc
      
      #Max cost per current
      if(cost_per_type==1){
        new_cm_mat[,"Max Cost Per Current"] <- new_cm_mat[,"Max Cost Per Budget Current"]/new_cm_mat[,"Outcome Current"]
      }
      else{
        new_cm_mat[,"Max Cost Per Current"] <- new_cm_mat[,"Outcome Current"]/new_cm_mat[,"Max Cost Per Budget Current"]
      }
      
      ###Define booleans
      sp_bool <- sum(new_cm_mat[,"Max Budget"]-new_cm_mat[,"Max Budget Current"]>=0)==nrow(new_cm_mat)
      mcp_bool <- sum(new_cm_mat[,"Max Cost Per"]-new_cm_mat[,"Max Cost Per Current"]>=0)==nrow(new_cm_mat)
      
      ###If booleans are both true, replace -Inf with value
      if(sp_bool&mcp_bool){
        mv_inc[j] <- new_sp_inc[1]
        cv_inc[j] <- new_sp_inc[2]
        cvfee_inc[j] <- new_sp_inc[3]
        out_inc[j] <- new_out_inc
        maxim_inc[j] <- new_maxim_inc
      }
    }
    
    ####Redefine the tactic vector
    tact_ind <- c(1:nrow(diag_mat))[which(maxim_inc!=-Inf)]
    
    ####Stop if a) the maximization values are all -Inf - this means no more mixes can be found or
    ####b) if the budget exceeds the max. Trim the matrices and fill in current value, if needed
    if(sum(maxim_inc!=-Inf)==0|iter>tot_iter){
      if(store_iter_vec[store_index]!=(iter-1)){end_index <- store_index+1}
      else{end_index <- store_index}
      sp_mat <- sp_mat[1:end_index,,drop=F]
      mv_mat <- mv_mat[1:end_index,,drop=F]
      out_mat <- out_mat[1:end_index,,drop=F]
      out_inc_mat <- out_inc_mat[1:end_index,,drop=F]
      maxim_mat <- maxim_mat[1:end_index,,drop=F]
      maxim_inc_mat <- maxim_inc_mat[1:end_index,,drop=F]
      if(store_iter_vec[store_index]!=(iter-1)){
        sp_mat[end_index,] <- sp_mat[end_index-1,]+store_sp_inc
        mv_mat[end_index,] <- mv_mat[end_index-1,]+store_mv_inc
        out_mat[end_index,] <- out_mat[end_index-1,]+store_out_inc
        maxim_mat[end_index,] <- maxim_mat[end_index-1,]+store_maxim_inc
        out_inc_mat[end_index,] <- store_out_inc
        maxim_inc_mat[end_index,] <- store_maxim_inc
      }
      break
    }
    
    ####Otherwise get the max and add to matrices if iter is in one of the storage location
    ###Find max
    max_index <- which.max(maxim_inc)
    
    ###Update constraint matrix
    ##Extract the index
    cm_index <- diag_cm_list[[max_index]]
    
    ##Update the matrix
    #Budgets and outcomes
    cm_mat[cm_index,"Max Budget Current"] <- cm_mat[cm_index,"Max Budget Current"]+
      c(mv_inc[max_index],cv_inc[max_index],cvfee_inc[max_index])[sp_nums[cm_index]]
    cm_mat[cm_index,"Max Cost Per Budget Current"] <- cm_mat[cm_index,"Max Cost Per Budget Current"]+
      c(mv_inc[max_index],cv_inc[max_index],cvfee_inc[max_index])[mcp_nums[cm_index]]
    cm_mat[cm_index,"Outcome Current"] <- cm_mat[cm_index,"Outcome Current"]+out_inc[max_index]
    
    #Max cost per current
    if(cost_per_type==1){
      cm_mat[cm_index,"Max Cost Per Current"] <- 
        cm_mat[cm_index,"Max Cost Per Budget Current"]/cm_mat[cm_index,"Outcome Current"]
    }
    else{
      cm_mat[cm_index,"Max Cost Per Current"] <- 
        cm_mat[cm_index,"Outcome Current"]/cm_mat[cm_index,"Max Cost Per Budget Current"]
    }
    
    ###Redefine old values of spend, outcome value, and optimization value
    old_mv[max_index] <- old_mv[max_index]+mv_inc[max_index]
    old_out[max_index] <- old_out[max_index]+out_inc[max_index]
    old_maxim[max_index] <- old_maxim[max_index]+maxim_inc[max_index]
    
    ###Add to storage vectors: note that storage is done on MV and the desired basis
    store_sp_inc[max_index] <- store_sp_inc[max_index]+period_inc
    store_mv_inc[max_index] <- store_mv_inc[max_index]+mv_inc[max_index]
    store_out_inc[max_index] <- store_out_inc[max_index]+out_inc[max_index]
    store_maxim_inc[max_index] <- store_maxim_inc[max_index]+maxim_inc[max_index]
    
    ###If iteration is one of the storage iterations, add to matrices
    if(iter%in%store_iter_vec){
      ##Get the storage index
      store_index <- which(store_iter_vec==iter)
      
      ##Fill in values
      sp_mat[store_index,] <- sp_mat[store_index-1,]+store_sp_inc
      mv_mat[store_index,] <- mv_mat[store_index-1,]+store_mv_inc
      out_mat[store_index,] <- out_mat[store_index-1,]+store_out_inc
      maxim_mat[store_index,] <- maxim_mat[store_index-1,]+store_maxim_inc
      out_inc_mat[store_index,] <- store_out_inc
      maxim_inc_mat[store_index,] <- store_maxim_inc
      
      ##Reset storage incremental values
      store_sp_inc <- vector("numeric",nrow(diag_mat))
      store_mv_inc <- vector("numeric",nrow(diag_mat))
      store_out_inc <- vector("numeric",nrow(diag_mat))
      store_maxim_inc <- vector("numeric",nrow(diag_mat))
    }
  }
  
  ####Return
  return(list(sp_mat=sp_mat,mv_mat=mv_mat,out_mat=out_mat,out_inc_mat=out_inc_mat,maxim_mat=maxim_mat,maxim_inc_mat=maxim_inc_mat))
}