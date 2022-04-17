#' Performance Optimization
#'
#' This function runs the performance optimization
#' @param CheckExamples this has full parameter list
#' 
#' @keywords Performance Optimization
#' @export
#' @examples
#' opt_mix(basedir,pa,date,process_data_fun,cost_per_type=1,num_periods,
#' basis="CV + Fee",scenarios=NULL,B_max=100000000,store_inc=1000,precision=10,
#' out_fun=out_fun_standard,maxim_fun=maxim_fun_standard,out_fun_args=NULL,
#' maxim_fun_args=NULL)

perform_optimization <- function(basedir,pa,date,process_data_fun,cost_per_type=1,num_periods,
                                 basis="CV + Fee",scenarios=NULL,B_max=100000000,store_inc=1000,precision=10,
                                 out_fun=out_fun_standard,maxim_fun=maxim_fun_standard,out_fun_args=NULL,
                                 maxim_fun_args=NULL){
  #####Check process_data_fun, cost_per_type, num_periods, basis, B_max, store_inc, precision, out_fun, and maxim_fun
  if(length(process_data_fun)!=1){stop("'Process Data' function must be a single function found in the environment.")}
  if(!is.character(process_data_fun)){stop("'Process Data' function must be a single function found in the environment.")}
  if(!exists(process_data_fun)){stop("'Process Data' function must be a single function found in the environment.")}
  
  if(length(cost_per_type)!=1){stop("Cost per type must be a 1 or 2.")}
  if(!is.numeric(cost_per_type)){stop("Cost per type must be a 1 or 2.")}
  if(!cost_per_type%in%c(1,2)){stop("Cost per type must be a 1 or 2.")}
  
  if(length(num_periods)!=1){stop("Number of periods must be a single, numeric value.")}
  if(!is.numeric(num_periods)){stop("Number of periods must be a single, numeric value.")}
  if(round(num_periods)!=num_periods){stop("Number of periods must be a single, numeric value.")}
  
  if(length(basis)!=1){stop("Basis must be one of 'MV', 'CV', or 'CV + Fee'.")}
  if(!is.character(basis)){stop("Basis must be one of 'MV', 'CV', or 'CV + Fee'.")}
  if(!basis%in%c("MV","CV","CV + Fee")){stop("Basis must be one of 'MV', 'CV', or 'CV + Fee'.")}
  
  if(length(B_max)!=1){stop("Max budget must be a single, numeric value.")}
  if(!is.numeric(B_max)){stop("Max budget must be a single, numeric value.")}
  if(round(B_max)!=B_max){stop("Max budget must be a single, numeric value.")}
  
  if(length(store_inc)!=1){stop("Storage increment must be a single, numeric value.")}
  if(!is.numeric(store_inc)){stop("Storage increment must be a single, numeric value.")}
  if(round(store_inc)!=store_inc){stop("Storage increment must be a single, numeric value.")}
  
  if(length(precision)!=1){stop("Precision must be a single, numeric value.")}
  if(!is.numeric(precision)){stop("Precision must be a single, numeric value.")}
  if(round(precision)!=precision){stop("Precision must be a single, numeric value.")}
  
  if(length(out_fun)!=1){stop("Outcome function must be a single function found in the environment.")}
  if(class(try(typeof(out_fun),silent=T))=="try-error"){
    stop("Outcome function must be a single function found in the environment.")
  }
  if(typeof(out_fun)!="closure"){
    stop("Outcome function must be a single function found in the environment.")
  }
  
  if(length(maxim_fun)!=1){stop("Maximization function must be a single function found in the environment.")}
  if(class(try(typeof(maxim_fun),silent=T))=="try-error"){
    stop("Maximization function must be a single function found in the environment.")
  }
  if(typeof(maxim_fun)!="closure"){
    stop("Maximization function must be a single function found in the environment.")
  }
  
  #####Check and read in fee, rate, and constraint matrices: by virtue of the processed constraint matrices
  #####being found, then the fee & rate matrices have been checked
  ####Get folders
  fee_folder <- paste(basedir,"PAs",pa,date,"Planning Inputs/Fee Schedules",sep="/")
  if(!file.exists(fee_folder)){stop("Folder does not exist. Consider running the 'Create Folder' script.")}
  rate_folder <- paste(basedir,"PAs",pa,date,"Planning Inputs/Rate Matrices",sep="/")
  if(!file.exists(rate_folder)){stop("Folder does not exist. Consider running the 'Create Folder' script.")}
  cm_folder <- paste(basedir,"PAs",pa,date,"Planning Inputs/Constraint Matrices/Processed",sep="/")
  if(!file.exists(cm_folder)){stop("Folder does not exist. Consider running the 'Create Folder' script.")}
  
  ####Get files
  fee_files <- list.files(fee_folder,full.names=T)
  fee_files <- grep("\\.csv",fee_files,value=T)
  if(length(fee_files)==0){stop("No CSVs exist in the fee schedule folder. At least one must be present.")}
  fee_list <- lapply(fee_files,read_csv)
  names(fee_list) <- gsub("\\.csv","",gsub("^.+/","",fee_files))
  
  rate_files <- list.files(rate_folder,full.names=T)
  rate_files <- grep("\\.csv",rate_files,value=T)
  if(length(rate_files)==0){stop("No CSVs exist in the rate matrix folder. At least one must be present.")}
  rate_list <- lapply(rate_files,read_csv)
  
  cm_files <- list.files(cm_folder,full.names=T)
  cm_files <- grep("\\.csv",cm_files,value=T)
  if(length(cm_files)==0){stop("No CSVs exist in the constraint matrix folder. At least one must be present.")}
  cm_list <- lapply(cm_files,read_csv)
  
  #####Check and read in response curve summary
  diag_mat_file <- paste(basedir,"PAs",pa,date,"Data/Processed/Response Curve Summary.csv",sep="/")
  if(!file.exists(diag_mat_file)){stop("Response curves have not been generated. Consider running the 'Build Response Curves' script.")}
  diag_mat <- read_csv(diag_mat_file,row.names=1)
  
  #####Check output folders exist
  sp_folder <- paste(basedir,"PAs",pa,date,"Optimal Mixes/Budgets",sep="/")
  if(!file.exists(sp_folder)){stop("Folder does not exist. Consider running the 'Create Folder' script.")}
  mv_folder <- paste(basedir,"PAs",pa,date,"Optimal Mixes/Budgets (MV)",sep="/")
  if(!file.exists(mv_folder)){stop("Folder does not exist. Consider running the 'Create Folder' script.")}
  out_folder <- paste(basedir,"PAs",pa,date,"Optimal Mixes/Outcomes",sep="/")
  if(!file.exists(out_folder)){stop("Folder does not exist. Consider running the 'Create Folder' script.")}
  mcp_folder <- paste(basedir,"PAs",pa,date,"Optimal Mixes/Cost Pers",sep="/")
  if(!file.exists(mcp_folder)){stop("Folder does not exist. Consider running the 'Create Folder' script.")}
  
  #####Get the granularity multiplier; by virtue of the processed constraint matrices being found, the granularity has been checked.
  granularity <- do.call(process_data_fun,list("basedir"=basedir,"pa"=pa,"date"=date,process=F))
  gran_mult <- 91/ifelse(granularity=="Daily",1,
                         ifelse(granularity=="Weekly",7,
                                ifelse(granularity=="Quarterly",91,365)))
  gran_mult <- ifelse(num_periods<gran_mult,num_periods,gran_mult)
  
  #####Reduce the number of constraints to check, to those where one of the max budget or the max cost per is finite
  cm_list <- lapply(cm_list,function(mat){mat[which(is.finite(mat[,"Max Budget"])|is.finite(mat[,"Max Cost Per"])),,drop=F]})
  
  #####Add segment as a column in the response curve summary
  diag_mat$"Segment" <- rownames(diag_mat)
  
  #####Append rate matrix to response curve summary; by virtue of the file checks above, the desired column
  #####names will be present in both - thanks to needing the check planning inputs and build response curves scripts
  diag_mat_list <- lapply(rate_list,merge,x=diag_mat,all.x=T)
  
  #####For each response curve summary matrix, determine the corresponding rows in the constraint matrix
  diag_cm_list_list <- vector("list",length(diag_mat_list))
  for(i in 1:length(diag_mat_list)){
    diag_cm_list_list[[i]] <- vector("list",nrow(diag_mat_list[[i]]))
    cm_temp_cols <- colnames(cm_list[[i]])[which(!colnames(cm_list[[i]])%in%c("Max Budget","Max Cost Per",
                                                                              "Max Budget Value Basis",
                                                                              "Max Cost Per Value Basis"))]
    for(j in 1:nrow(diag_mat_list[[i]])){
      diag_cm_text <- paste("which(",paste(paste("(cm_list[[i]][,\"",
                                                 cm_temp_cols,
                                                 "\"]==diag_mat_list[[i]][j,\"",
                                                 cm_temp_cols,
                                                 "\"]|cm_list[[i]][,\"",
                                                 cm_temp_cols,
                                                 "\"]==\"All\")",sep=""),collapse="&"),")",sep="")
      diag_cm_list_list[[i]][[j]] <- eval(parse(text=diag_cm_text))
    }
  }
  
  #####Get the response curve summary matrix names
  diag_mat_names <- gsub("\\.csv","",gsub(paste(cm_folder,"/",sep=""),"",cm_files))
  
  #####Check scenarios and limit
  ####Check
  if(!is.null(scenarios)){
    if(!is.character(scenarios)){stop("No constraint matrix corresponds to the scenarios supplied.")}
    if(sum(scenarios%in%diag_mat_names)!=length(scenarios)){stop("No constraint matrix corresponds to the scenarios supplied.")}
  }
  
  ####Limit on specified scenarios
  if(!is.null(scenarios)){scen_index <- which(diag_mat_names%in%scenarios)}
  else{scen_index <- 1:length(diag_mat_names)}
  diag_mat_list <- diag_mat_list[scen_index]
  diag_mat_names <- diag_mat_names[scen_index]
  cm_list <- cm_list[scen_index]
  diag_cm_list_list <- diag_cm_list_list[scen_index]
  
  #####Now create optimal mixes
  sp_mat_list <- vector("list",length(diag_mat_list))
  mv_mat_list <- vector("list",length(diag_mat_list))
  out_mat_list <- vector("list",length(diag_mat_list))
  mcp_mat_list <- vector("list",length(diag_mat_list))
  for(i in 1:length(diag_mat_list)){
    ###Run optimal mix
    o <- opt_mix(diag_mat=diag_mat_list[[i]],diag_mat_name=diag_mat_names[i],cm_mat=cm_list[[i]],
                 diag_cm_list=diag_cm_list_list[[i]],fee_list=fee_list,process_data_fun=process_data_fun,
                 cost_per_type=cost_per_type,num_periods=num_periods,basis=basis,gran_mult=gran_mult,
                 B_max=B_max,store_inc=store_inc,precision=precision,out_fun=out_fun,maxim_fun=maxim_fun,
                 out_fun_args=out_fun_args,maxim_fun_args=maxim_fun_args)
    
    ###Store output
    ##Extract and calculate cost pers & replace first row with 0 since it will be NaN
    sp_mat <- o$sp_mat*num_periods
    mv_mat <- o$mv_mat*num_periods
    out_mat <- o$out_mat*num_periods
    if(cost_per_type==1){mcp_mat <- sp_mat/out_mat}
    else{mcp_mat <- out_mat/sp_mat}
    mcp_mat <- apply(mcp_mat,2,function(vec){vec[which(is.nan(vec))] <- 0; return(vec)})
    
    ##Change row names and add to list
    rownames(sp_mat) <- paste("$",gsub(" +","",format(round(rowSums(sp_mat))/1000000,scientific=F)),"M",sep="")
    rownames(mv_mat) <- paste("$",gsub(" +","",format(round(rowSums(mv_mat))/1000000,scientific=F)),"M",sep="")
    rownames(out_mat) <- paste("$",gsub(" +","",format(round(rowSums(out_mat))/1000000,scientific=F)),"M",sep="")
    rownames(mcp_mat) <- paste("$",gsub(" +","",format(round(rowSums(mcp_mat))/1000000,scientific=F)),"M",sep="")
    sp_mat_list[[i]] <- sp_mat
    mv_mat_list[[i]] <- mv_mat
    out_mat_list[[i]] <- out_mat
    mcp_mat_list[[i]] <- mcp_mat
  }
  
  #####Output matrices
  silence <- mapply(write.csv,sp_mat_list,paste(sp_folder,"/",diag_mat_names,".csv",sep=""),SIMPLIFY=F)
  silence <- mapply(write.csv,mv_mat_list,paste(mv_folder,"/",diag_mat_names,".csv",sep=""),SIMPLIFY=F)
  silence <- mapply(write.csv,out_mat_list,paste(out_folder,"/",diag_mat_names,".csv",sep=""),SIMPLIFY=F)
  silence <- mapply(write.csv,mcp_mat_list,paste(mcp_folder,"/",diag_mat_names,".csv",sep=""),SIMPLIFY=F)
}