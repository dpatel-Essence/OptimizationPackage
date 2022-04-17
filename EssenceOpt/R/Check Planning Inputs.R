#' Check Planning Inputs
#'
#' Check the planning inputs given
#' @param basedir,pa,date Location of directory
#' @param process_data_fun processed data
#' @param cost_per_type must be 1 or 2 
#' 
#' 
#' @keywords Check planning inputs
#' @export
#' @examples
#' check_planning_inputs(basedir,pa,date,process_data_fun,cost_per_type=1)


check_planning_inputs <- function(basedir,pa,date,process_data_fun,cost_per_type=1){
  #####Check the inputs and file structure
  ####Check the process data function and value of cost per type
  if(length(process_data_fun)!=1){stop("'Process Data' function must be a single function found in the environment.")}
  if(!is.character(process_data_fun)){stop("'Process Data' function must be a single function found in the environment.")}
  if(!exists(process_data_fun)){stop("'Process Data' function must be a single function found in the environment.")}
  if(length(cost_per_type)!=1){stop("Cost per type must be a 1 or 2.")}
  if(!is.numeric(cost_per_type)){stop("Cost per type must be a 1 or 2.")}
  if(!cost_per_type%in%c(1,2)){stop("Cost per type must be a 1 or 2.")}
  
  ####Get folders
  fee_folder <- paste(basedir,"PAs",pa,date,"Planning Inputs/Fee Schedules",sep="/")
  if(!file.exists(fee_folder)){stop("Folder does not exist. Consider running the 'Create Folder' script.")}
  rate_folder <- paste(basedir,"PAs",pa,date,"Planning Inputs/Rate Matrices",sep="/")
  if(!file.exists(rate_folder)){stop("Folder does not exist. Consider running the 'Create Folder' script.")}
  cm_folder <- paste(basedir,"PAs",pa,date,"Planning Inputs/Constraint Matrices/Raw",sep="/")
  if(!file.exists(cm_folder)){stop("Folder does not exist. Consider running the 'Create Folder' script.")}
  
  ####Get files
  fee_files <- list.files(fee_folder,full.names=T)
  fee_files <- grep("\\.csv",fee_files,value=T)
  if(length(fee_files)==0){stop("No CSVs exist in the fee schedule folder. At least one must be present.")}
  
  rate_files <- list.files(rate_folder,full.names=T)
  rate_files <- grep("\\.csv",rate_files,value=T)
  if(length(rate_files)==0){stop("No CSVs exist in the rate matrix folder. At least one must be present.")}
  
  cm_files <- list.files(cm_folder,full.names=T)
  cm_files <- grep("\\.csv",cm_files,value=T)
  if(length(cm_files)==0){stop("No CSVs exist in the constraint matrix folder. At least one must be present.")}
  
  ####Check files match and read in
  if(length(rate_files)!=length(cm_files)){stop("Rate and constraint matrices must have the same names.")}
  if(sum(gsub("^.+/","",rate_files)==gsub("^.+/","",cm_files))!=length(rate_files)){
    stop("Rate and constraint matrices must have the same names.")
  }
  fee_list <- lapply(fee_files,read_csv)
  rate_list <- lapply(rate_files,read_csv)
  cm_list <- lapply(cm_files,read_csv)
  
  ####Check and read in templates
  rate_template_file <- paste(basedir,"PAs",pa,date,"Planning Inputs/Template Output/Rate Template.csv",sep="/")
  if(!file.exists(rate_template_file)){stop("Rate template does not exist. Consider running the 'Create Templates' script.")}
  rate_template <- read_csv(rate_template_file)
  cm_template_file <- paste(basedir,"PAs",pa,date,"Planning Inputs/Template Output/Constraint Matrix Template.csv",sep="/")
  if(!file.exists(cm_template_file)){stop("Constraint matrix template does not exist. Consider running the 'Create Templates' script.")}
  cm_template <- read_csv(cm_template_file)
  
  ####Check and read in data file
  ###Read file
  data_file <- paste(basedir,"PAs",pa,date,"Data/Processed/Processed Data.csv",sep="/")
  if(!file.exists(data_file)){stop("Data does not exist. Consider running the 'Process Data' script.")}
  dat <- read_csv(data_file)
  
  ###Get and check granularity; define basis values for further down
  gran_vals <- c("Daily","Weekly","Quarterly","Yearly")
  basis_vals <- c("MV","CV","CV + Fee")
  granularity <- do.call(process_data_fun,list("basedir"=basedir,"pa"=pa,"date"=date,process=F))
  if(length(granularity)!=1){stop("Data granularity does not match standard values. Consider updating the 'Process Data' script.")}
  if(!is.character(granularity)){stop("Data granularity does not match standard values. Consider updating the 'Process Data' script.")}
  if(!granularity%in%gran_vals){stop("Data granularity does not match standard values. Consider updating the 'Process Data' script.")}
  
  ###Check columns
  if(!"Date"%in%colnames(dat)){stop("Data must have a 'Date' column in a date format. Consider updating the 'Process Data' script.")}
  if(!"MV Spend"%in%colnames(dat)){stop("Data must have a 'MV Spend' column in a date format. Consider updating the 'Process Data' script.")}
  if(!"Outcome"%in%colnames(dat)){stop("Data must have a 'Outcome' column in a date format. Consider updating the 'Process Data' script.")}
  if(!"MV Cost Per"%in%colnames(dat)){stop("Data must have a 'MV Cost Per' column in a date format. Consider updating the 'Process Data' script.")}
  if(sum(grepl("^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]$",dat[,"Date"]))!=nrow(dat)){
    stop("Data must have a 'Date' column in a date format. Consider updating the 'Process Data' script.")
  }
  if(!is.numeric(dat[,"MV Spend"])){stop("Data must have a 'MV Spend' column that is numeric. Consider updating the 'Process Data' script.")}
  if(!is.numeric(dat[,"Outcome"])){stop("Data must have a 'Outcome' column that is numeric. Consider updating the 'Process Data' script.")}
  if(!is.numeric(dat[,"MV Cost Per"])){stop("Data must have a 'MV Cost Per' column that is numeric. Consider updating the 'Process Data' script.")}
  
  #####Check the fee schedules
  for(i in 1:length(fee_list)){
    ####Check column names
    if(ncol(fee_list[[i]])!=3){stop("Fees can only have three columns: 'Lower Bound', 'Upper Bound', and 'Fee %'.")}
    if(sum(colnames(fee_list[[i]])==c("Lower Bound","Upper Bound","Fee %"))!=3){
      stop("Fees can only have three columns: 'Lower Bound', 'Upper Bound', and 'Fee %'.")
    }
    
    ####Check bounds
    ###Numeric
    if(!is.numeric(fee_list[[i]][,"Lower Bound"])){stop("Lower bound must be numeric.")}
    if(!is.numeric(fee_list[[i]][,"Upper Bound"])){stop("Upper bound must be numeric.")}
    
    ###Non-NA
    if(sum(is.na(fee_list[[i]][,"Lower Bound"]))!=0){stop("Not all possible spend values found in fee schedule.")}
    if(sum(is.na(fee_list[[i]][,"Upper Bound"]))!=0){stop("Not all possible spend values found in fee schedule.")}
    
    ###Lowest lower limit and highest upper limit
    if(fee_list[[i]][1,"Lower Bound"]!=0){stop("Lowest lower bound must be 0.")}
    if(fee_list[[i]][nrow(fee_list[[i]]),"Upper Bound"]!=Inf){stop("Highest upper bound must be Inf.")}
    
    ###Other bounds should be offset by one row
    if(nrow(fee_list[[i]])>1){
      if(sum(fee_list[[i]][2:nrow(fee_list[[i]]),"Lower Bound"]==
             fee_list[[i]][1:(nrow(fee_list[[i]])-1),"Upper Bound"])
         !=(nrow(fee_list[[i]])-1)){
        stop("Not all possible spend values found in fee schedule.")
      }
    }
    
    ####Fee percents are numeric, and are completely filled in with values between 0 and 1
    if(!is.numeric(fee_list[[i]][,"Fee %"])){stop("Fee % must be numeric.")}
    if(sum(is.na(fee_list[[i]][,"Fee %"]))!=0){stop("Fee % must be a percent between 0 and 1.")}
    if(sum(fee_list[[i]][,"Fee %"]<=1&fee_list[[i]][,"Fee %"]>=0)!=nrow(fee_list[[i]])){stop("Fee % must be a percent between 0 and 1.")}
  }
  
  #####Check the rate matrices
  rate_st_cols <- c("CV %","Plan Name","Fee Schedule")
  for(i in 1:length(rate_list)){
    ####Check layouts
    if(nrow(rate_list[[i]])!=nrow(rate_template)){stop("One or more rate matrices do not have the same layout as the template.")}
    if(ncol(rate_list[[i]])!=ncol(rate_template)){stop("One or more rate matrices do not have the same layout as the template.")}
    if(sum(colnames(rate_list[[i]])==colnames(rate_template))!=ncol(rate_template)){
      stop("One or more rate matrices do not have the same layouts as the template.")
    }
    rate_temp_index <- which(!colnames(rate_template)%in%rate_st_cols)
    rate_temp_eq <- all.equal(rate_list[[i]][,rate_temp_index,drop=F],rate_template[,rate_temp_index,drop=F])
    if(!is.logical(rate_temp_eq)){stop("One or more rate matrices do not have the same layout as the template.")}
    if(!rate_temp_eq){stop("One or more rate matrices do not have the same layout as the template.")}
    
    ####Check that the data combinations are found in the constraint matrix
    rate_temp_cols <- colnames(rate_template)[rate_temp_index]
    if(sum(rate_temp_cols%in%colnames(dat))!=length(rate_temp_cols)){
      stop("One or more rate matrices have column names that do not match the data.")
    }
    rate_dat_bool <- sum(unlist(apply(dat[,rate_temp_cols],1,function(vec,dat){
      length(eval(parse(text=paste("which(",paste(paste("dat[,",1:length(vec),
                                                        "]==vec[",1:length(vec),
                                                        "]",sep=""),collapse="&"),")",sep=""))))==1
    },dat=rate_list[[i]][,rate_temp_cols])))==nrow(dat)
    if(!rate_dat_bool){stop("One or more rate matrices do not contain the values in the data.")}
    
    ####Check the inputs
    ###Column names
    if(!sum(rate_st_cols%in%colnames(rate_list[[i]]))){
      stop("One or more rate matrices have column names not as expected.")
    }
    
    ###CV percents are numeric, and are completely filled in with values between 0 and 1
    if(!is.numeric(rate_list[[i]][,"CV %"])){stop("CV % must be numeric.")}
    if(sum(is.na(rate_list[[i]][,"CV %"]))!=0){stop("CV % must be a percent between 0 and 1.")}
    if(sum(rate_list[[i]][,"CV %"]<=1&rate_list[[i]][,"CV %"]>=0)!=nrow(rate_list[[i]])){stop("CV % must be a percent between 0 and 1.")}
    
    ###Plan name is a character and is completely filled in
    if(!is.character(rate_list[[i]][,"Plan Name"])){stop("Plan name must be a character.")}
    if(sum(is.na(rate_list[[i]][,"Plan Name"]))!=0){stop("Plan name must be specified for each line item.")}
    
    ###Fee schedule is a charactera and is completely filled in
    if(!is.character(rate_list[[i]][,"Fee Schedule"])){stop("Fee schedule must be a character.")}
    if(sum(is.na(rate_list[[i]][,"Fee Schedule"]))!=0){stop("Fee schedule must be specified for each line item.")}
    
    ###Each plan name has one fee schedule, and fee schedule matches the schedule names in the folder
    if(nrow(unique(rate_list[[i]][,c("Plan Name","Fee Schedule"),drop=F]))!=length(unique(rate_list[[i]][,"Plan Name"]))){
      stop("Plan name can only have one associated fee schedule.")
    }
    if(sum(rate_list[[i]][,"Fee Schedule"]%in%gsub("\\.csv","",gsub("^.+/","",fee_files)))!=nrow(rate_list[[i]])){
      stop("Fee schedule is not found in the folder.")
    }
  }
  
  #####Check the constraint matrices and capture certain values
  ####Initiate variables & check
  cm_st_cols <- c("Max Budget","Max Cost Per","Max Budget Granularity",
                  "Max Budget Value Basis","Max Cost Per Value Basis")
  mb_type_list <- vector("list",length(cm_list))
  mb_num_list <- vector("list",length(cm_list))
  mb_date_list <- vector("list",length(cm_list))
  mcp_type_list <- vector("list",length(cm_list))
  mcp_num_list <- vector("list",length(cm_list))
  mcp_date_list <- vector("list",length(cm_list))
  
  ####Loop
  for(i in 1:length(cm_list)){
    ####Check layouts
    if(nrow(cm_list[[i]])!=nrow(cm_template)){stop("One or more constraint matrices do not have the same layout as the template.")}
    if(ncol(cm_list[[i]])!=ncol(cm_template)){stop("One or more constraint matrices do not have the same layout as the template.")}
    if(sum(colnames(cm_list[[i]])==colnames(cm_template))!=ncol(cm_template)){
      stop("One or more constraint matrices do not have the same layouts as the template.")
    }
    cm_temp_index <- which(!colnames(cm_template)%in%cm_st_cols)
    cm_temp_eq <- all.equal(cm_list[[i]][,cm_temp_index,drop=F],cm_template[,cm_temp_index,drop=F])
    if(!is.logical(cm_temp_eq)){stop("One or more constraint matrices do not have the same layout as the template.")}
    if(!cm_temp_eq){stop("One or more constraint matrices do not have the same layout as the template.")}
    
    ####Check that the data combinations are found in the constraint matrix
    cm_temp_cols <- colnames(cm_template)[cm_temp_index]
    if(sum(cm_temp_cols%in%colnames(dat))!=length(cm_temp_cols)){
      stop("One or more constraint matrices have column names that do not match the data.")
    }
    cm_dat_bool <- sum(unlist(apply(dat[,cm_temp_cols],1,function(vec,dat){
      length(eval(parse(text=paste("which(",paste(paste("dat[,",1:length(vec),
                                                        "]==vec[",1:length(vec),
                                                        "]",sep=""),collapse="&"),")",sep=""))))==1
    },dat=cm_list[[i]][,cm_temp_cols])))==nrow(dat)
    if(!cm_dat_bool){stop("One or more constraint matrices do not contain the values in the data.")}
    
    ####Check the inputs
    ###Column names
    if(!sum(cm_st_cols%in%colnames(cm_list[[i]]))){
      stop("One or more constraint matrices have column names not as expected.")
    }
    
    ###Columns filled in match others
    if(sum(apply(cm_list[[i]][,c(grep("Max Budget",cm_st_cols,value=T)),drop=F],1,function(vec){
      sum(is.na(vec)|vec=="")==0|sum(is.na(vec)|vec=="")==length(vec)
    }))!=nrow(cm_list[[i]])){stop("One or more constraint matrices have unmatched max budget values.")}
    if(sum(apply(cm_list[[i]][,c(grep("Max Cost Per",cm_st_cols,value=T)),drop=F],1,function(vec){
      sum(is.na(vec)|vec=="")==0|sum(is.na(vec)|vec=="")==length(vec)
    }))!=nrow(cm_list[[i]])){stop("One or more constraint matrices have unmatched max cost per values.")}
    
    ###Granularity and basis are among the standard default values
    if(sum(is.na(cm_list[[i]][,"Max Budget Granularity"]))==nrow(cm_list[[i]])){
      cm_list[[i]][,"Max Budget Granularity"] <- as.character(cm_list[[i]][,"Max Budget Granularity"])
    }
    if(sum(is.na(cm_list[[i]][,"Max Budget Value Basis"]))==nrow(cm_list[[i]])){
      cm_list[[i]][,"Max Budget Value Basis"] <- as.character(cm_list[[i]][,"Max Budget Value Basis"])
    }
    if(sum(is.na(cm_list[[i]][,"Max Cost Per Value Basis"]))==nrow(cm_list[[i]])){
      cm_list[[i]][,"Max Cost Per Value Basis"] <- as.character(cm_list[[i]][,"Max Cost Per Value Basis"])
    }
    
    if(!is.character(cm_list[[i]][,"Max Budget Granularity"])){
      stop(paste("Max budget granularity must be one of ",paste(paste("'",gran_vals,"'",sep=""),collapse=", "),".",sep=""))
    }
    if(!is.character(cm_list[[i]][,"Max Budget Value Basis"])){
      stop(paste("Max budget value basis must be one of ",paste(paste("'",basis_vals,"'",sep=""),collapse=", "),".",sep=""))
    }
    if(!is.character(cm_list[[i]][,"Max Cost Per Value Basis"])){
      stop(paste("Max cost per value basis must be one of ",paste(paste("'",basis_vals,"'",sep=""),collapse=", "),".",sep=""))
    }
    
    if(sum(cm_list[[i]][,"Max Budget Granularity"]%in%c("",NA,gran_vals))!=nrow(cm_list[[i]])){
      stop(paste("Max budget granularity must be one of ",paste(paste("'",gran_vals,"'",sep=""),collapse=", "),".",sep=""))
    }
    if(sum(cm_list[[i]][,"Max Budget Value Basis"]%in%c("",NA,basis_vals))!=nrow(cm_list[[i]])){
      stop(paste("Max budget value basis must be one of ",paste(paste("'",basis_vals,"'",sep=""),collapse=", "),".",sep=""))
    }
    if(sum(cm_list[[i]][,"Max Cost Per Value Basis"]%in%c("",NA,basis_vals))!=nrow(cm_list[[i]])){
      stop(paste("Max cost per value basis must be one of ",paste(paste("'",basis_vals,"'",sep=""),collapse=", "),".",sep=""))
    }
    
    ###Max Budget and cost per are numeric or a date
    ##Type of input
    if(sum(is.na(cm_list[[i]][,"Max Budget"]))==nrow(cm_list[[i]])){
      cm_list[[i]][,"Max Budget"] <- as.numeric(cm_list[[i]][,"Max Budget"])
    }
    if(sum(is.na(cm_list[[i]][,"Max Cost Per"]))==nrow(cm_list[[i]])){
      cm_list[[i]][,"Max Cost Per"] <- as.numeric(cm_list[[i]][,"Max Cost Per"])
    }
    
    if(!(is.numeric(cm_list[[i]][,"Max Budget"])|is.character(cm_list[[i]][,"Max Budget"]))){
      stop("Max budget must be a number or a date range.")
    }
    if(!(is.numeric(cm_list[[i]][,"Max Cost Per"])|is.character(cm_list[[i]][,"Max Cost Per"]))){
      stop("Max cost per must be a number or a date range.")
    }
    
    ##Max budget
    mb_type <- rep(NA,nrow(cm_list[[i]]))
    if(!is.numeric(cm_list[[i]][,"Max Budget"])){
      mb_type[which(cm_list[[i]][,"Max Budget"]=="")] <- "Blank"
      
      options(warn=-1)
      mb_num <- as.numeric(cm_list[[i]][,"Max Budget"])
      options(warn=0)
      mb_type[which(!is.na(mb_num))] <- "Numeric"
      mb_num_list[[i]] <- mb_num
      
      mb_date_bool <- grepl("^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]_[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]$",cm_list[[i]][,"Max Budget"])
      mb_type[which(mb_date_bool)] <- "Date"
      mb_dates <- do.call("rbind",strsplit(cm_list[[i]][which(mb_date_bool),"Max Budget"],split="_"))
      mb_date_list[[i]] <- mb_dates
    }
    else{
      mb_num <- cm_list[[i]][,"Max Budget"]
      mb_type[which(is.na(cm_list[[i]][,"Max Budget"]))] <- "Blank"
      mb_type[which(!is.na(cm_list[[i]][,"Max Budget"]))] <- "Numeric"
      mb_num_list[[i]] <- mb_num
    }
    if(sum(is.na(mb_type))!=0){stop("Max budget must be a number or a date range.")}
    mb_type_list[[i]] <- mb_type
    
    ##Max cost per
    mcp_type <- rep(NA,nrow(cm_list[[i]]))
    if(!is.numeric(cm_list[[i]][,"Max Cost Per"])){
      mcp_type[which(cm_list[[i]][,"Max Cost Per"]=="")] <- "Blank"
      
      options(warn=-1)
      mcp_num <- as.numeric(cm_list[[i]][,"Max Cost Per"])
      options(warn=0)
      mcp_type[which(!is.na(mcp_num))] <- "Numeric"
      mcp_num_list[[i]] <- mcp_num
      
      mcp_date_bool <- grepl("^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]_[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]$",cm_list[[i]][,"Max Cost Per"])
      mcp_type[which(mcp_date_bool)] <- "Date"
      mcp_dates <- do.call("rbind",strsplit(cm_list[[i]][which(mcp_date_bool),"Max Cost Per"],split="_"))
      mcp_date_list[[i]] <- mcp_dates
    }
    else{
      mcp_num <- cm_list[[i]][,"Max Cost Per"]
      mcp_type[which(is.na(cm_list[[i]][,"Max Cost Per"]))] <- "Blank"
      mcp_type[which(!is.na(cm_list[[i]][,"Max Cost Per"]))] <- "Numeric"
      mcp_num_list[[i]] <- mcp_num
    }
    if(sum(is.na(mcp_type))!=0){stop("Max cost per must be a number or a date range.")}
    mcp_type_list[[i]] <- mcp_type
    
    ###Dates extracted have at least one value in data
    ##Max budgets
    if(sum(mb_type=="Date")!=0){
      mb_date_val_bool <- sum(unlist(mapply(function(ind1,ind2,mat1,mat2,dat,cm_temp_cols){
        find_vals <- unlist(mat1[ind1,cm_temp_cols])
        start_date <- unlist(mat2[ind2,1])
        end_date <- unlist(mat2[ind2,2])
        row_find_text <- paste("which(",paste(paste("(if(find_vals[",
                                                    1:length(cm_temp_cols),
                                                    "]==\"All\"){rep(TRUE,nrow(dat))} else{dat[,\"",
                                                    cm_temp_cols,
                                                    "\"]==find_vals[",
                                                    1:length(cm_temp_cols),
                                                    "]})",sep=""),collapse="&"),")",sep="")
        dat <- dat[eval(parse(text=row_find_text)),,drop=F]
        if(nrow(dat)==0){return(T)}
        else{
          return(sum(as.Date(dat[,"Date"])>=as.Date(start_date)&as.Date(dat[,"Date"])<=as.Date(end_date))>0)
        }
      },ind1=which(mb_type=="Date"),ind2=1:nrow(mb_dates),
      MoreArgs=list(mat1=cm_list[[i]],mat2=mb_dates,dat=dat,cm_temp_cols=cm_temp_cols),SIMPLIFY=F)))==nrow(mb_dates)
      if(!mb_date_val_bool){stop("Max budget dates must reference at least one value in the data.")}
    }
    
    ##Max cost per
    if(sum(mcp_type=="Date")!=0){
      mcp_date_val_bool <- sum(unlist(mapply(function(ind1,ind2,mat1,mat2,dat,cm_temp_cols){
        find_vals <- unlist(mat1[ind1,cm_temp_cols])
        start_date <- unlist(mat2[ind2,1])
        end_date <- unlist(mat2[ind2,2])
        row_find_text <- paste("which(",paste(paste("(if(find_vals[",
                                                    1:length(cm_temp_cols),
                                                    "]==\"All\"){rep(TRUE,nrow(dat))} else{dat[,\"",
                                                    cm_temp_cols,
                                                    "\"]==find_vals[",
                                                    1:length(cm_temp_cols),
                                                    "]})",sep=""),collapse="&"),")",sep="")
        dat <- dat[eval(parse(text=row_find_text)),,drop=F]
        if(nrow(dat)==0){return(T)}
        else{
          return(sum(as.Date(dat[,"Date"])>=as.Date(start_date)&as.Date(dat[,"Date"])<=as.Date(end_date))>0)
        }
      },ind1=which(mcp_type=="Date"),ind2=1:nrow(mcp_dates),
      MoreArgs=list(mat1=cm_list[[i]],mat2=mcp_dates,dat=dat,cm_temp_cols=cm_temp_cols),SIMPLIFY=F)))==nrow(mcp_dates)
      if(!mcp_date_val_bool){stop("Max cost per dates must reference at least one value in the data.")}
    }
    
    ####Check that at least one constraint exists for each curve
    cm_temp_constr_mat <- cm_list[[i]][which(apply(cm_list[[i]][,cm_temp_cols],1,function(vec){sum(vec=="All")==0})),cm_temp_cols]
    cm_temp_constr_bool <- rep(F,nrow(cm_temp_constr_mat))
    for(j in 1:nrow(cm_temp_constr_mat)){
      cm_temp_constr_text <- paste("which(",paste(paste("(cm_list[[i]][,\"",
                                                        cm_temp_cols,
                                                        "\"]==cm_temp_constr_mat[j,\"",
                                                        cm_temp_cols,
                                                        "\"]|cm_list[[i]][,\"",
                                                        cm_temp_cols,
                                                        "\"]==\"All\")",sep=""),collapse="&"),")",sep="")
      cm_temp_constr_vals <- cm_list[[i]][eval(parse(text=cm_temp_constr_text)),,drop=F]
      cm_temp_constr_bool[j] <- sum(!apply(cm_temp_constr_vals[,cm_st_cols,drop=F],1,function(vec){
        sum(vec==""|is.na(vec))==length(vec)
      }))>0
    }
    if(sum(cm_temp_constr_bool)!=length(cm_temp_constr_bool)){stop("Each curve must be bounded by at least one constraint.")}
  }
  
  #####Reconfigure the output for usage in the optimization
  cm_list_opt <- cm_list
  for(i in 1:length(cm_list_opt)){
    ####Extract columns
    cm_list_opt[[i]] <- cm_list_opt[[i]][,cm_temp_cols,drop=F]
    
    ####Add in blank columns and fill in basis
    cm_list_opt[[i]]$"Max Budget" <- rep(Inf,nrow(cm_list_opt[[i]]))
    cm_list_opt[[i]]$"Max Cost Per" <- rep(Inf,nrow(cm_list_opt[[i]]))
    cm_list_opt[[i]]$"Max Budget Value Basis" <- rep("MV",nrow(cm_list_opt[[i]]))
    cm_list_opt[[i]]$"Max Cost Per Value Basis" <- rep("MV",nrow(cm_list_opt[[i]]))
    
    cm_list_opt[[i]][which(cm_list[[i]][,"Max Budget Value Basis"]!=""),"Max Budget Value Basis"] <- 
      cm_list[[i]][which(cm_list[[i]][,"Max Budget Value Basis"]!=""),"Max Budget Value Basis"]
    cm_list_opt[[i]][which(cm_list[[i]][,"Max Cost Per Value Basis"]!=""),"Max Cost Per Value Basis"] <- 
      cm_list[[i]][which(cm_list[[i]][,"Max Cost Per Value Basis"]!=""),"Max Cost Per Value Basis"]
    
    ####Max budgets
    ###Fill in numeric
    if(sum(mb_type_list[[i]]=="Numeric")!=0){
      for(j in which(mb_type_list[[i]]=="Numeric")){
        ##Conversion factor
        mb_conv_den <- ifelse(cm_list[[i]][j,"Max Budget Granularity"]=="Daily",1,
                              ifelse(cm_list[[i]][j,"Max Budget Granularity"]=="Weekly",7,
                                     ifelse(cm_list[[i]][j,"Max Budget Granularity"]=="Quarterly",91,365)))
        mb_conv_num <- ifelse(granularity=="Daily",1,
                              ifelse(granularity=="Weekly",7,
                                     ifelse(granularity=="Quarterly",9,365)))
        mb_conv_fact <- mb_conv_num/mb_conv_den
        
        ##Fill in value
        cm_list_opt[[i]][j,"Max Budget"] <- mb_num_list[[i]][j]*mb_conv_fact
      }
    }
    
    ###Fill in dates
    if(sum(mb_type_list[[i]]=="Date")!=0){
      cm_list_opt[[i]][which(mb_type_list[[i]]=="Date"),"Max Budget"] <- unlist(mapply(function(ind1,ind2,mat1,mat2,dat,cm_temp_cols){
        ##Find values
        find_vals <- unlist(mat1[ind1,cm_temp_cols])
        start_date <- unlist(mat2[ind2,1])
        end_date <- unlist(mat2[ind2,2])
        row_find_text <- paste("which(",paste(paste("(if(find_vals[",
                                                    1:length(cm_temp_cols),
                                                    "]==\"All\"){rep(TRUE,nrow(dat))} else{dat[,\"",
                                                    cm_temp_cols,
                                                    "\"]==find_vals[",
                                                    1:length(cm_temp_cols),
                                                    "]})",sep=""),collapse="&"),")",sep="")
        dat <- dat[eval(parse(text=row_find_text)),,drop=F]
        
        ##If there are rows, aggregate
        if(nrow(dat)==0){return(Inf)}
        else{
          agg_cols <- which(find_vals!="All")
          agg_list <- list("Date"=dat[,"Date"])
          if(length(agg_cols)!=0){agg_list <- c(agg_list,as.list(dat[,agg_cols,drop=F]))}
          dat <- aggregate(dat[,c("MV Spend","Outcome"),drop=F],by=agg_list,FUN=sum)
          return(max(dat[which(as.Date(dat[,"Date"])>=as.Date(start_date)&as.Date(dat[,"Date"])<=as.Date(end_date)),"MV Spend"]))
        }
      },ind1=which(mb_type_list[[i]]=="Date"),ind2=1:nrow(mb_date_list[[i]]),
      MoreArgs=list(mat1=cm_list[[i]],mat2=mb_date_list[[i]],dat=dat,cm_temp_cols=cm_temp_cols),SIMPLIFY=F))
      cm_list_opt[[i]][which(mb_type_list[[i]]=="Date"),"Max Budget Value Basis"] <- "MV"
    }
    
    ####Max cost per
    ###Fill in numeric
    cm_list_opt[[i]][which(mcp_type_list[[i]]=="Numeric"),"Max Cost Per"] <- mcp_num_list[[i]][which(mcp_type_list[[i]]=="Numeric")]
    
    ###Fill in dates
    if(sum(mcp_type_list[[i]]=="Date")!=0){
      cm_list_opt[[i]][which(mcp_type_list[[i]]=="Date"),"Max Cost Per"] <- unlist(mapply(function(ind1,ind2,mat1,mat2,dat,cm_temp_cols){
        ##Find values
        find_vals <- unlist(mat1[ind1,cm_temp_cols])
        start_date <- unlist(mat2[ind2,1])
        end_date <- unlist(mat2[ind2,2])
        row_find_text <- paste("which(",paste(paste("(if(find_vals[",
                                                    1:length(cm_temp_cols),
                                                    "]==\"All\"){rep(TRUE,nrow(dat))} else{dat[,\"",
                                                    cm_temp_cols,
                                                    "\"]==find_vals[",
                                                    1:length(cm_temp_cols),
                                                    "]})",sep=""),collapse="&"),")",sep="")
        dat <- dat[eval(parse(text=row_find_text)),,drop=F]
        
        ##If there are rows, aggregate
        if(nrow(dat)==0){return(Inf)}
        else{
          agg_cols <- which(find_vals!="All")
          agg_list <- list("Date"=dat[,"Date"])
          if(length(agg_cols)!=0){agg_list <- c(agg_list,as.list(dat[,agg_cols,drop=F]))}
          dat <- aggregate(dat[,c("MV Spend","Outcome"),drop=F],by=agg_list,FUN=sum)
          if(cost_per_type==1){dat$"MV Cost Per" <- dat$"MV Spend"/dat$"MV Outcome"}
          else{dat$"MV Cost Per" <- dat$"MV Outcome"/dat$"MV Spend"}
          return(max(dat[which(as.Date(dat[,"Date"])>=as.Date(start_date)&as.Date(dat[,"Date"])<=as.Date(end_date)),"MV Cost Per"]))
        }
      },ind1=which(mcp_type_list[[i]]=="Date"),ind2=1:nrow(mcp_date_list[[i]]),
      MoreArgs=list(mat1=cm_list[[i]],mat2=mcp_date_list[[i]],dat=dat,cm_temp_cols=cm_temp_cols,cost_per_type=cost_per_type),SIMPLIFY=F))
      cm_list_opt[[i]][which(mcp_type_list[[i]]=="Date"),"Max Cost Per Value Basis"] <- "MV"
    }
  }
  
  #####Write to CSVs
  cm_folder_out <- paste(basedir,"PAs",pa,date,"Planning Inputs/Constraint Matrices/Processed",sep="/")
  if(!file.exists(cm_folder_out)){stop("Folder does not exist. Consider running the 'Create Folder' script.")}
  silence <- mapply(write.csv,cm_list_opt,gsub("Planning Inputs/Constraint Matrices/Raw",
                                               "Planning Inputs/Constraint Matrices/Processed",cm_files),MoreArgs=list(row.names=F))
}