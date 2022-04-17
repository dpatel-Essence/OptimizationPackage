#' Build Response Curves
#'
#' This Function reads in csv's with particular settings
#' @param basedir,pa,date Location of directory
#' @param powers default from 0.1 to 0.9 by 0.01 - list of powers to test
#' @param method rlm is default
#' 
#' 
#' @keywords Read CSV
#' @export
#' @examples
#' build_response_curves(basedir,pa,date,powers=seq(0.3,0.9,by=0.01))

#suppressMessages(library(MASS))
build_response_curves <- function(basedir,pa,date,powers=seq(0.1,0.9,by=0.01),method="rlm"){
  #####Check and read in data file
  ####File and folder
  data_file <- paste(basedir,"PAs",pa,date,"Data/Processed/Processed Data.csv",sep="/")
  if(!file.exists(data_file)){stop("Data does not exist. Consider running the 'Process Data' script.")}
  dat <- read_csv(data_file)
  
  ####Check data frame
  req_colnames <- c("Segment","MV Spend","Outcome","MV Cost Per")
  if(is.null(dim(dat))){stop("Data must be a data frame.")}
  if(sum(req_colnames%in%colnames(dat))!=length(req_colnames)){
    stop(paste("Data must contain the following column names: ",
               paste(paste("'",req_colnames,"'",sep=""),collapse=", "),".",sep=""))
  }
  for(i in req_colnames){
    if(sum(is.na(dat[,i]))!=0){
      paste("Remove 'NA' values from the '",i,"' column.",sep="")
    }
  }
  
  #####Check and read in rate template
  rate_template_file <- paste(basedir,"PAs",pa,date,"Planning Inputs/Template Output/Rate Template.csv",sep="/")
  if(!file.exists(rate_template_file)){stop("Rate template does not exist. Consider running the 'Create Templates' script.")}
  rate_template <- read_csv(rate_template_file)
  rate_template <- rate_template[,which(!colnames(rate_template)%in%c("CV %","Plan Name","Fee Schedule")),drop=F]
  
  #####Derive segments and build response curves
  dat <- dat[which(!is.infinite(dat[,"MV Cost Per"])),,drop=F]
  usegs <- sort(unique(dat[,"Segment"]))
  diag_mat <- matrix(nrow=length(usegs),ncol=5,dimnames=list(usegs,c("Power","Coefficient","AIC","Lower Bound","Upper Bound")))
  
  ####Loop through
  seg_temp_list <- vector("list",length(usegs))
  for(i in 1:length(usegs)){
    ####Subset; ensure segment is unique along the column names of the template and extract values
    subset <- dat[which(dat[,"Segment"]==usegs[i]),,drop=F]
    if(nrow(unique(subset[,colnames(rate_template),drop=F]))!=1){
      stop("Segments do not correspond to the planning inputs. Consider updating the 'Process Data' script.")
    }
    seg_temp_list[[i]] <- unique(subset[,colnames(rate_template),drop=F])[1,]
    
    ####Get best fit
    ###Get unique combinations only
    cost_conv <- subset[,c("MV Spend","Outcome")]
    cost_conv <- unique(cost_conv)
    cost <- cost_conv[,1]
    conv <- cost_conv[,2]
    
    ###Perform regressions
    fits <- lapply(powers,function(power,cost,conv,method){
      cost_p <- cost^power
      options(warn=-1)
      if(method=="rlm"){fit <- rlm(conv~0+cost_p,maxit=100)}
      if(method=="lm"){fit <- lm(conv~0+cost_p)}
      options(warn=0)
      return(list(aic=AIC(fit),coef=fit$coefficients,conf=confint.default(fit,level=0.999)))
    },cost=cost,conv=conv,method=method)
    
    ###Get best AIC
    best_ind <- which.min(unlist(lapply(fits,function(lst){lst$aic})))
    
    ###Get power, coef, and AIC
    diag_mat[i,"Power"] <- powers[best_ind]
    diag_mat[i,"Coefficient"] <- unname(fits[[best_ind]]$coef)
    diag_mat[i,"AIC"] <- unname(fits[[best_ind]]$aic)
    diag_mat[i,c("Lower Bound","Upper Bound")] <- unname(fits[[best_ind]]$conf[1,])
  }
  
  #####Bind segment template columns
  seg_temp <- do.call("rbind",seg_temp_list)
  colnames(seg_temp) <- colnames(rate_template)
  diag_mat <- cbind(data.frame(diag_mat,check.names=F),seg_temp)
  
  #####Output to CSV
  out_folder <- paste(basedir,"PAs",pa,date,"Data/Processed",sep="/")
  if(!file.exists(out_folder)){stop("Folder does not exist. Consider running the 'Create Folder' script.")}
  write.csv(diag_mat,paste(basedir,"PAs",pa,date,"Data/Processed/Response Curve Summary.csv",sep="/"))
}