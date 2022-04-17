#' Detect and Remove Outliers
#' 
#' 
#' This function detects and removes outliers in a separate file
#' @param dat processed data
#' 
#' @keywords outlier removal
#' @export
#' @examples
#' detect_and_remove_outliers()


detect_and_remove_outliers <- function(dat){
  #####Check data frame
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
  
  #####Cycle through segments and remove outliers
  ####Remove infinite cost pers & initiate matrix
  dat <- dat[which(!is.infinite(dat[,"MV Cost Per"])),,drop=F]
  usegs <- sort(unique(dat[,"Segment"]))
  diag_mat <- matrix(nrow=length(usegs),ncol=3,dimnames=list(usegs,c("Total Rows","Final Rows","Keep Rate")))
  dat_list <- vector("list",length(usegs))
  
  ####Loop through
  for(i in 1:length(usegs)){
    ####Subset
    subset <- dat[which(dat[,"Segment"]==usegs[i]),,drop=F]
    diag_mat[i,"Total Rows"] <- nrow(subset)
    
    ####Determine outliers and re-subset
    out1 <- boxplot(subset[,"MV Spend"],plot=F)$out
    out2 <- boxplot(subset[,"Outcome"],plot=F)$out
    out3 <- boxplot(subset[,"MV Cost Per"],plot=F)$out
    subset <- subset[which(!(round(subset[,"MV Spend"],1)%in%round(out1,1)|
                               round(subset[,"Outcome"],1)%in%round(out2,1)|
                               round(subset[,"MV Cost Per"],1)%in%round(out3,1))),,drop=F]
    diag_mat[i,"Final Rows"] <- nrow(subset)
    diag_mat[i,"Keep Rate"] <- 100*(diag_mat[i,"Final Rows"]/diag_mat[i,"Total Rows"])
    dat_list[[i]] <- subset
  }
  
  ####Rbind
  dat <- do.call("rbind",dat_list)
  
  #####Return
  return(list(dat=dat,diag_mat=diag_mat))
}