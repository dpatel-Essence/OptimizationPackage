#' A Modified Plot_Curves Function
#'
#' This function allows you to plot curves even when the original plot curves doesn't work
#' @param basedir Location of Base Directory
#' @param pa,date,ms_name,out_name,mcp_name,process_data_fun,cost_per_type additional parameters
#' 
#' @keywords plot curves
#' @export
#' @examples
#' plot_curves2()

plot_curves2 = function(basedir,pa,date,ms_name,out_name,mcp_name,process_data_fun,cost_per_type=1){
  #####Check and read in response curve summary and data file. If former exists, then data has been checked
  diag_mat_file <- paste(basedir,"PAs",pa,date,"Data/Processed/Response Curve Summary.csv",sep="/")
  if(!file.exists(diag_mat_file)){stop("Response curves have not been generated. Consider running the 'Build Response Curves' script.")}
  diag_mat <- read_csv(diag_mat_file,row.names=1)
  
  data_file <- paste(basedir,"PAs",pa,date,"Data/Processed/Processed Data.csv",sep="/")
  dat <- read_csv(data_file)
  
  #####Check names and cost per type
  if(length(ms_name)!=1){stop("MV spend name must be a single character value.")}
  if(!is.character(ms_name)){stop("MV spend name must be a single character value.")}
  if(length(out_name)!=1){stop("Outcome name must be a single character value.")}
  if(!is.character(out_name)){stop("Outcome name must be a single character value.")}
  if(length(mcp_name)!=1){stop("MV cost per name must be a single character value.")}
  if(!is.character(mcp_name)){stop("MV cost name must be a single character value.")}
  if(length(cost_per_type)!=1){stop("Cost per type must be a 1 or 2.")}
  if(!is.numeric(cost_per_type)){stop("Cost per type must be a 1 or 2.")}
  if(!cost_per_type%in%c(1,2)){stop("Cost per type must be a 1 or 2.")}
  
  #####Check process data function and output
  if(length(process_data_fun)!=1){stop("'Process Data' function must be a single function found in the environment.")}
  if(!is.character(process_data_fun)){stop("'Process Data' function must be a single function found in the environment.")}
  if(!exists(process_data_fun)){stop("'Process Data' function must be a single function found in the environment.")}
  
  gran_vals <- c("Daily","Weekly","Quarterly","Yearly")
  granularity <- do.call(process_data_fun,list("basedir"=basedir,"pa"=pa,"date"=date,process=F))
  if(length(granularity)!=1){stop("Data granularity does not match standard values. Consider updating the 'Process Data' script.")}
  if(!is.character(granularity)){stop("Data granularity does not match standard values. Consider updating the 'Process Data' script.")}
  if(!granularity%in%gran_vals){stop("Data granularity does not match standard values. Consider updating the 'Process Data' script.")}
  
  ####Check folders to store curves
  rc_folder <- paste(basedir,"PAs",pa,date,"Graphics/Response Curves",sep="/")
  if(!file.exists(rc_folder)){stop("Folder does not exist. Consider running the 'Create Folder' script.")}
  cpc_folder <- paste(basedir,"PAs",pa,date,"Graphics/Cost Per Curves",sep="/")
  if(!file.exists(cpc_folder)){stop("Folder does not exist. Consider running the 'Create Folder' script.")}
  
  #####Get usegs from row names
  usegs <- rownames(diag_mat)
  
  #####Now plot response curves and cost per curves
  dat <- dat[which(!is.infinite(dat[,"MV Cost Per"])),,drop=F]
  for(i in 1:length(usegs)){
    ####Subset
    subset <- dat[which(dat[,"Segment"]==usegs[i]),,drop=F]
    
    ####Determine divisors
    poss_vals <- 10^seq(0,12,by=3)
    poss_vals_app_ms <- paste(" ($",c(""," Thousands"," Millions"," Billions"," Trillions"),")",sep="")
    poss_vals_app_out <- c("",paste(" (",c("Thousands","Millions","Billions","Trillions"),")",sep=""))
    
    if(max(pretty(subset[,"MV Spend"]))<1){div_ms_ind <- 1}
    else{div_ms_ind <- which(max(pretty(subset[,"MV Spend"]))/poss_vals<1000&max(pretty(subset[,"MV Spend"]))/poss_vals>=1)}
    if(max(pretty(subset[,"Outcome"]))<1){div_out_ind <- 1}
    else{div_out_ind <- which(max(pretty(subset[,"Outcome"]))/poss_vals<1000&max(pretty(subset[,"Outcome"]))/poss_vals>=1)}
    div_ms <- poss_vals[div_ms_ind]
    div_out <- poss_vals[div_out_ind]
    
    full_ms_name <- paste(granularity," MV ",ms_name,poss_vals_app_ms[div_ms_ind],sep="")
    full_out_name <- paste(granularity," ",out_name,poss_vals_app_out[div_out_ind],sep="")
    full_mcp_name <- paste(granularity," MV ",mcp_name," ($)",sep="")
    
    ####Define actual values
    act_ms <- c(0,subset[,"MV Spend"]/div_ms)
    act_out <- c(0,subset[,"Outcome"]/div_out)
    if(cost_per_type==1){
      act_mcp <- c(0,ifelse(is.nan(subset[,"MV Spend"]/subset[,"Outcome"]),0,
                            subset[,"MV Spend"]/subset[,"Outcome"]))
    }
    else{
      act_mcp <- c(0,ifelse(is.nan(subset[,"Outcome"]/subset[,"MV Spend"]),0,
                            subset[,"Outcome"]/subset[,"MV Spend"]))
    }
    
    ####Define line values
    line_ms <- seq(0,max(subset[,"MV Spend"])*1.1,length.out=1000)
    line_out <- diag_mat[i,"Coefficient"]*(line_ms^diag_mat[i,"Power"])
    if(cost_per_type==1){
      line_mcp <- ifelse(is.nan(line_ms/line_out),0,line_ms/line_out)
    }
    else{
      line_mcp <- ifelse(is.nan(line_out/line_ms),0,line_out/line_ms)
    }
    line_ms <- line_ms/div_ms
    line_out <- line_out/div_out
    
    ####Response Curve
    png(paste(basedir,"/PAs/",pa,"/",date,"/Graphics/Response Curves/",usegs[i],".PNG",sep=""),
        height=480*1.5*1.5,width=480*1.5*1.5)
    par(mar=c(12,12,8,8),family="Arial")
    plot(act_ms,act_out,xlab="",ylab="",col="black",las=1,cex.axis=2,lwd=3,
         font=1,cex.main=3,font.main=1,xaxt="n",yaxt="n",pch=19)
    axis(1,pretty(act_ms),labels=pretty(act_ms),las=1,cex.axis=2)
    axis(2,at=pretty(act_out),labels=pretty(act_out),las=1,cex.axis=2)
    mtext(text=full_out_name,side=2,cex=1.9,line=7,font=1)
    mtext(text=full_ms_name,side=1,cex=1.9,line=7,font=1)
    lines(line_ms,line_out,lwd=3,col="red")
    x <- dev.off()
    
    ####Cost Per Curve
    png(paste(basedir,"/PAs/",pa,"/",date,"/Graphics/Cost Per Curves/",usegs[i],".PNG",sep=""),
        height=480*1.5*1.5,width=480*1.5*1.5)
    par(mar=c(12,12,8,8),family="Arial")
    plot(act_ms,act_mcp,xlab="",ylab="",col="black",las=1,cex.axis=2,lwd=3,
         font=1,cex.main=3,font.main=1,xaxt="n",yaxt="n",pch=19)
    axis(1,at=pretty(act_ms),labels=pretty(act_ms),las=1,cex.axis=2)
    axis(2,at=pretty(act_mcp),labels=pretty(act_mcp),las=1,cex.axis=2)
    mtext(text=full_mcp_name,side=2,cex=1.9,line=7,font=1)
    mtext(text=full_ms_name,side=1,cex=1.9,line=7,font=1)
    lines(line_ms,line_mcp,lwd=3,col="red")
    x <- dev.off()
    
    
    
    
  }
}