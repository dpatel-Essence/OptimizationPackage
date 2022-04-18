#' Process Data Function for EMEA Daily Sales
#'
#' This function process provided raw data
#' @param basedir,pa,date Location of directory
#' @param process_data_fun processed data
#' @param attr = 'LC' or MTA 
#' @param file defaults to DSM_EMEA_Daily Sales Data_working_Q4_2020_to_Q4_2021.csv"
#' 
#' @keywords Create all results
#' @export
#' @examples
#' process_data_dsm(basedir,pa,date,process=T, attr = 'LC', file ="Data/Raw/DSM_EMEA_Daily Sales Data_working_Q4_2020_to_Q4_2021.csv")

process_data_dsm <- function(basedir,pa,date,process=T, attr = 'LC', file ="Data/Raw/DSM_EMEA_Daily Sales Data_working_Q4_2020_to_Q4_2021.csv"){
  #####Process data if desired
  if(process){
    library(dplyr) # for functions
    
    ####Read in data file
    dat <- read_csv(paste(basedir,"PAs",pa,date,file,sep="/"))
    dat[,'Date'] = as.Date(as.character(dat[,'date']), tryFormats = c("%m/%d/%Y"))
    
    #rename columns
    colnames(dat)[which(colnames(dat)=="Spend_MV")] <- "Spend"
    colnames(dat)[which(colnames(dat)=="Product_Family_Sold")] <- "Product Family"
    if (attr == 'LC'){
      colnames(dat)[which(colnames(dat)=="Unit_Sold")] <- "Sales"
    } else {
      colnames(dat)[which(colnames(dat)=="MTA Sales_Daily")] <- "Sales"
    }
    
    # dat[(dat[,'Product Family'] %in% c('Cameras', 'Connectivity', 'Displays','Locks','Protect','Speakers','Streaming', 'Thermostats')),'Product Family'] <- 'Total Home'
    # dat[(dat[,'Product Family'] %in% c('Pixel 5')),'Product Family'] <- 'Pixel 5a'    
    
    ####there is no unique identifier so have to create it 
    dat <- aggregate(dat[,c("Spend",	"Sales"),drop=F],
                     by=list("Date"=dat[,"Date"],
                             "Product Family"=dat[,"Product Family"],
                             "Channel"=dat[,"Channel"],
                             "Type"=dat[,"Type"]
                     )
                     ,FUN=sum,na.rm=T)
    
    #### no concierge
    dat = dat[dat['Product Family']!='Concierge',]
    dat = dat[dat['Product Family']!='Subscriptions',]
    ###drop country = 'other'
    ###drop product family = 'other'
    dat = dat[dat['Product Family']!='Other',]
    #drop channel = 'other'
    dat = dat[dat['Channel']!='Other',]
    ####First, a check
    if(nrow(unique(dat[,c("Date","Product Family","Channel", "Type")]))!=nrow(dat)){
      stop("Rows are not unique.")
    }
    
    ####Add cost per
    dat$"MV Cost Per" <- dat$"Spend"/dat$"Sales"
    
    ####Add segment
    dat$"Segment" <- paste(dat[,"Product Family"],dat[,"Channel"],dat[,"Type"],sep=" - ")
    
    
    # ####Limit dates
    # dat[,"date"] <- substring(dat[,"date"],1,10)
    dat <- dat[which(as.Date(dat[,"Date"])>=as.Date("2020-10-01")&as.Date(dat[,"Date"])<=as.Date("2021-09-30")),,drop=F]
    # 
    # ####Rename columns
    # colnames(dat)[which(colnames(dat)%in%c("market_value_cost","acquisitions","revenue_182d_rand"))] <- 
    #   c("MV Spend","Acquisitions","Outcome")
    
    
    ####Remove rows with zero spend or zero revenue
    dat <- dat[which(dat[,"MV Cost Per"]!=0),,drop=F]
    
    colnames(dat)[which(colnames(dat)=="Spend")] <- "MV Spend"
    colnames(dat)[which(colnames(dat)=="Sales")] <- "Outcome"
    
    
    ###Reorder
    dat <- dat[order(dat[,"Segment"]),,drop=F]
    
    
  if(FALSE){  #this is some pretty specific logic to combine types if there isn't enough data
    agg1 <- aggregate(dat[,c("Outcome"),drop=F],by=list("Product Family"=dat[,"Product Family"],
                                                        "Channel"=dat[,"Channel"],
                                                        "Type"=dat[,"Type"]),
                      FUN=function(vec){length(unique(vec))})
    agg2 <- aggregate(dat[,c("MV Spend"),drop=F],by=list("Product Family"=dat[,"Product Family"],
                                                         "Channel"=dat[,"Channel"],
                                                         "Type"=dat[,"Type"]),
                      FUN=function(vec){sum(is.finite(vec))})
    agg <- merge(agg1,agg2,by=c("Product Family","Channel", "Type"),all=T)
    
    agg$meets_minimums <- 1*(agg$Outcome > 3 &  agg$'MV Spend' > 3)
    
    min_ratio <- aggregate(agg[,c("meets_minimums"),drop=F],by=list("Product Family"=agg[,"Product Family"],
                                                                    "Channel"=agg[,"Channel"]),
                           FUN=function(vec){mean(vec)})
    colnames(min_ratio)[which(colnames(min_ratio)=="meets_minimums")] <- "Ratio"
    
    
    comb_count <- aggregate(agg[,c("meets_minimums"),drop=F],by=list("Product Family"=agg[,"Product Family"],
                                                                     "Channel"=agg[,"Channel"]),
                            FUN=function(vec){length(vec)})
    colnames(comb_count)[which(colnames(comb_count)=="meets_minimums")] <- "Count"
    to_combine <- merge(min_ratio,comb_count,by=c("Product Family","Channel"),all=T)
    to_combine <- to_combine[to_combine$Ratio < 1 & to_combine$Count > 1,]
    
    #write the combined data to a csv
    types_combined <- merge(to_combine, agg, by=c("Product Family","Channel"))
    write.csv(types_combined,paste(basedir,"PAs",pa,date,"Data/Processed/Combined Data.csv",sep="/"),row.names=F)
  
    for(i in 1:length(to_combine[,1])){
      dat[dat$`Product Family` == to_combine[i, 'Product Family'] &
            dat$`Country` == to_combine[i, 'Country'] &
            dat$`Channel` == to_combine[i, 'Channel'], 'Type'] <- 'Combined'
    }
    
  }    
    
    
    dat <- aggregate(dat[,c("MV Spend",	"Outcome"),drop=F],
                     by=list("Date"=dat[,"Date"],
                             "Product Family"=dat[,"Product Family"],
                             "Channel"=dat[,"Channel"],
                             "Type"=dat[,"Type"]
                     )
                     ,FUN=sum,na.rm=T)
    
    ####Add cost per
    dat$"MV Cost Per" <- dat$"MV Spend"/dat$"Outcome"
    
    ####Add segment
    dat$"Segment" <- paste(dat[,"Product Family"],dat[,"Channel"],dat[,"Type"],sep=" - ")
    
    outliers <- detect_and_remove_outliers(dat)
    diag_mat <- outliers$diag_mat
    dat <- outliers$dat
    
    #remove any segments which do not have enough diversity in x or y
    agg1 <- aggregate(dat[,c("Outcome"),drop=F],by=list("Product Family"=dat[,"Product Family"],
                                                        "Channel"=dat[,"Channel"],
                                                        "Type"=dat[,"Type"]),
                      FUN=function(vec){length(unique(vec))})
    agg2 <- aggregate(dat[,c("MV Spend"),drop=F],by=list("Product Family"=dat[,"Product Family"],
                                                         "Channel"=dat[,"Channel"],
                                                         "Type"=dat[,"Type"]),
                      FUN=function(vec){sum(is.finite(vec))})
    agg <- merge(agg1,agg2,by=c("Product Family","Channel", "Type"),all=T)
    agg$"Segment" <- paste(agg[,"Product Family"],agg[,"Channel"],agg[,"Type"],sep=" - ")
    
    agg$meets_minimums <- 1*(agg$Outcome > 3 &  agg$'MV Spend' > 3)
    rm_segs = agg[which(!agg$meets_minimums),'Segment']
    dat <- dat[which(!dat[,"Segment"]%in%rm_segs),,drop=F]
    
    ####Remove any segments which have fewer than x data points; custom to EMEA tranche: do not remove Great Britain TrueView
    min_points = 20
    rm_segs <- aggregate(dat[,"Outcome",drop=F],by=list("Segment"=dat[,"Segment"]),FUN=length)
    # rm_segs <- rm_segs[which(rm_segs[,"Outcome"]<=100&rm_segs[,"Segment"]!="Great Britain - GDN - TrueView"),"Segment"]
    rm_segs_copy <- rm_segs
    rm_segs <- rm_segs[which(rm_segs[,"Outcome"]<=min_points),'Segment']
    
    #write the dropped data to a csv
    write.csv(rm_segs_copy[rm_segs_copy[,"Outcome"]<=min_points,],
              paste(basedir,"PAs",pa,date,"Data/Processed/Dropped Data.csv",sep="/"),row.names=F)
    
    dat <- dat[which(!dat[,"Segment"]%in%rm_segs),,drop=F]
    
    # add curves that weren't added before
    # not sure what this is doing 
    # cuts <- read_csv(paste(basedir,"PAs",pa,date,"Planning Inputs/Rate Matrices/v1.csv",sep="/"))
    # cuts <- cuts[,c("Product Family","Country","Channel","Type")]
    # cuts_in_data <- distinct(dat[,c("Product Family","Channel", "Country", "Type")])
    # missing_cuts <- anti_join(cuts, cuts_in_data, by = c("Product Family","Channel", "Country", "Type"))
    # missing_cuts$"Segment" <- paste(missing_cuts[,"Product Family"],missing_cuts[,"Country"],missing_cuts[,"Channel"],missing_cuts[,"Type"],sep=" - ")
    # missing_cuts$"Date" <-  as.Date("2021-01-01")
    # missing_cuts$"MV Spend" <-  10
    # missing_cuts$"Outcome" <- 0
    # missing_cuts$"MV Cost Per" <- 0
    # missing_cuts <- missing_cuts[,c(names(dat))]
    # 
    # dat <- rbind(dat, missing_cuts)
    
    ###Reorder
    dat <- dat[order(dat[,"Segment"]),,drop=F]
    
    ####Write to CSV and return
    write.csv(dat,paste(basedir,"PAs",pa,date,"Data/Processed/Processed Data.csv",sep="/"),row.names=F)
    write.csv(diag_mat,paste(basedir,"PAs",pa,date,"Data/Processed/Outlier Summary.csv",sep="/"))
    
    
  }
  #####Otherwise, return the granularity
  else{
    return("Daily")
  }
}