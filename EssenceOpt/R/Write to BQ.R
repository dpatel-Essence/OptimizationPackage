#' Write to BQ Function
#'
#' This function write output to BQ after checking to make sure the table exists
#' @param project_id,dataset,table,data Used in checking
#' @param CheckExamples for Write to BQ params
#' 
#' @keywords plot curves
#' @export
#' @examples
#' write_to_bq(pa, date, basedir, scenario, table_name, 
#' dataset = 'interim_solution_forecasting', 
#' project_id = 'essence-analytics-dwh',
#' optimization = T,
#' curves = F,
#' pa_bq = "")
#' 
#' 
#' 

check_and_write_bq <- function(project_id, dataset, table, data){
  
  ds = bq_table(project_id, dataset, table)
  if(bq_table_exists(ds)){
    bq_table_delete(ds)
  }
  bq_tb <- bq_table_create(
    ds,
    data
  )
  bq_table_upload(bq_tb, data)
  
}

write_to_bq <- function(pa, date, basedir, scenario, table_name, 
                        dataset = 'interim_solution_forecasting', 
                        project_id = 'essence-analytics-dwh',
                        optimization = T,
                        curves = F,
                        pa_bq = ""){
  
  library(bigrquery)
  library(stringr)
  
  if(pa_bq == ""){
    pa_bq = pa
  }
  
  if(!str_detect(table_name, regex("^[A-Za-z0-9_]*$", ignore_case = TRUE))){stop("Table name must letters numbers and underscores only")}
  
  if(optimization){
    #read in files
    b_mv <- read_csv(paste(basedir,"/PAs/",pa,"/",date,"/Optimal Mixes/Budgets (MV)/",scenario,".csv",sep=""),row.names=NULL)
    b <- read_csv(paste(basedir,"/PAs/",pa,"/",date,"/Optimal Mixes/Budgets/",scenario,".csv",sep=""),row.names=NULL)
    o <- read_csv(paste(basedir,"/PAs/",pa,"/",date,"/Optimal Mixes/Outcomes/",scenario,".csv",sep=""),row.names=NULL)
    
    #convert cost to numeric
    b_mv[,1] = as.numeric(substr(b_mv[,1],2,nchar(b_mv[,1])-1)) * 1e6
    b[,1] = as.numeric(substr(b[,1],2,nchar(b[,1])-1)) * 1e6
    o[,1] = as.numeric(substr(o[,1],2,nchar(o[,1])-1)) * 1e6
    
    names(b_mv)[1] = 'market_value_cum'
    names(b)[1] = 'basis_cum'
    names(o)[1] = 'outcome_cum'
    
    #convert all to numeric
    b_mv[,-1] = sapply( b_mv[,-1], as.numeric )
    b[,-1] = sapply(b[,-1], as.numeric )
    o[,-1] = sapply(o[,-1], as.numeric )
    
    
    # library(reshape)
    m_b_mv = melt(as.data.table(b_mv), id = c("market_value_cum"), variable.name = 'Segment', value.name = 'market_value')
    m_b = melt(as.data.table(b), id = c("basis_cum"), variable.name = 'Segment', value.name = 'basis')
    m_o = melt(as.data.table(o), id = c("outcome_cum"), variable.name = 'Segment', value.name = 'outcome')
    
    all_results = m_b_mv
    #Add budget
    all_results$basis_cum = m_b$basis_cum
    all_results$basis = m_b$basis
    all_results$basis_segment = m_b$Segment
    #Add outcome    
    all_results$outcome_cum = m_o$outcome_cum
    all_results$outcome = m_o$outcome
    all_results$outcome_segment = m_o$Segment
    
    if(!all((all_results$Segment == all_results$basis_segment) & (all_results$Segment == all_results$outcome_segment))){
      stop("Segments do not match")}
    
    all_results = all_results[, c(2,1,3,4,5,7,8)]
    
         
    check_and_write_bq(project_id, dataset, gsub(' ', '_', paste("results",pa_bq,table_name,sep="_")), all_results)
    # check_and_write_bq(project_id, dataset, paste("mv",pa,table_name,sep="_"), m_b_mv)
    # check_and_write_bq(project_id, dataset, paste("basis",pa,table_name,sep="_"), m_b)
    # check_and_write_bq(project_id, dataset, paste("outcome",pa,table_name,sep="_"), m_o)
  }
  
  if(curves){
    rcs <- read_csv(paste(basedir,"/PAs/",pa,"/",date,"/Data/Processed/Response Curve Summary.csv",sep=""),row.names=NULL)
    names(rcs)[1] <- "Segment"
    rcs_upload <- rcs[,c("Segment", "Power", "Coefficient", "AIC", "Lower Bound", "Upper Bound")]
    names(rcs_upload)[c(5,6)] <- c("Lower_Bound", "Upper_Bound")
    check_and_write_bq(project_id, dataset, gsub(' ', '_', paste("curves",pa_bq,table_name,sep="_")), rcs_upload)
  }
  
}
