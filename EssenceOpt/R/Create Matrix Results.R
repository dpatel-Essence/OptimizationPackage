#' Create Matrix Results
#'
#' This function prepares all optimizations for plotting
#' @param df1 b_mv dataset
#' @param df2 b dataset
#' @param df3 o dataset
#' 
#' @keywords Create Matrix results
#' @export
#' @examples
#' Create_all_results(df1,df2,df3)


#library(data.table)
#library(reshape2)
Create_Matrix_Results = function(df1 = b_mv,df2 = b,df3 = o) {
  #convert cost to numeric
  df1[,1] = as.numeric(substr(df1[,1],2,nchar(df1[,1])-1)) * 1e6
  df2[,1] = as.numeric(substr(df2[,1],2,nchar(df2[,1])-1)) * 1e6
  df3[,1] = as.numeric(substr(df3[,1],2,nchar(df3[,1])-1)) * 1e6
  
  names(df1)[1] = 'market_value_cum'
  names(df2)[1] = 'basis_cum'
  names(df3)[1] = 'outcome_cum'
  #convert all to numeric
  df1[,-1] = sapply( df1[,-1], as.numeric )
  df2[,-1] = sapply(df2[,-1], as.numeric )
  df3[,-1] = sapply(df3[,-1], as.numeric )
  
  m_b_mv = reshape2::melt(as.data.table(df1), id = c("market_value_cum"), variable.name = 'Segment', value.name = 'market_value')
  m_b = reshape2::melt(as.data.table(df2), id = c("basis_cum"), variable.name = 'Segment', value.name = 'basis')
  m_o = reshape2::melt(as.data.table(df3), id = c("outcome_cum"), variable.name = 'Segment', value.name = 'outcome')
  
  all_results = m_b_mv
  #Add budget
  all_results$basis_cum = m_b$basis_cum
  all_results$basis = m_b$basis
  all_results$basis_segment = m_b$Segment
  #Add outcome    
  all_results$outcome_cum = m_o$outcome_cum
  all_results$outcome = m_o$outcome
  all_results$outcome_segment = m_o$Segment
  return((all_results))
}   