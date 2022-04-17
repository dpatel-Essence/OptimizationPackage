#' A Grouping function for Plotting
#'
#' 
#' @param y melted dataset to be grouped for plotting
#' 
#' @keywords grouping
#' @export
#' @examples
#' Grouping_function()

#library(dplyr)
Grouping_function = 
  function(y){
    y %>% 
      group_by(Segment) %>% 
      select(Segment,market_value,basis,outcome) %>% 
      summarise_at(vars(-group_cols()),max) %>% 
      arrange(Segment)%>%
      mutate(CPS = basis/outcome) %>% as.data.frame()
  }
