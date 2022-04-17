#' Clear for Rerun
#'
#' Ensures a rerun is possible
#' @param basedir,pa,date Location of directory
#' 
#' 
#' @keywords clear_for_rerun
#' @export
#' @examples
#' clear_for_rerun(basedir,pa,date)


clear_for_rerun <- function(basedir,pa,date){
  #####Define folders to clear
  folders <- c(paste(basedir,"PAs",pa,date,"Data/Processed",sep="/"),
               paste(basedir,"PAs",pa,date,"Graphics/Cost Per Curves",sep="/"),
               paste(basedir,"PAs",pa,date,"Graphics/Response Curves",sep="/"),
               paste(basedir,"PAs",pa,date,"Optimal Mixes/Budgets",sep="/"),
               paste(basedir,"PAs",pa,date,"Optimal Mixes/Budgets (MV)",sep="/"),
               paste(basedir,"PAs",pa,date,"Optimal Mixes/Cost Pers",sep="/"),
               paste(basedir,"PAs",pa,date,"Optimal Mixes/Outcomes",sep="/"),
               paste(basedir,"PAs",pa,date,"Planning Inputs/Constraint Matrices/Processed",sep="/"),
               paste(basedir,"PAs",pa,date,"Planning Inputs/Template Output",sep="/"))
  
  #####Loop through and clear
  for(i in folders){
    if(!file.exists(i)){stop("Folder does not exist. Consider running the 'Create Folder' script.")}
    files <- list.files(i,full.names=T)
    if(length(files)!=0){
      z <- file.remove(files)
    }
  }
}