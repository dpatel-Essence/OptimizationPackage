#' Create Folder
#'
#' Creates folders for optimizations
#' @param basedir,pa,date Location of directory
#' 
#' 
#' @keywords creates folders
#' @export
#' @examples
#' create_folder(basedir,pa,date)


create_folder <- function(basedir,pa,date){
  #####Create PA folder, if it doesn't exist
  if(!file.exists(paste(basedir,"PAs",pa,sep="/"))){dir.create(paste(basedir,"PAs",pa,sep="/"))}
  
  #####Create date folder, if it doesn't exist
  if(!file.exists(paste(basedir,"PAs",pa,date,sep="/"))){dir.create(paste(basedir,"PAs",pa,date,sep="/"))}
  
  #####Create subfolders, if they don't exist
  ####Data
  if(!file.exists(paste(basedir,"PAs",pa,date,"Data",sep="/"))){
    dir.create(paste(basedir,"PAs",pa,date,"Data",sep="/"))
  }
  if(!file.exists(paste(basedir,"PAs",pa,date,"Data/Processed",sep="/"))){
    dir.create(paste(basedir,"PAs",pa,date,"Data/Processed",sep="/"))
  }
  if(!file.exists(paste(basedir,"PAs",pa,date,"Data/Raw",sep="/"))){
    dir.create(paste(basedir,"PAs",pa,date,"Data/Raw",sep="/"))
  }
  
  ####Graphics
  if(!file.exists(paste(basedir,"PAs",pa,date,"Graphics",sep="/"))){
    dir.create(paste(basedir,"PAs",pa,date,"Graphics",sep="/"))
  }
  if(!file.exists(paste(basedir,"PAs",pa,date,"Graphics/Cost Per Curves",sep="/"))){
    dir.create(paste(basedir,"PAs",pa,date,"Graphics/Cost Per Curves",sep="/"))
  }
  if(!file.exists(paste(basedir,"PAs",pa,date,"Graphics/Response Curves",sep="/"))){
    dir.create(paste(basedir,"PAs",pa,date,"Graphics/Response Curves",sep="/"))
  }
  
  ####Optimal Mixes
  if(!file.exists(paste(basedir,"PAs",pa,date,"Optimal Mixes",sep="/"))){
    dir.create(paste(basedir,"PAs",pa,date,"Optimal Mixes",sep="/"))
  }
  if(!file.exists(paste(basedir,"PAs",pa,date,"Optimal Mixes/Budgets",sep="/"))){
    dir.create(paste(basedir,"PAs",pa,date,"Optimal Mixes/Budgets",sep="/"))
  }
  if(!file.exists(paste(basedir,"PAs",pa,date,"Optimal Mixes/Budgets (MV)",sep="/"))){
    dir.create(paste(basedir,"PAs",pa,date,"Optimal Mixes/Budgets (MV)",sep="/"))
  }
  if(!file.exists(paste(basedir,"PAs",pa,date,"Optimal Mixes/Cost Pers",sep="/"))){
    dir.create(paste(basedir,"PAs",pa,date,"Optimal Mixes/Cost Pers",sep="/"))
  }
  if(!file.exists(paste(basedir,"PAs",pa,date,"Optimal Mixes/Outcomes",sep="/"))){
    dir.create(paste(basedir,"PAs",pa,date,"Optimal Mixes/Outcomes",sep="/"))
  }
  
  ####Planning Inputs
  if(!file.exists(paste(basedir,"PAs",pa,date,"Planning Inputs",sep="/"))){
    dir.create(paste(basedir,"PAs",pa,date,"Planning Inputs",sep="/"))
  }
  if(!file.exists(paste(basedir,"PAs",pa,date,"Planning Inputs/Constraint Matrices",sep="/"))){
    dir.create(paste(basedir,"PAs",pa,date,"Planning Inputs/Constraint Matrices",sep="/"))
  }
  if(!file.exists(paste(basedir,"PAs",pa,date,"Planning Inputs/Constraint Matrices/Raw",sep="/"))){
    dir.create(paste(basedir,"PAs",pa,date,"Planning Inputs/Constraint Matrices/Raw",sep="/"))
  }
  if(!file.exists(paste(basedir,"PAs",pa,date,"Planning Inputs/Constraint Matrices/Processed",sep="/"))){
    dir.create(paste(basedir,"PAs",pa,date,"Planning Inputs/Constraint Matrices/Processed",sep="/"))
  }
  if(!file.exists(paste(basedir,"PAs",pa,date,"Planning Inputs/Fee Schedules",sep="/"))){
    dir.create(paste(basedir,"PAs",pa,date,"Planning Inputs/Fee Schedules",sep="/"))
  }
  if(!file.exists(paste(basedir,"PAs",pa,date,"Planning Inputs/Rate Matrices",sep="/"))){
    dir.create(paste(basedir,"PAs",pa,date,"Planning Inputs/Rate Matrices",sep="/"))
  }
  if(!file.exists(paste(basedir,"PAs",pa,date,"Planning Inputs/Template Input",sep="/"))){
    dir.create(paste(basedir,"PAs",pa,date,"Planning Inputs/Template Input",sep="/"))
  }
  if(!file.exists(paste(basedir,"PAs",pa,date,"Planning Inputs/Template Output",sep="/"))){
    dir.create(paste(basedir,"PAs",pa,date,"Planning Inputs/Template Output",sep="/"))
  }
  
  ####Scripts
  if(!file.exists(paste(basedir,"PAs",pa,date,"Scripts",sep="/"))){
    dir.create(paste(basedir,"PAs",pa,date,"Scripts",sep="/"))
  }
}