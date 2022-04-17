#' Create Folder
#'
#' Creates templates for optimizations
#' @param basedir,pa,date Location of directory
#' 
#' 
#' @keywords creates templates
#' @export
#' @examples
#' create_folder(basedir,pa,date)

create_templates <- function(basedir,pa,date,row_order=NULL){
  #####Define folders and check the file structure
  input_folder <- paste(basedir,"PAs",pa,date,"Planning Inputs/Template Input",sep="/")
  if(!file.exists(input_folder)){stop("Folder does not exist. Consider running the 'Create Folder' script.")}
  output_folder <- paste(basedir,"PAs",pa,date,"Planning Inputs/Template Output",sep="/")
  if(!file.exists(output_folder)){stop("Folder does not exist. Consider running the 'Create Folder' script.")}
  
  files <- list.files(input_folder,full.names=T)
  files <- grep("\\.csv",files,value=T)
  if(length(files)==0){stop("No CSVs exist in the folder. At least one must be present.")}
  
  #####Read in the CSVs
  file_list <- lapply(files,read_csv)
  
  #####Get the permutations of the rows in the files
  ####Permutations
  individ_perm_num <- permn(lapply(file_list,function(mat){1:nrow(mat)}))
  individ_perm_list <- vector("list",length(file_list))
  for(i in 1:length(file_list)){
    individ_perm_list[[i]] <- file_list[[i]][individ_perm_num[,i],,drop=F]
  }
  individ_perm <- do.call("cbind",individ_perm_list)
  
  ####Reorder
  if(!is.null(row_order)){
    if(sum(colnames(individ_perm)%in%row_order)!=ncol(individ_perm)|
       sum(row_order%in%colnames(individ_perm))!=length(row_order)){
      stop("Order must be a vector with the exact column names in the input files.")
    }
    column_ord <- eval(parse(text=paste("order(",paste(paste("individ_perm[,\"",
                                                             row_order,"\"]",sep=""),collapse=","),")",sep="")))
    individ_perm <- individ_perm[column_ord,,drop=F]
  }
  
  #####Get the permutations with the "all" buckets
  ####Get values
  u_vals <- lapply(1:ncol(individ_perm),function(ind,mat){
    c("All",unique(mat[,ind]))
  },mat=individ_perm)
  all_perm <- permn(u_vals)
  colnames(all_perm) <- colnames(individ_perm)
  
  ####Filter out values: those with zero "alls" and those that cannot exist based on the inputs
  all_perm <- all_perm[which(apply(all_perm,1,function(vec){sum(vec=="All")>0})),,drop=F]
  bool <- matrix(F,nrow=nrow(all_perm),ncol=length(file_list))
  for(i in 1:length(file_list)){
    for(j in 1:nrow(file_list[[i]])){
      bool[eval(parse(text=paste("which(",paste(paste("(all_perm[,\"",
                                                      colnames(file_list[[i]]),
                                                      "\"]==file_list[[i]][j,\"",
                                                      colnames(file_list[[i]]),
                                                      "\"]|all_perm[,\"",
                                                      colnames(file_list[[i]]),
                                                      "\"]==\"All\")",
                                                      sep=""),collapse="&"),")",sep=""))),i] <- T
    }
  }
  all_perm <- all_perm[which(apply(bool,1,function(vec){sum(vec)==length(vec)})),,drop=F]
  
  ####Reorder
  all_perm_ord <- unlist(lapply(ncol(all_perm):1,function(val,vec,mat){
    combs <- t(combn(vec,val))
    combs <- combs[nrow(combs):1,,drop=F]
    index <- unlist(apply(combs,1,function(vec,mat){
      ind_list <- lapply(1:nrow(mat),function(ind,mat){which(mat[ind,]=="All")},mat=mat)
      index <- which(unlist(lapply(ind_list,length))==length(vec))
      index <- index[which(apply(do.call("rbind",ind_list[index]),1,function(vec1,vec2){sum(vec1==vec2)==length(vec1)},vec2=vec))]
      return(index)
    },mat=mat))
  },vec=1:ncol(all_perm),mat=all_perm))
  all_perm <- all_perm[all_perm_ord,,drop=F]
  
  #####Constraint matrix template: rbind the permutations and add in blank columns
  cm_template <- data.frame(rbind(all_perm,individ_perm),check.names=F)
  cm_template$"Max Budget" <- rep(NA,nrow(cm_template))
  cm_template$"Max Cost Per" <- rep(NA,nrow(cm_template))
  cm_template$"Max Budget Granularity" <- rep(NA,nrow(cm_template))
  cm_template$"Max Budget Value Basis" <- rep(NA,nrow(cm_template))
  cm_template$"Max Cost Per Value Basis" <- rep(NA,nrow(cm_template))
  
  #####Rate template
  rate_template <- data.frame(individ_perm,check.names=F)
  rate_template$"CV %" <- rep(NA,nrow(rate_template))
  rate_template$"Plan Name" <- rep(NA,nrow(rate_template))
  rate_template$"Fee Schedule" <- rep(NA,nrow(rate_template))
  
  #####Write to CSV
  write.csv(cm_template,paste(basedir,"PAs",pa,date,
                              "Planning Inputs/Template Output/Constraint Matrix Template.csv",sep="/"),row.names=F,na="")
  write.csv(rate_template,paste(basedir,"PAs",pa,date,
                                "Planning Inputs/Template Output/Rate Template.csv",sep="/"),row.names=F,na="")
}