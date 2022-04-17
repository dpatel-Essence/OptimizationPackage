#' Read CSV Function
#'
#' This Function reads in csv's with particular settings
#' @param file Location of Base Directory
#' 
#' @keywords Read CSV
#' @export
#' @examples
#' read_csv()

#Collate

#fls = paste(dir(pattern="'\\.[rR]$'", recursive=TRUE), collapse=" ")
#cat(strwrap(sprintf("Collate: %s", fls), exdent=4), sep="\n")

#if(any(grepl("package:readr", search()))) {detach("package:readr");print("readr removed from global environment")}

read_csv <- function(file,...){
  read.csv(file,stringsAsFactors=F,check.names=F,...)
}