#' Permutations
#'
#' Used in Detect and Remove Outliers Function
#' @param lst
#' 
#' @keywords Permutations
#' @export
#' @examples
#' permn()

permn <- function(lst){
  out <- NULL
  if(length(lst)==1){out <- matrix(lst[[1]])}
  else{
    for(i in 1:length(lst)){
      if(i==1){
        out <- cbind(out,rep(rep(lst[[i]],each=1),do.call("prod",lapply(lst[(i+1):length(lst)],length))))
      }
      else if(i==length(lst)){
        out <- cbind(out,rep(rep(lst[[i]],each=do.call("prod",lapply(lst[1:(i-1)],length))),1))
      }
      else{
        out <- cbind(out,rep(rep(lst[[i]],each=do.call("prod",lapply(lst[1:(i-1)],length))),
                             do.call("prod",lapply(lst[(i+1):length(lst)],length))))
      }
    }
  }
  colnames(out) <- NULL
  return(out)
}