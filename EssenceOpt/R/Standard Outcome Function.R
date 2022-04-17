#' Standard Outcome Function
#'
#' Creates Matrix of outcome to the power * coefficient
#' @param sp outcome
#' @param diag_mat coefficient and power matrix
#' 
#' @keywords Standard Outcome function
#' @export
#' @examples
#' out_fun_standard(sp,diag_mat)

out_fun_standard <- function(sp,diag_mat){
  diag_mat[,"Coefficient"]*(sp^diag_mat[,"Power"])
}