#' Standard Maximization Function
#'
#' Creates Matrix of Spend to the power * coefficient
#' @param sp spend
#' @param diag_mat coefficient and power matrix
#' 
#' @keywords Standard maximization function
#' @export
#' @examples
#' maxim_fun_standard(sp,diag_mat)

maxim_fun_standard <- function(sp,diag_mat){
  diag_mat[,"Coefficient"]*(sp^diag_mat[,"Power"])
}