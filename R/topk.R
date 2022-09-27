#'
#' Grid of tumor purities, tumor ploidies is sorted by the associated values of kappas (values of the objective function)
#' and top k values (i.e., with the smallest kappa) are returned
#'
#' @param grid grid of tumor purities, tumor ploidies (matrix)
#' @param kappas vector of associated values of the objective function kappa
#' @param k top k to be returned
#' @return elements of the grid with the k smallest values of kappa, together with the corresponding valaues of kappa
#' @export
#' @seealso \code{find_cnhplus}
topk = function(grid, kappas, k) {
  #
  checkmate::assert(
    checkmate::check_matrix(grid),
    checkmate::check_vector(kappas),
    checkmate::check_number(k)
  )
  #
  ks = sort.int(kappas, decreasing = FALSE, index.return = TRUE)
  dd = data.frame(purity = grid[ks$ix[1:k],1],
                  ploidy = grid[ks$ix[1:k],2],
                  kappa = ks$x[1:k])
  row.names(dd) = NULL # k=1
  return(dd)
}
