#'
#' For given RCN profile r of sample and a grid of purities and ploidies, those pairs for which the tumor ACN profile q fails the non-negativity requirement are removed from the grid
#'
#' \code{make_grid_plus} removes from the grid all pairs of (purity, ploidy) for which the tumor ACN profile q has at least one segment's copy negative; tumor ACN profile q is obtained from the sample RCN profile q and the tumor purity, the tumor ploidy.
#'
#' @param grid grid of purities, ploidies (matrix)
#' @param r sample RCN  profile (vector)
#' @return grid (matrix) of purities, ploidies
#' @export
make_grid_plus = function(grid, r) {
  #
  checkmate::assert(
    checkmate::check_matrix(grid),
    checkmate::check_vector(r)
  )
  #
  return(grid[apply(grid, 1, check_q, r = r) == TRUE,])
  #
}
