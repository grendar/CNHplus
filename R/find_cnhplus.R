#'
#' For sample RCN profile r finds CNH+ and the associated tumor purity, tumor ploidy
#'
#' \code{find_cnhplus} by searching over grid, finds value of CNH+ and associated tumor purity, tumor ploidy for a sample RCN.
#' It also makes possible to find CNH.
#'
#' @param grid, grid of purities, ploidies (matrix)
#' @param r sample RCN profile (vector)
#' @param w vector of segment widths
#' @param k how many candidate pairs of purity, ploidy for the solution to return
#' @param plus if True (default) CNH+ is solved for; if False, CNH
#' @return top k elements of the grid together with the associated values of the objective function kappa
#' @seealso \code{plot_profile}
#' @export
find_cnhplus = function(grid, r, w, k, plus = TRUE) {
  #
  tryCatch(
    {
      if (nrow(grid) == 0) {
        return()
      }
    },
    error = function(e) {
      message('    Error: grid is missing!')
    }
  )
  #
  if (plus == TRUE) {
    grid = make_grid_plus(grid=grid, r=r)
  }
  #
  kappas = apply(grid, 1, kappa, r=r, w=w)
  return(topk(grid = grid, kappas = kappas, k = k))
  #
}
