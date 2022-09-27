#'
#' Makes grid for vectors of purities and ploidies
#'
#' \code{make_grid} creates the cartesian product of a vector of purities and a vector of ploidies.
#'
#' @param purity vector of purities
#' @param ploidy vector of ploidies
#' @return matrix comprising the cartesian product of the vector of purities and the vector of ploidies
#' @examples
#' # to make grid as in van Dijk et al.
#' grid = make_grid(purity = seq(0.2, 1, 0.01), ploidy = seq(1.5, 5, 0.01))
#' @references{\insertRef{van2021chromosomal}{CNHplus}}
#' @export
make_grid = function(purity, ploidy) {
  #
  checkmate::assert(
    checkmate::check_vector(purity),
    checkmate::check_vector(ploidy)
  )
  #
  return(as.matrix(expand.grid(purity = purity, ploidy = ploidy)))
}
