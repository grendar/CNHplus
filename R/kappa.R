#'
#' Computes value of the objective function kappa for purity, ploidy pair, RCN sample profile r and associated segments width vector w
#'
#' \code{kappa} evaluates the objective function kappa.
#'
#' @param pu_plo vector with two components: tumor purity, tumor ploidy
#' @param r sample RCN profile
#' @param w vector of segments width
#' @return value of kappa
#' @export
kappa = function(pu_plo, r, w) {
  #
  checkmate::assert(
    checkmate::check_vector(r),
    checkmate::check_vector(w),
    checkmate::check_vector(pu_plo, len = 2)
  )
  #
  q = r2q(r=r, purity=pu_plo[1], ploidy=pu_plo[2])
  d = abs(q - round(q))
  w = w/sum(w)
  return(sum(w*d))
  #
}
