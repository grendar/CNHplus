#'
#' Computes Absolute Copy Number (ACN) tumor profile from Relative Copy Number (RCN) sample profile and tumor purity, tumor ploidy
#'
#' \code{r2q} computes ACN of tumor for tumor RCN, tumor purity, tumor ploidy.
#'
#' @param r sample RCN
#' @param purity tumor purity (scalar)
#' @param ploidy tumor ploidy (scalar)
#' @return ACN tumor profile (vector)
#' @export
r2q = function(r, purity, ploidy) {
  #
  checkmate::assert(
    checkmate::check_vector(r),
    checkmate::check_number(purity),
    checkmate::check_number(ploidy)
  )
  #
  return(ploidy*r + 2*(r -1)*(1/purity -1))
  #
}
