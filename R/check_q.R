#'
#' Is the ACN tumor profile q, corresponding to RCN sample profile r, tumor purity, tumor ploidy non-negative in every segment?
#'
#' \code{check_q} checks whether tumor ACN profile q is non-negative.
#'
#' @param pu_plo vector with two components; tumor purity, tumor ploidy
#' @param r sample RCN profile (vector)
#' @return logical; True, if tumor ACN profile q is non-negative in every segment; False otherwise
#' @export
check_q = function(pu_plo, r) {
  #
  checkmate::assert(
    checkmate::check_vector(r),
    checkmate::check_vector(pu_plo, len = 2)
  )
  #
  q = r2q(r=r, purity=pu_plo[1], ploidy=pu_plo[2])
  ifelse(all(q >= 0), return(TRUE), return(FALSE))
  #
}
