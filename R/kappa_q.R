#'
#' Computes value of the objective function kappa for ACN profile q and associated segments width vector w
#'
#' \code{kappa_q} evaluates the objective function kappa.
#'
#' @param q ACN profile of tumor (vector)
#' @param w vector of segments width
#' @return value of kappa
#' @export
kappa_q = function(q, w) {
  #
  checkmate::assert(
    checkmate::check_vector(q),
    checkmate::check_vector(w)
  )
  #
  d = abs(q - round(q))
  w = w/sum(w)
  return(sum(w*d))
  #
}
