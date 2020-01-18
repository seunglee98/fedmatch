#' winsor
#'
#' @param x numeric or vector. variable to be winsored
#' @param fraction numeric. winsor cutoff
#'
#' @return x winsored at fraction
#' @export
#'
#' @examples
#' 
#' 
winsor <- function (x, fraction=0.05) {
  if(length(fraction) != 1 || fraction < 0 || fraction > 0.5) {stop("bad value for 'fraction'") }
  x[x < quantile(x, fraction,   na.rm=TRUE)] = quantile(x, fraction, na.rm=TRUE)
  x[x > quantile(x, 1-fraction, na.rm=TRUE)] = quantile(x, 1-fraction, na.rm=TRUE)
  return(x)
}
