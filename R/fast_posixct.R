#' Faster date transformation
#' 
#' Based on the trick that often dates are repeated in a column, we make date transformation
#'  faster by computing date transformation only on uniques.
#' @param x	An object to be converted
#' @param ... other argument to pass to  \code{\link{as.POSIXct}}
#' @return as.POSIXct and as.POSIXlt return an object of the appropriate class. If tz was specified,
#' as.POSIXlt will give an appropriate "tzone" attribute. Date-times known to be invalid will be returned as NA.
#' @details The more
#' @examples
#' # Work the same as as.POSIXct
#' as.POSIXct_fast("2018-01-01", format="%Y-%m-%d")
#' @export
as.POSIXct_fast <- function(x, ...){
  if (! is.factor(x)){
    x <- as.factor(x)
  }
  date_levels <- levels(x)
  date_levels <- as.POSIXct(date_levels, ...)
  return(date_levels[x])
}
