#' Faster date transformation
#' 
#' Based on the trick that often dates are repeated in a column, we make date transformation
#'  faster by computing date transformation only on uniques.
#' @param x	An object to be converted
#' @param ... other argument to pass to  \code{\link{as.Date}}
#' @return methods return an object of class Date
#' @details Based on empirical experiences, it is faster to use this function if in average each 
#' date is present more than 2 time. 
#' @name fast_dates
#' @rdname fast_dates
#' @examples
#' # Work the same as as.Date
#' as.Date.fast("2018-01-01", format="%Y-%m-%d")
#' @export
as.Date.fast <- function(x, ...){
  if (! is.factor(x)){
    x <- as.factor(x)
  }
  date_levels <- levels(x)
  date_levels <- as.Date(date_levels, ...)
  return(date_levels[x])
}

#' @rdname fast_dates
#' @examples
#' # Work the same as as.Date
#' as.POSIXct.fast("2018-01-01", format="%Y-%m-%d")
#' @export
as.POSIXct.fast <- function(x, ...){
  if (! is.factor(x)){
    x <- as.factor(x)
  }
  date_levels <- levels(x)
  date_levels <- as.POSIXct(date_levels, ...)
  return(date_levels[x])
}

#' @rdname fast_dates
#' @examples
#' # Work the same as as.Date
#' parse_date_time.fast("2018-01-01", orders = "Ymd")
#' # Benchmark
#' \dontrun{
#' data(messy_adult)
#' microbenchmark::microbenchmark(
#'    as.Date(messy_adult[["date1"]], format = "%Y-%m-%d"),
#'    as.Date.fast(messy_adult[["date1"]], format = "%Y-%m-%d"),
#'    as.POSIXct(messy_adult[["date1"]], format = "%Y-%m-%d"),
#'    as.POSIXct.fast(messy_adult[["date1"]], format = "%Y-%m-%d")
#' )
#' # benchmark shows that it is way faster, especially for POSIXct
#' # "##NOT RUN:" mean that this example hasn't been run on CRAN since its long. But you can run it!
#'}
#' @export
#' @importFrom lubridate parse_date_time
parse_date_time.fast <- function(x, ...){
  if (! is.factor(x)){
    x <- as.factor(x)
  }
  date_levels <- levels(x)
  date_levels <- parse_date_time(date_levels, ...)
  return(date_levels[x])
}
