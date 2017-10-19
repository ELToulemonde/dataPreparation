###################################################################################################
######################################  generateFactorFromDate ####################################
###################################################################################################
#' Generate factor from dates
#' 
#' Taking Date or POSIXct colums, and building factor columns from them. 
#' @param dataSet Matrix, data.frame or data.table
#' @param cols list of date column(s) name(s) of dataSet to transform into factor. To transform all 
#' dates, set it to "auto", (characters, default to "auto")
#' @param type "year", "yearquarter", "yearmonth", "quarter" or "month", way to aggregate a date, 
#' (character, default to "yearmonth")
#' @param drop should \code{cols} be dropped after generation (logical, default to FALSE)
#' @param verbose should the function log (logical, default to TRUE)
#' @param ... Other arguments such as \code{name_separator} to separate words in new columns names
#' (character, default to ".")
#' @examples
#' # Load set, and find dates
#' data(messy_adult)
#' messy_adult <- findAndTransformDates(messy_adult, verbose = FALSE)
#' 
#' # Generate new columns
#' # Generate year month columns
#' messy_adult <- generateFactorFromDate(messy_adult, cols = c("date1", "date2", "num1"))
#' head(messy_adult[, .(date1.yearmonth, date2.yearmonth)])
#'
#'
#' # Generate quarter columns
#' messy_adult <- generateFactorFromDate(messy_adult, cols = c("date1", "date2"), type = "quarter")
#' head(messy_adult[, .(date1.quarter, date2.quarter)])
#' @export
#' @import data.table
generateFactorFromDate <- function(dataSet, cols, type = "yearmonth", drop = FALSE, verbose = TRUE, ...){
  ## Working environement
  function_name <- "generateFactorFromDate"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  cols <- real_cols(dataSet, cols, function_name, types = "date")
  is.verbose(verbose)
  
  ## Initialization
  args <- list(...)
  name_separator <- build_name_separator(args)
  start_time <- proc.time()
  
  ## Computation
  if (verbose){ 
    printl(function_name, ": I will create a factor column from each date column.")
    pb <- initPB(function_name, cols)
  }
  for (col in cols){
    new_col <- paste0(col, name_separator, type)
    new_col <- make_new_col_name(new_col, names(dataSet))
    dataSet[, (new_col) := date_factor(dataSet[[col]], type = type)]
    if (isTRUE(drop)){
      dataSet[, c(col) := NULL]
    }
  
    if (verbose){
      setPB(pb, col)
    }
  }
  if (verbose){ 
	gc(verbose = FALSE)
    printl(function_name, ": It took me ", round( (proc.time() - start_time)[[3]], 2), 
           "s to transform ", length(cols), " column(s).")
  }
  
  ## Wrapp-up
  return(dataSet)
}




###################################################################################################
######################################## date_factor ##############################################
###################################################################################################
## Code inspired by Ben Gorman in mltools package https://github.com/ben519/mltools
# Date Factor
# 
# Map a vector of dates to a factor at one of these levels {"yearmonth", "yearquarter", "quarter", "month"}
# @details
# The resulting vector is an ordered factor of the specified \code{type} (e.g. yearmonth)
# 
# @param dataSet A vector of date values
# @param type One of {"year", "yearquarter", "yearmonth", "quarter", "month"}
# @examples
# library(data.table)
# dataSet <- as.Date(c("2014-01-01", "2015-01-01", "2015-06-01"))
# date_factor(dataSet, type = "yearmonth")
# date_factor(dataSet, type = "yearquarter")
# date_factor(dataSet, type = "yearquarter")
#
# @export
#' @import data.table
date_factor <- function(dataSet, type = "yearmonth"){
  ## Working environement
  function_name <- "date_factor"
  
  ## Sanity check
  if (!type %in% c("year", "yearquarter", "yearmonth", "quarter", "month"))
    stop(paste0(function_name, "type must be one of 'year', 'yearquarter', 'yearmonth', 'quarter' or 'month'"))
  if (! is.date(dataSet)){
    stop(paste0(function_name, ": dataSet should contain dates."))
  }
  
  ## Initialization
  # set formating function
  if (type ==  "yearmonth"){
    format_func <- function(x)format(x, "%Y %b")
  } else if (type ==  "yearquarter"){
    format_func <- function(x) paste0(year(x), " Q", quarter(x))
    
  }else if (type ==  "year"){
    format_func <- year
    
  }else if (type ==  "quarter"){
    format_func <- function(x) paste0("Q", quarter(x))
    
  }else if (type ==  "month"){
    format_func <- function(x)format(x, "%b")
  }
  ## Computation
  result <- factor(format_func(dataSet))
  
  ## Wrapp-up
  return(result)
}



###################################################################################################
############################### generateDateDiffs #########################################################
###################################################################################################
#' Date difference
#' 
#' Perform the differences between all dates of the dataSet set and optionally with a static date.
#' @param dataSet Matrix, data.frame or data.table
#' @param cols list of date column(s) name(s) of dataSet to comute difference on. To transform all 
#' dates, set it to "auto"
#' @param analysisDate Static date (Date or POSIXct, optional)
#' @param units unit of difference between too dates (string, default to 'years') 
#' @param drop should \code{cols} be dropped after generation (logical, default to FALSE)
#' @param verbose should the function log (logical, default to TRUE)
#' @param ... Other arguments such as \code{name_separator} to separate words in new columns names
#' (character, default to ".")
#' @details 
#' \code{units} is the same as \code{\link{difftime}} units, but with years as a unit. 
#' @return dataSet (as a \code{\link{data.table}}) with more columns. 
#' A numeric column has been added for every couple of Dates. The result is in years. 
#' @examples
#' # First build a useful dataSet set
#' require(data.table)
#' dataSet <- data.table(ID = 1:100, 
#'                   date1 = seq(from = as.Date("2010-01-01"), 
#'                               to = as.Date("2015-01-01"), 
#'                               length.out = 100), 
#'                   date2 = seq(from = as.Date("1910-01-01"), 
#'                               to = as.Date("2000-01-01"), 
#'                               length.out = 100)
#'                   )
#'
#' # Now let's compute
#' dataSet <- generateDateDiffs(dataSet, cols = "auto", analysisDate = as.Date("2016-11-14"))
#' @import data.table
#' @export
generateDateDiffs <- function(dataSet, cols, analysisDate = NULL, units = "years", drop = FALSE, verbose = TRUE, ...){
  ## Working environement
  function_name <- "generateDateDiffs"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  if (!is.null(analysisDate) & ! is.date(analysisDate)){
    stop(paste0(function_name, ": analysisDate must be a Date"))
  }
  cols <- real_cols(dataSet, cols, function_name, types = "date")

  ## Initialization
  args <- list(...)
  name_separator <- build_name_separator(args)
  if (class(analysisDate) == "Date"){
    analysisDate <- as.POSIXct(format(analysisDate, "%Y-%m-%d"))
  }
  n_transformed <- 0
  if (verbose){ 
    printl(function_name, ": I will generate difference between dates.")
    start_time <- proc.time()
    pb <- initPB(function_name, cols)
  }
  ## Computation
  # Unify format
  dataSet <- dateFormatUnifier(dataSet = dataSet, format = "POSIXct")
  # Compute date difference
  for (col_i in cols){
    col_J <- cols[- (1:which(col_i == cols))]
    for (col_j in col_J){
      new_col <- paste(col_i, "Minus", col_j, sep = name_separator)
      new_col <- make_new_col_name(new_col, names(dataSet))
      dataSet[, c(new_col) := diffTime(dataSet[[col_i]], dataSet[[col_j]], units = units)]
      n_transformed <- n_transformed + 1
    }
    if (!is.null(analysisDate)){
      new_col <- paste(col_i, "Minus", "analysisDate", sep = name_separator)
      new_col <- make_new_col_name(new_col, names(dataSet))
      dataSet[, c(new_col) := diffTime(dataSet[[col_i]], analysisDate, units = units)]
      n_transformed <- n_transformed + 1
    }
    if (isTRUE(drop)){
      dataSet[, c(col_i) := NULL]
    }
    if (verbose){
      setPB(pb, col_i)
    }
  }
  if (verbose){ 
    printl(function_name, ": It took me ", round( (proc.time() - start_time)[[3]], 2), 
           "s to create ", n_transformed, " column(s).")
  }
  ## wrapp-up
  return(dataSet)
}


###################################################################################################
############################### Unify dates types #################################################
###################################################################################################

# @return a numeric
# extension of difftime to handle years
diffTime <- function(col1, col2, units = "days"){
  if (units %in% c("auto", "secs", "mins", "hours", "days", "weeks")){
    return(as.numeric(difftime(col1, col2, units = units)))
  }
  if (units == "years"){
    return(as.numeric(difftime(col1, col2, units = "days")) / 365.25) # To-do: check number of days in years instead? 
  }
  else{
    stop("Sorry this unit hasn't been implemented yet")
  }
}

