#######################################################################################
############################# Set col as numeric ######################################
#######################################################################################
#' Set columns as numeric
#' 
#' Set as numeric a character column (or a list of columns) from a data.table.
#' @param dataSet Matrix, data.frame or data.table
#' @param cols List of column(s) name(s) of dataSet to transform into numerics
#' @param stripString should I change "," to "." in the string? (logical, default to FALSE) 
#' If set to TRUE, computation will be a bit longer
#' @param verbose Should the function log (logical, default to TRUE)
#' @return  dataSet (as a \code{\link{data.table}}), with specified columns set as numeric. 
#' @examples
#' # Build a fake data.table
#' dataSet <- data.frame(charCol1 = c("1", "2", "3"), 
#' 						 charCol2 = c("4", "5", "6"))
#'
#' # Set charCol1 and charCol2 as numeric
#' dataSet <- setColAsNumeric(dataSet, cols = c("charCol1", "charCol2"))
#'
#' # Using strip string when spaces or wrong decimal separator is used
#' dataSet <- data.frame(charCol1 = c("1", "2", "3"), 
#'                       charCol2 = c("4, 1", "5, 2", "6, 3"))
#'
#' # Set charCol1 and charCol2 as numeric
#' setColAsNumeric(dataSet, cols = c("charCol1", "charCol2")) 
#' # generate mistakes
#' setColAsNumeric(dataSet, cols = c("charCol1", "charCol2"), stripString = TRUE) 
#' # Doesn't generate any mistake (but is a bit slower)
#' @import data.table
#' @export
setColAsNumeric <- function(dataSet, cols, stripString = FALSE, verbose = TRUE){ 
  ## Working environement
  function_name <- "setColAsNumeric"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  cols <- real_cols(dataSet, cols, function_name, types = c("character"))
  is.verbose(verbose)
  
  ## Initialization
  
  ## Computation
  if (verbose & length(cols) > 0){
    printl(function_name, ": I will set some columns as numeric")
    pb <- initPB(function_name, cols)
  }
  for (col in cols){
    if (verbose){
      printl(function_name, ": I am doing the column ", col, ".")
      options(warn = -1) # if verbose, disable warning, it will  be logged
    }
    n_na_init <- sum(is.na(dataSet[[col]]))
    if (stripString){
      set(dataSet, NULL, col, as.numericStrip(dataSet[[col]]))
    }
    else{
      set(dataSet, NULL, col, as.numeric(dataSet[[col]])) 
    }
    if (verbose){
      printl(function_name, ": ", sum(is.na(dataSet[[col]])) - n_na_init, 
             " NA have been created due to transformation to numeric.")
    } 
    if (verbose){
      setPB(pb)
    }
  }
  if (verbose){
    # reset warnings
    options(warn = 0)
  }
  
  ## Wrapp-up
  return(dataSet)
}


#######################################################################################
############################# Set col as character ####################################
#######################################################################################
#' Set columns as character
#'
#' Set as character a column (or a list of columns) from a data.table.
#' @param dataSet Matrix, data.frame or data.table
#' @param cols List of column(s) name(s) of dataSet to transform into characters. To transform 
#' all columns, set it to "auto". (characters, default to "auto")
#' @param verbose Should the function log (logical, default to TRUE)
#' @return  dataSet (as a \code{\link{data.table}}), with specified columns set as character. 
#' @examples
#' # Build a fake data.frame
#' dataSet <- data.frame(numCol = c(1, 2, 3), factorCol = as.factor(c("a", "b", "c")))
#'
#' # Set numCol and factorCol as character
#' dataSet <- setColAsCharacter(dataSet, cols = c("numCol", "factorCol"))
#' @import data.table
#' @export
setColAsCharacter <- function(dataSet, cols = "auto", verbose = TRUE){ 
  ## Working environement
  function_name <- "setColAsCharacter"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  is.verbose(verbose)
  cols <- real_cols(dataSet, cols, function_name)
  
  ## Initalization
  if (verbose & length(cols) > 0){
    printl(function_name, ": I will set some columns as character")
    pb <- initPB(function_name, cols)
  }
  
  ## Computation
  for (col in cols){
    if (verbose){
      printl(function_name, ": I am doing the column ", col, ".")
    }
    if ( (is.character(dataSet[[col]])) & verbose){
      printl(function_name, ": ", col, " is a character, i do nothing.")
    }
    if (! (is.character(dataSet[[col]]))){
      set(dataSet, NULL, col, as.character(dataSet[[col]])) 
    }
    if (verbose){
      setPB(pb)
    }
  }
  return(dataSet)
}

#######################################################################################
############################# Set col as Date #########################################
#######################################################################################
#' Set columns as POSIXct 
#'
#' Set as POSIXct a character column (or a list of columns) from a data.table.
#' @param dataSet Matrix, data.frame or data.table
#' @param cols List of column(s) name(s) of dataSet to transform into dates
#' @param format Date's format (function will be faster if the format is provided) 
#' (character or list of character, default to NULL).\cr
#' For timestamps, format need to be provided ("s" or "ms" or second or millisecond timestamps)
#' @param verbose Should the function log (logical, default to TRUE)
#' @details 
#' setColAsDate is way faster when format is provided. If you want to identify dates and format
#' automatically, have a look to \code{\link{identifyDates}}. \cr
#' If input column is a factor, it will be returned as a POSIXct column. \cr
#' If \code{cols} is kept to default (NULL) setColAsDate won't do anything.
#' @return \code{dataSet} (as a \code{\link{data.table}}), with specified columns set as Date. 
#' If the transformation generated only NA, the column is set back to its original value.
#' @examples
#' # Lets build a dataSet set
#' dataSet <- data.frame(ID = 1:5, 
#'                   date1 = c("2015-01-01", "2016-01-01", "2015-09-01", "2015-03-01", "2015-01-31"), 
#'                   date2 = c("2015_01_01", "2016_01_01", "2015_09_01", "2015_03_01", "2015_01_31")
#'                   )
#'
#' # Using setColAsDate for date2
#' data_transformed <- setColAsDate(dataSet, cols = "date2", format = "%Y_%m_%d")
#' 
#' # Control the results
#' lapply(data_transformed, class)
#' 
#' # With multiple formats:
#' data_transformed <- setColAsDate(dataSet, format = list(date1 = "%Y-%m-%d", date2 = "%Y_%m_%d"))
#' lapply(data_transformed, class)
#' 
#' # It also works with timestamps
#' dataSet <- data.frame(time_stamp = c(1483225200, 1485990000, 1488495600))
#' setColAsDate(dataSet, cols = "time_stamp", format = "s")
#' @import data.table
#' @importFrom lubridate parse_date_time
#' @importFrom stringr str_replace_all
#' @export
setColAsDate <- function(dataSet, cols = NULL, format = NULL, verbose = TRUE){
  ## Working environment
  function_name <- "setColAsDate"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  is.verbose(verbose)
  is.format(format, function_name)
  cols <- parse_date_cols(cols, format, function_name)
  cols <- real_cols(dataSet, cols, function_name, types = c("character", "factor", "numeric", "integer"))
  
  ## Initialization
  start_time <- proc.time()
  if (verbose & length(cols) > 0){
    printl(function_name, ": I will set some columns as Date.")
    options(warn = -1) # if verbose, disable warning, it will  be logged
    pb <- initPB(function_name, cols)
  }
  n_transformed <- length(cols)
  
  ## Computation
  for (col in cols){
    if (verbose){
      printl(function_name, ": I am doing the column ", col, ".")
    }
    # Creating data sample to transform
    if (is.factor(dataSet[[col]])){
      data_sample <- levels(dataSet[[col]])[dataSet[[col]]]
    }
    else{
      data_sample <- dataSet[[col]]
    }
    n_na_init <- sum(is.na(data_sample))
    
    # Get format:
    if (is.character(format) || is.null(format)){
      col_format <- format
    }
    if (is.list(format)){
      col_format <- format[[col]]
      if (is.null(col_format)){
        col_format <- format[col == cols]
      }
    }
    if (is.character(data_sample)){
      # If format is NULL, we let R determine the format
      if (is.null(col_format)){
        # If format is not given, search for it. 
        formats_tmp <- identifyDates(dataSet[, c(col), with = FALSE], n_test = min(30, nrow(dataSet)))
        if (length(formats_tmp) == 0){
          printl(function_name, ": ", col, " doesn't seem to be a date, if it really is please provide format.")
          next()
        }
        else{
          col_format <- formats_tmp[[col]]
        }
      }
      # If it isn't NULL
      if (!is.null(col_format)){
        # it is faster if it's a format accepted by parse_date_time, so we check that
        format_pdt <- str_replace_all(col_format, "[[:punct:]]", "")
        if (format_pdt %in% formatForparse_date_time()){
          result <- parse_date_time(data_sample, orders = format_pdt)
        }
        else{
          result <- as.POSIXct(data_sample, format = col_format)
        }
      }
    }
    else if (is.numeric(data_sample) & !is.null(col_format)){
      if (col_format == "s"){
        result <- as.POSIXct(dataSet[[col]], origin = "1970-01-01 00:00:00")
      }
      if (col_format == "ms"){
        result <- as.POSIXct(dataSet[[col]] / 1000, origin = "1970-01-01 00:00:00")
      }
    }
    else{
      options(warn = 0)
      warning(paste0(function_name, ": I can't handle ", col, ", please see documentation."))
      options(warn = -1)
      n_transformed <- n_transformed - 1
      next()
    }
    n_na_end <- sum(is.na(result))
    if (n_na_end - n_na_init > 0){
      if (n_na_end == nrow(dataSet) & n_na_init < nrow(dataSet)){ 
        # If we generated only NA and format wasn't provide, we shouldn't have changer it so we set it bakck to char
        if (verbose){
          printl(function_name, ":", " Since i generated only NAs i set ", col, " as it was before.")
        }
        result <- data_sample
      } 
      else{
        if (verbose){
          printl(function_name, ":", n_na_end - n_na_init, " NA have been created due to transformation to Date.")
        }
      }
    }
    # Assign result
    set(dataSet, NULL, col, result)  
    
    # Set log
    if (verbose){setPB(pb)}
  }
  ## Wrapp-up
  if (verbose){
    printl(function_name, ": it took me: ", round( (proc.time() - start_time)[[3]], 2), 
           "s to transform ", n_transformed, " column(s) to Dates.")
    options(warn = 0) # reset warnings
  }
  return(dataSet)
}

## Control input format
is.format <- function(format, function_name = "is.format"){
  if (!is.null(format)){
    if (!(is.character(format) || is.list(format))){
      stop(paste0(function_name, ": format should either be list of formats or a character."))
    }
    else{
      if (is.list(format) & ! all(sapply(format, is.character))){
        stop(paste0(function_name, ": format should either be list of character or a character."))
      }
    }
  }
}

# Match cols and format, this function is due to will to keep consitency between previous version
parse_date_cols <- function(cols, format, function_name = "parse_date_cols"){
  if (! is.null(cols)){
    if (is.list(format)){
      if (length(format) != length(cols) & any(!cols %in% format)){
        stop(paste0(function_name, ": you provide cols and format but I'm not able to match them, please feed format as named list."))
      }
      return(cols)
    }
  }
  else{
    cols <- names(format)
    return(cols)
  }
  return(cols)
}
############################################################################################################
########################################### charToFactorOrLogical ##########################################
############################################################################################################
#' Set columns as factor
#' 
#' Set columns as factor and control number of unique element, to avoid having too large factors.
#' @param dataSet Matrix, data.frame or data.table
#' @param cols List of column(s) name(s) of dataSet to transform into factor. To transform all columns
#'  set it to "auto", (characters, default to auto).
#' @param n_levels Max number of levels for factor (integer, default to 53) 
#' set it to -1 to disable control.
#' @param verbose Should the function log (logical, default to TRUE)
#' @details 
#' Control number of levels will help you to distinguish true categorical columns from just characters 
#' that should be handled in another way.
#' @return \code{dataSet}(as a \code{\link{data.table}}), with specified columns set as factor or logical.
#' @examples
#' # Load messy_adult
#' data("messy_adult")
#' 
#' # we wil change education
#' messy_adult <- setColAsFactor(messy_adult, cols = "education")
#' 
#' sapply(messy_adult[, .(education)], class)
#' # education is now a factor
#' @export
setColAsFactor <- function(dataSet, cols = "auto", n_levels = 53, verbose = TRUE){
  ## Working environment
  function_name <- "setColAsFactor"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  if (!is.numeric(n_levels)){stop(paste0(function_name, ": n_levels should be an integer."))}
  cols <- real_cols(dataSet, cols, function_name)
  is.verbose(verbose)
  
  ## Initialization
  if (verbose){
    printl(function_name, ": I will set some columns to factor.")
    pb <- initPB(function_name, cols)
  }
  n_transformed <- 0
  start_time <- proc.time()
  
  ## Computation
  for (col in cols){ 
    if (verbose){
      printl(function_name, ": I am doing the column ", col, ".")
    }
    if (n_levels != -1){
      if (fastMaxNbElt(dataSet[[col]], n_levels)){
        set(dataSet, NULL, col, as.factor(dataSet[[col]]))
        n_transformed <- n_transformed + 1
      }
      else{
        printl(function_name, ": ", col, " has more than ", n_levels, " values, i don't transform it.")
      }  
    }
    else{
      set(dataSet, NULL, col, as.factor(dataSet[[col]]))
    }
    if (verbose){
      setPB(pb)
    }
  }
  if (verbose){
    printl(function_name, ": it took me: ", round((proc.time() - start_time)[[3]], 2), 
           "s to transform ", n_transformed, " column(s) to factor.")
  }
  ## Wrapp up
  return(dataSet)
}
