#######################################################################################
############################# Set col as numeric ######################################
#######################################################################################
#' Set columns as numeric
#' 
#' Set as numeric a character column (or a list of columns) from a data.table
#' @param dataSet Matrix, data.frame or data.table
#' @param cols a list of colnames of dataSet (or just one) to transform into numerics
#' @param stripString should i change ", " to "." in the string? (logical, default to FALSE) 
#' If set to TRUE, computation will be a bit longer
#' @param verbose should the function log (logical, default to TRUE)
#' @return  dataSet (as a \code{\link{data.table}}), with specified columns set as numeric. 
#' @examples
#' # Build a fake data.table
#' dataSet <- data.table(charCol1 = c("1", "2", "3"), 
#' 						 charCol2 = c("4", "5", "6"))
#'
#' # Set charCol1 and charCol2 as numeric
#' dataSet <- setColAsNumeric(dataSet, cols = c("charCol1", "charCol2"))
#'
#' # Using strip string when spaces or wrong decimal separator is used
#' dataSet <- data.table(charCol1 = c("1", "2", "3"), 
#'                       charCol2 = c("4, 1", "5, 2", "6, 3"))
#'
#' # Set charCol1 and charCol2 as numeric
#' setColAsNumeric(dataSet, cols = c("charCol1", "charCol2")) 
#' # generate mistakes
#' setColAsNumeric(dataSet, cols = c("charCol1", "charCol2"), stripString = TRUE) 
#' # Doesn't generate any mistake (but is a bit slower)
#' @export
setColAsNumeric <- function(dataSet, cols, stripString = FALSE, verbose = TRUE){ 
  ## Working environement
  function_name <- "setColAsNumeric"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  cols = real_cols(cols, names(dataSet), function_name)
  ## Initialization
  
  ## Computation
  if (verbose){
    printl(function_name, ": I will set some columns as numeric")
  }
  for (col in cols){
    if (verbose){
      printl(function_name, ": I am doing the column", col)
    }
    if ( (!class(dataSet[[col]])%in% c("character", "integer", "numeric")) & verbose){
      warning(paste(function_name, ":", col, 
					"isn\"t a character a numeric or an integer, i do nothing"))
    }
    if (class(dataSet[[col]]) == "character"){
      
      nb_na_init = sum(is.na(dataSet[[col]]))
      if (stripString){
        set(dataSet, NULL, col, as.numericStrip(dataSet[[col]]))
      }
      else{
        set(dataSet, NULL, col, as.numeric(dataSet[[col]])) 
      }
      if (verbose){
        printl(function_name, ":", sum(is.na(dataSet[[col]])) - nb_na_init, 
			   "NA have been created due to transformation to numeric")
      } 
    }
  }
  
  ## Wrapp-up
  return(dataSet)
}


#######################################################################################
############################# Set col as character ####################################
#######################################################################################
#' Set columns as character
#'
#' Set as character a column (or a list of columns) from a data.table
#' @param dataSet Matrix, data.frame or data.table
#' @param cols a list of colnames of dataSet (or just one) to transform into characters
#' @param verbose should the function log (logical, default to TRUE)
#' @return  dataSet (as a \code{\link{data.table}}), with specified columns set as character. 
#' @examples
#' # Build a fake data.table
#' dataSet <- data.table(numCol = c(1, 2, 3), factorCol = as.factor(c("a", "b", "c")))
#'
#' # Set numCol and factorCol as character
#' dataSet <- setColAsCharacter(dataSet, cols = c("numCol", "factorCol"))
#' @export
setColAsCharacter <- function(dataSet, cols, verbose = TRUE){ 
  ## Working environement
  library(data.table)
  setNumericRounding(0) # Good practice for data.table
  function_name <- "setColAsCharacter"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  
  ## Initialization
  cols = real_cols(cols, names(dataSet), function_name)
  
  ## Computation
  if (verbose){
    printl(function_name, ": I will set some columns as character")
  }
  for (col in cols){
    if (verbose){
      printl(function_name, ": I am doing the column", col)
    }
    if ( (class(dataSet[[col]]) == "character") & verbose){
      printl(function_name, ":", col, "is a character, i do nothing")
    }
    if (class(dataSet[[col]]) != "character"){
      set(dataSet, NULL, col, as.character(dataSet[[col]])) 
    }
    
  }
  return(dataSet)
}

#######################################################################################
############################# Set col as Date #########################################
#######################################################################################
#' Set columns as POSIXct 
#'
#' Set as POSIXct a character column (or a list of columns) from a data.table
#' @param dataSet Matrix, data.frame or data.table
#' @param cols a list of colnames of dataSet (or just one) to transform into dates
#' @param format the format of date (function is faster if the format is provided) (default to NULL)
#' @param verbose should the function log (logical, default to TRUE)
#' @details 
#' setColAsDate is way faster when format is provided. If you want to identify dates and format
#' automatically, have a look to \code{\link{findAndTransformDates}}
#' @return dataSet (as a \code{\link{data.table}}), with specified columns set as Date. 
#' If the transformation generated only NA, the column is set back to its original value.
#' @examples
#' # Lets build a dataSet set
#' dataSet <- data.table(ID = 1:5, 
#'                   date1 = c("2015-01-01", "2016-01-01", "2015-09-01", "2015-03-01", "2015-01-31"), 
#'                   date2 = c("2015_01_01", "2016_01_01", "2015_09_01", "2015_03_01", "2015_01_31")
#'                   )
#'
#' # Using setColAsDate for date2
#' data_transformed <- setColAsDate(dataSet,cols = "date2", format = "%Y_%m_%d")
#' 
#' # Control the results
#' lapply(data_transformed,class)
#' @importFrom lubridate parse_date_time 
#' @importFrom stringr str_replace_all
#' @export
setColAsDate <- function(dataSet, cols, format = NULL, verbose = TRUE){
  ## Working environment
  library(lubridate)
  library(stringr)
  library(data.table)
  setNumericRounding(0) # Good practice for data.table
  function_name <- "setColAsDate"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  
  ## Initialization
  start_time <- proc.time()
  if (verbose){
    printl(function_name, ": I will set some columns as Date")
  }
  cols = real_cols(cols, names(dataSet), function_name)
  
  ## Computation
  for (col in cols){
    if (verbose){
      printl(function_name, ": I am doing the column", col)
    }
    if (class(dataSet[[col]]) != "character" & verbose){
      warning(paste0(function_name, ": ", col, " is\'nt a character, i do nothing"))
    }
    if (class(dataSet[[col]]) == "character"){
      nb_na_init <- sum(is.na(dataSet[[col]]))
      data_sample <- dataSet[[col]]
      # If format is NULL, we let R determine the format
      if (is.null(format)){
        set(dataSet, NULL, col, as.POSIXct(dataSet[[col]])) 
      }
      # If it isn't NULL
      if (!is.null(format)){
        format4parse_date_time <- formatForparse_date_time()
		format_tmp = str_replace_all(format, "[[:punct:]]", "")
        if (format_tmp %in% format4parse_date_time){
          set(dataSet, NULL, col, parse_date_time(dataSet[[col]], orders = format_tmp))
        }
        else{
          set(dataSet, NULL, col, as.POSIXct(dataSet[[col]], format = format))
        }
      }
      
      nb_na_end <- sum(is.na(dataSet[[col]]))
      if (verbose){
        printl(function_name, ":", nb_na_end - nb_na_init, "NA have been created due to transformation to Date")
      }
      # If we generated only NA and format wasn't provide, we shouldn't have changer it so we set it bakck to char
      if (nb_na_end == nrow(dataSet) & nb_na_init < nrow(dataSet) & is.null(format)){
        if (verbose){
          printl(function_name, ":", "Since i generated only NAs i set", col, "as it was before")
        }
        dataSet[[col]] <- data_sample
      }
    }
  }
  
  ## Wrapp-up
  if (verbose){
    printl(function_name, ": it took me: ", round((proc.time() - start_time)[[3]], 2), 
           "s to transform ", length(cols), " columns to Dates")
  }
  return(dataSet)
  
}



############################################################################################################
########################################### charToFactorOrLogical ##########################################
############################################################################################################
#' Set columns as factor
#' 
#' Set columns as factor, or logical if they have too many different values
#' @param dataSet Matrix, data.frame or data.table
#' @param cols a list of colnames of dataSet (or just one) to transform into factor
#' @param n_levels max number of levels for factor (integer, default to 53)
#' @param verbose should the function log (logical, default to TRUE)
#' @details 
#' Control number of levels will help you to distinguish true categoricals from just characters 
#' that should be handle in another way.
#' @return dataSet (as a \code{\link{data.table}}), with specified columns set as factor or logical.
#' @examples
#' # Load messy_adult
#' data("messy_adult")
#' 
#' # we wil change mail and education
#' head(messy_adult[, .(mail, education)])
#' 
#' messy_adult <- setColAsFactorOrLogical(messy_adult, cols = c("mail", "education"))
#' 
#' sapply(messy_adult[, .(mail, education)], class)
#' head(messy_adult[, .(mail, education)])
#' # education is now a factor and mail a logical wether there was or not an mail.
#' @export
setColAsFactorOrLogical <- function(dataSet, cols, n_levels = 53, verbose = TRUE){
  ## Working environment
  library(data.table)
  setNumericRounding(0) # Good practice for data.table
  function_name = "setColAsFactorOrLogical"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  if (!is.numeric(n_levels)){stop(paste0(function_name, ": n_levels should be an integer"))}
  cols = real_cols(cols, names(dataSet), function_name)
  
  ## Computation
  for (col in cols){ 
    if (verbose){
      printl(function_name, ": I am doing the column", col)
    }
    if (fastMaxNbElt(dataSet[[col]], n_levels)){
      set(dataSet, NULL, col, as.factor(dataSet[[col]]))
    }
    else{
      set(dataSet, NULL, col, !is.na(dataSet[[col]]) & dataSet[[col]] != "" )
	  
    }
  }


  ## Wrapp up
  return(dataSet)
}
