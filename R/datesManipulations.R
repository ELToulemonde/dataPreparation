###################################################################################
########################### findAndTransformDates #################################
###################################################################################
#' Identify date columns in a dataSet set
#' 
#' Function to find and transform dates. It use a bunch of default formats. 
#' But you can also add your own formats.
#' @param dataSet Matrix, data.frame or data.table
#' @param formats List of additional Date formats to check (see \code{\link{strptime}})
#' @param n_test Number of non-null rows on which to test (numeric, default to 30)
#' @param verbose Should the algorithm talk? (Logical, default to TRUE)
#' @details 
#' This function is looking for perfect transformation. 
#' If there are some mistakes in dataSet, consider setting them to NA before.
#' @section Warning:
#' All these changes will happen \strong{by reference}: please send a copy() of
#' your data.table to prepareSet if you do not want your
#' original dataSet to be modified.
#' @return The dataSet set (as a data.table) with identified dates transformed.
#' @examples
#' # Load exemple set
#' data(messy_adult)
#' head(messy_adult)
#' # using the findAndTransformDates
#' findAndTransformDates(messy_adult, n_test = 5)
#' head(messy_adult)
#' @export
findAndTransformDates <- function(dataSet, formats = NULL, n_test = 30, verbose = TRUE){
  ## Working environement
  function_name = "findAndTransformDates"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)

  ## initialization
  start_time <- proc.time()
  
  ## Computation
  # First we find dates
  dates <- identifyDates(dataSet, formats = formats, n_test = n_test, verbose = verbose)
  if (verbose){
    printl(function_name, ": It took me ", round((proc.time() - start_time)[[3]], 2), "s to identify formats")
  }
  # Now we transform (if there is something to transform)
  if (length(dates$dates) < 1){
    if (verbose){
      printl(function_name, ": There are no dates to transform. (If i missed something please provide the date format in inputs or consider using setColAsDate to transform it).")
    }
    return(dataSet)
  }
  start_time <- proc.time()
  for ( i in 1:length(dates$dates)){
    dataSet <- setColAsDate(dataSet, cols = dates$dates[i], format = dates$formats[i], verbose = FALSE)
  }
  if (verbose){
    printl(function_name, ": It took me ", round((proc.time() - start_time)[[3]], 2), "s to transform ", length(dates$dates), " columns to a Date format")
  }
  return(dataSet)
}


###################################################################################
############################### identifyDates #####################################
###################################################################################
# Identify date columns in a dataSet set
# 
# Function to identify dates columns and give there format. It use a bunch of default formats. But you can also add your own formats.
# @param dataSet a data.table or data.frame or matric 
# @param formats list of your personnal Date formats you want to check (see \code{\link{strptime}})
# @param n_test number of non-null rows on which we should test that it is indeed a date (default to 30)
# @param verbose logical: should the algorithm talk (Default to TRUE)
# @param hours logical: should we check for hours in date columns (Default to TRUE)
# @return 
# A list of two list. The first list is dates contains the list columns that are Dates. 
# The second list is formats contains the list of formats for each of the Dates of the first list. 
# BE CAREFULL those formats are the formats without separator (ex: %Y%m%d)
# @export
identifyDates <- function(dataSet, formats = NULL, n_test = 30, verbose = TRUE, ...){
  ## Working environement
  function_name = "identifyDates"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  n_test <- control_nb_rows(dataSet = dataSet, nb_rows = n_test, function_name = function_name, variable_name = "n_test")
  
  if (!is.logical(verbose)){stop("identifyDates: verbose should be a logical")}
  
  ## Initialization
  dates <- NULL
  formats <- NULL
  date_sep <-  c(",", "/", "-", "_", ":")
  
  ## Computation
  if (verbose){ 
    pb <- initPB(function_name, names(dataSet))
  }
  for ( col in names(dataSet) ){ 
    # We search dates only in characters
    if ( all(class(dataSet[[col]]) == "character") ){
      # Get a few lines that aren't NA, NULL nor ""
      data_sample <- findNFirstNonNull(dataSet[[col]], n_test)
      
      # We check only columns that contains something (not NA, NULL, "")
      if (length(data_sample) > 0){ 
        # Identify potentially used separator by checking it split date in at last two elements
        date_sep_tmp <- date_sep[sapply(date_sep, function(x)length(grep(x, data_sample))) > 0] 
        
        # Check formats with"date hours" only if there are more than 10 characters
        hours <- max(sapply(data_sample, nchar), na.rm = TRUE) > 10 
        # Debug warning
        if (is.na(hours) || is.infinite(hours)){ 
          warning(paste0(function_name, ": error i shouldn\'t be there. ",  ))
          hours <- FALSE
        }
        
        # Build list of all formats to check
        defaultDateFormats <- getPossibleDatesFormats(date_sep_tmp, hours = hours)
        formats_tmp <- unique(c(defaultDateFormats, formats))
        
        # Look for the good format
        format <- identifyFormats(dataSet = data_sample, formats = formats_tmp)
        
        # If a format has been found we note it
        if (! is.null(format)){ 
          dates <- c(dates, col)
          formats <- c(formats, format)
        }
      }
    }
    
    if (verbose){
      setPB(pb, col)
    }
  }
  if (verbose){ 
    close(pb); rm(pb); gc()
  }
  ## Wrapp-up
  return(list(dates = dates, formats = formats))
}

## To-do
# Handle month as french or english character
# Better output format?
# Compute expected string length if format is good? 




############################################################################################################
########################################### identifyFormats ################################################
############################################################################################################
# 
# 
identifyFormats <- function(dataSet, formats){
  ## Sanity check
  if (class(dataSet) != "character"){
    stop("identifyFormats: dataSet should be some characters")
  }
  
  ## Initalization
  formatFound <- FALSE
  nformat <- 1
  N_format <- length(formats)
  
  ## Computation
  while (!formatFound & nformat <= N_format){
    # We try to convert and unconvert to see if we found the right format
    converted <- as.POSIXct(dataSet, format = formats[nformat])
    unConverted <- format(converted, format = formats[nformat])
    if (sum(unConverted == dataSet, na.rm = TRUE) == length(dataSet)){
      formatFound <- TRUE
    }
    else{ # In a "else" otherwise if we find the format we will always take the second one!
      nformat <- nformat + 1
    }
  }
  
  ## Wrapp-up
  if (formatFound){
    format <- formats[nformat]
  }
  else{
	# Return NULL if we didn't find format
    format <- NULL
  }
  return(format)
}


###################################################################################
############################### diffDates #########################################
###################################################################################
#' Date difference
#' 
#' Perform the differences between all dates of the dataSet set and optionally with a static date.
#' @param dataSet Matrix, data.frame or data.table
#' @param analysisDate Static date (Date or POSIXct, optional)
#' @param units unit of difference between too dates (string, default to 'years') 
#' @param name_separator Separator to put between words in new column names (default to '.')
#' @details 
#' \code{units} is the same as \code{\link{difftime}} unites, but with years as a unit. 
#' @return dataSet (as a \code{\link{data.table}}) with more columns. 
#' A numeric column has been added for every couple of Dates. The result is in years. 
#' @examples
#' # First build a useful dataSet set
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
#' dataSet <- diffDates(dataSet, analysisDate = as.Date("2016-11-14"))
#' @import data.table
#' @export
diffDates <- function(dataSet, analysisDate = NULL, units = "years", name_separator = "."){
  ## Working environement
  function_name = "diffDates"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  if (!is.null(analysisDate) & ! any(class(analysisDate) %in% c("Date", "POSIXct"))){
    stop("diffDates: analysisDate must be a Date")
  }
  if(is.null(name_separator)){
	name_separator = "."
  }
  ## Initialization
  if (class(analysisDate) == "Date"){
	analysisDate <- as.POSIXct(format(analysisDate, "%Y-%m-%d"))
  }
  dataSet <- dateFormatUnifier(dataSet = dataSet, format = "POSIXct")
  
  ## Computation
  dates <- names(dataSet)[sapply(dataSet, is.date)]
  if (length(dates) > 1){
    for (i in 1:(length(dates) - 1)){
      col_i <- dates[i]
      for (j in (i+1):length(dates)){
        col_j <- dates[j]
        newColName <- paste(col_i, "Minus", col_j, sep = name_separator)
        #set(dataSet, NULL, newColName, diffTime(dataSet[[col_i]], dataSet[[col_j]], units = units))
		    dataSet[, c(newColName) := diffTime(dataSet[[col_i]], dataSet[[col_j]], units = units)]
      }  
    }
    rm(i, j, col_i, col_j)
  }
  
  # If there was an analysisDate
  if (!is.null(analysisDate) & length(dates) > 0){
    for (col in dates){
      newColName <- paste(col, "Minus", "analysisDate", sep = name_separator)
      # Doesn't work, issue putted on data.table
      #set(dataSet, NULL, newColName, diffTime(dataSet[[col]], analysisDate, units = units))
	    dataSet[, c(newColName) := diffTime(dataSet[[col]], analysisDate, units = units)]
    }
  }
  
  ## wrapp-up
  return(dataSet)
}


#######################################################################################
############################### Unify dates types #####################################
#######################################################################################

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
    stop("Sorry this unit hasn\'t been implemented yet")
  }
}



#######################################################################################
############################### Unify dates types #####################################
#######################################################################################
#' Unify dates format
#'
#' Unify every column in a date format to the same date format
#' @param dataSet Matrix, data.frame or data.table
#' @param format desired target format: Date, POSIXct or POSIXlt, (character, default to Date)
#' @details 
#' This function only handle Date, POSIXct and POSIXlt dates. 
#' POSIXct format is a bit slower than Date but can keep hours-min.
#' @return The same dataSet set but with dates column with the desired format
#' @export
#' @examples
#' # build a data.table
#' dataSet <- data.table( column1 = as.Date("2016-01-01"), column2 = as.POSIXct("2017-01-01") )
#'
#' # Use the function
#' dataSet = dateFormatUnifier(dataSet, format = "Date")
#'
#' # Control result
#' sapply(dataSet, class)
#' # return date for both column
dateFormatUnifier <- function(dataSet, format = "Date"){
  ## Working environement
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  if (! any(format %in% c("Date", "POSIXct", "POSIXlt"))){
    stop(paste("dateFormatUnifier: only format: Date, POSXIct, POSIXlt are implemented. You gave:", format))
  }
  
  ## Initialization
  dates <- names(dataSet)[sapply(dataSet, is.date)]
  format_function <- paste0("as.", format)
  
  ## Computation
  for ( col in dates){
    # Only change dates that don't have the right format
    if (! format %in% class(dataSet[[col]])){
      set(dataSet, NULL, col, get(format_function)(dataSet[[col]]))  
    }
  }
  
  ## Wrapp-up
  return(dataSet)
}

#######################################################################################
############################### Is in a date format  ##################################
#######################################################################################
# Check if an object is in a date format
# Are handeled: Date, POSIXct, POSIXlt 	
# Excension of is.Date, is.POSIXct...
is.date <- function(x){
  return(any(class(x) %in% c("Date", "POSIXct", "POSIXt", "POSIXlt")))
}


########################################################################################
########################################### getPossibleDatesFormats ####################
########################################################################################
## Ensemble of formats
getPossibleDatesFormats <- function(date_sep =  c("," , "/", "-", "_", ":"), hours = TRUE){
  ## Initialization
  hours_format <- c("%H:%M:%S", "%H:%M", "%H")
  base_year <- c("%Y", "%y")
  base_month <- c("%b", "%B", "%m")
  base_day <- c("%d", "%a", "%A")
  
  ## Computation
  # Build list of possible formats
  datesFormats <- NULL
  for (separator in date_sep){
    for (year in base_year){
      for (month in base_month){
        for (day in base_day){
          tempList <- c(
            paste(year, month, day, sep = separator), 
            paste(year, day, month, sep = separator), 
            paste(day, month, year, sep = separator), 
            paste(month, day, year, sep = separator)
          )
          datesFormats <- c(datesFormats, tempList)
        }
      }
    }
  }
  
  # Complete the list with the same formats but with a time format at the and separed by a ' '
  formats <- c(datesFormats)
  if (hours){
    for (datesFormat in datesFormats){
      for (hoursFormat in hours_format){
        formats <- c(formats, paste(datesFormat, hoursFormat))
      }
    }
  }
  
  ## Wrapp-up
  return(formats)
}

#######################################################################################
############################### Format for parse_date_time ############################
#######################################################################################
# Code is commented and result hard written so that it's way faster. You should keep it that way
formatForparse_date_time<- function(){
  # # Get complete liste of format
  # listOfFastFormat = getPossibleDatesFormats()
  # result = NULL
  # for (format in listOfFastFormat){
  #   temp =parse_date_time(format(Sys.Date(), format), orders = format)== Sys.Date()
  #   if (is.na(temp)){
  #     temp = FALSE
  #   }
  #   if (temp){
  #     result =c(result, format)
  #   }
  # }
  # result = sapply(result, function(x)str_replace_all(x, "[[:punct:]]", ""))
  # result = unique(result)
  
  result <- c("Ybd", "Ydb", "dbY", "bdY", "YBd", "YdB", "dBY", "BdY", "Ymd", "Ydm", "dmY", "mdY", "ybd", "ydb", "dby", "bdy", "yBd", "ydB", "dBy", "Bdy", "ymd", "ydm", "dmy", "mdy", "Ybd HMS", "Ybd HM", "Ybd H", "Ydb HMS", "Ydb HM", "Ydb H", "dbY HMS", "dbY HM", "dbY H", "bdY HMS", "bdY HM", "bdY H", "YBd HMS", "YBd HM", "YBd H", "YdB HMS", "YdB HM", "YdB H", "dBY HMS", "dBY HM", "dBY H", "BdY HMS", "BdY HM", "BdY H", "Ymd HMS", "Ymd HM", "Ymd H", "Ydm HMS", "Ydm HM", "Ydm H", "dmY HMS", "dmY HM", "dmY H", "mdY HMS", "mdY HM", "mdY H", "ybd HMS", "ybd HM", "ybd H", "ydb HMS", "ydb HM", "ydb H", "dby HMS", "dby HM", "dby H", "bdy HMS", "bdy HM", "bdy H", "yBd HMS", "yBd HM", "yBd H", "ydB HMS", "ydB HM", "ydB H", "dBy HMS", "dBy HM", "dBy H", "Bdy HMS", "Bdy HM", "Bdy H", "ymd HMS", "ymd HM", "ymd H", "ydm HMS", "ydm HM", "ydm H", "dmy HMS", "dmy HM", "dmy H", "mdy HMS", "mdy HM", "mdy H")
  
  
  ## Wrapp-up
  return(result)
}
