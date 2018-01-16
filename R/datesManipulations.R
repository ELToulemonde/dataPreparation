###################################################################################
########################### findAndTransformDates #################################
###################################################################################
#' Identify date columns
#' 
#' Find and transform dates that are hidden in a character column. \cr
#' It use a bunch of default formats, and you can also add your own formats.
#' @param dataSet Matrix, data.frame or data.table
#' @param formats List of additional Date formats to check (see \code{\link{strptime}})
#' @param n_test Number of non-null rows on which to test (numeric, default to 30)
#' @param ambiguities How ambiguities should be treated (see details in ambiguities section)
#' (character, default to IGNORE)
#' @param verbose Should the algorithm talk? (Logical, default to TRUE)
#' @details 
#' This function is looking for perfect transformation. 
#' If there are some mistakes in dataSet, consider setting them to NA before. \cr
#' In the unlikely case where you have numeric higher than \code{as.numeric(as.POSIXct("1990-01-01"))}
#' they will be considered as timestamps and you might have some issues. On the other side, 
#' if you have timestamps before 1990-01-01, they won't be found, but you can use 
#' \code{\link{setColAsDate}} to force transformation.
#' @section Ambiguity:
#' Ambiguities are often present in dates. For example, in date: 2017/01/01, there is no way to know
#'  if format is YYYY/MM/DD or YYYY/DD/MM. \cr
#' Some times ambiguity can be solved by a human. For example
#'  17/12/31, a human might guess that it is YY/MM/DD, but there is no sure way to know.  \cr
#' To be safe, findAndTransformDates doesn't try to guess ambiguities. \cr
#' To answer ambiguities problem, param \code{ambiguities} is now available. It can take one of the following values
#' \itemize{
#' \item \code{IGNORE} function will then take the first format which match (fast, but can make some mistakes)
#' \item \code{WARN} function will try all format and tell you - via prints - that there are multiple matches (and won't perform date transformation)
#' \item \code{SOLVE} function will try to solve ambiguity by going through more lines, so will be slower. 
#' If it is able to solve it, it will transform the column, if not it will print the various acceptable formats.
#' }
#' @return dataSet set (as a data.table) with identified dates transformed by \strong{reference}.
#' @examples
#' # Load exemple set
#' data(messy_adult)
#' head(messy_adult)
#' # using the findAndTransformDates
#' findAndTransformDates(messy_adult, n_test = 5)
#' head(messy_adult)
#' 
#' # Example with ambiguities
#' \dontrun{
#' require(data.table)
#' data(messy_adult) # reload data
#' # Add an ambiguity by sorting date1
#' messy_adult$date1 = sort(messy_adult$date1, na.last = TRUE)
#' # Try all three methods:
#' result_1 = findAndTransformDates(copy(messy_adult))
#' result_2 = findAndTransformDates(copy(messy_adult), ambiguities = "WARN")
#' result_3 = findAndTransformDates(copy(messy_adult), ambiguities = "SOLVE")
#' }
#' # "##NOT RUN:" mean that this example hasn't been run on CRAN since its long. But you can run it!
#' @export
findAndTransformDates <- function(dataSet, formats = NULL, n_test = 30, ambiguities = "IGNORE", verbose = TRUE){
  ## Working environement
  function_name <- "findAndTransformDates"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  is.verbose(verbose)
  if (!is.character(ambiguities) || ! ambiguities %in% c("IGNORE", "WARN", "SOLVE")){
    stop(paste0(function_name, ": ambiguities should be either IGNORE, WARN or SOLVE."))
  }
  ## initialization
  start_time <- proc.time()
  
  ## Computation
  # First we find dates
  dates <- identifyDates(dataSet, formats = formats, n_test = n_test, ambiguities = ambiguities, verbose = verbose)
  if (verbose){
    printl(function_name, ": It took me ", round( (proc.time() - start_time)[[3]], 2), "s to identify formats")
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
    printl(function_name, ": It took me ", round( (proc.time() - start_time)[[3]], 2), "s to transform ", length(dates$dates), " columns to a Date format.")
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
identifyDates <- function(dataSet, formats = NULL, n_test = 30, ambiguities = "IGNORE", verbose = TRUE, ...){
  ## Working environement
  function_name <- "identifyDates"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  n_test <- control_nb_rows(dataSet = dataSet, nb_rows = n_test, function_name = function_name, variable_name = "n_test")
  is.verbose(verbose)
  
  ## Initialization
  dates <- NULL
  formats <- NULL
  date_sep <-  getPossibleSeparators()
  ## Computation
  if (verbose){ 
    pb <- initPB(function_name, names(dataSet))
  }
  for ( col in names(dataSet) ){ 
    # We search dates only in characters
    if (is.character(dataSet[[col]]) || is.numeric(dataSet[[col]]) || (is.factor(dataSet[[col]]) & is.character(levels(dataSet[[col]])))){
      # Get a few lines that aren't NA, NULL nor ""
      if (is.factor(dataSet[[col]])){ # If it's a factor, we take levels to convert
        data_sample <- findNFirstNonNull(levels(dataSet[[col]]), n_test)
      }
      else{
        data_sample <- findNFirstNonNull(dataSet[[col]], n_test)
      }
      
      # We check only columns that contains something (not NA, NULL, "")
      if (length(data_sample) > 0){ 
        if (is.character(data_sample)){
          # Identify potentially used separator by checking it split date in at last two elements
          date_sep_tmp <- date_sep[sapply(date_sep, function(x)length(grep(x, data_sample))) > 0] 
          if (length(date_sep_tmp) == 0){
            next() # No separator means no dates
          }
          # Check formats with "date_hours" only if there are more than 10 characters
          date_hours <- max(sapply(data_sample, nchar), na.rm = TRUE) > 10 
          # Debug warning
          if (is.na(date_hours) || is.infinite(date_hours)){ 
            warning(paste0(function_name, ": error i shouldn't be there. Please report bug on GitHub.",  ))
            date_hours <- FALSE
          }
          # Build list of all formats to check
          defaultDateFormats <- getPossibleDatesFormats(date_sep_tmp, date_hours = date_hours)
          formats_tmp <- unique(c(defaultDateFormats, formats))
          
          # Look for the good format
          format <- identifyDatesFormats(dataSet = data_sample, formats = formats_tmp, ambiguities = ambiguities)
          if (length(format) > 1 & ambiguities != "IGNORE"){ # There is an ambiguity (second check is redondant but for code understanding)
            
            if (ambiguities == "SOLVE"){
              if (verbose){
                printl(function_name, ": column ", col, " seems to have an ambiguity, I try to solve it.")
              }
              n_prev <- 30 # Keep track of previous n to make sure that while loop stops.
              n <- 30 * 10
              while (length(format) > 1 & n_prev != n){
                n = max(n_prev * 10, nrow(dataSet))
                if (is.factor(dataSet[[col]])){ # If it's a factor, we take levels to convert
                  data_sample <- findNFirstNonNull(levels(dataSet[[col]]), n)
                }
                else{
                  data_sample <- findNFirstNonNull(dataSet[[col]], n)
                }
                format <- identifyDatesFormats(dataSet = data_sample, formats = format, ambiguities = ambiguities)
                
              }
            }
            if (length(format) > 1 & verbose){ # If ambiguity wasn't solved
              printl(function_name, ": column ", col, " seems to be a date but there is an ambiguity in the format. ",
                     " The following formats seems to work: ", paste(format, collapse = ", "))
              format <- NULL
            }
          }
        }
        if (is.numeric(data_sample)){
          format <- identifyTimeStampsFormats(dataSet = data_sample)
        }
        # If a format has been found we note it
        if (! is.null(format)){ 
          if (length(format) == 1){
            dates <- c(dates, col)
            formats <- c(formats, format)  
          }
        }
      }
    }
    
    if (verbose){
      setPB(pb, col)
    }
  }
  ## Wrapp-up
  return(list(dates = dates, formats = formats))
}

## To-do
# Handle month as french or english character
# Better output format?
# Compute expected string length if format is good? 




############################################################################################################
########################################### identifyDatesFormats ################################################
############################################################################################################
# 
# 
identifyDatesFormats <- function(dataSet, formats, ambiguities="IGNORE"){
  ## Working environement
  function_name <- "identifyDatesFormats"
  ## Sanity check
  if (! is.character(dataSet)){
    stop(paste0(function_name, ": dataSet should be some characters."))
  }
  
  ## Initalization
  
  ## Computation
  temp_format <- NULL
  for (format in formats){
    converted <- as.POSIXct(dataSet, format = format)
    un_converted <- format(converted, format = format)
    if (control_date_conversion(un_converted, dataSet)){
      if (ambiguities == "IGNORE"){
        return(format)  
      }
      else{
        temp_format <- c(temp_format, format)
      }
    }
  }
  return(temp_format)
}


# identify time_stamps_formats
identifyTimeStampsFormats <- function(dataSet){
  ## Working environement
  function_name <- "identifyTimeStampsFormats"
  ## Sanity check
  if (! is.numeric(dataSet)){
    stop(paste0(function_name, ": dataSet should be some numerics."))
  }
  
  # Expect timestamp in seconds
  date <- as.POSIXct(dataSet, origin = "1970-01-01 00:00:00")
  year <- as.numeric(format(date, "%Y"))
  
  if (all(year > 1990) & all(year < 2100)) {
    return ("s")
  }
  
  # Expect timestamp in milliseconds
  date <- as.POSIXct(dataSet / 1000, origin = "1970-01-01 00:00:00")
  year <- as.numeric(format(date, "%Y"))
  
  if (all(year > 1990) & all(year < 2100)) {
    return ("ms")
  }
  # Not a date
  return (NULL)
}

## Control that date is the same (with more checks like: without 0 or tolower? or boths?)
# To-do find a cleaner way to test all comparaisons
control_date_conversion <- function(un_converted, original){
  without_0 <- gsub("(?<=^|(?![:])[[:punct:]])0", "", un_converted, perl = TRUE)
  tolowers <- tolower(un_converted)
  tolowers_without_0 <- tolower(without_0)
  without_first_0 <- sub("^0", "", un_converted)
  tolowers_without_first_0 <- tolower(without_first_0)
  un_con_list <- list(un_converted, without_0, tolowers, tolowers_without_0, without_first_0, tolowers_without_first_0)
  
  original_with_spaces_after_sep <- gsub("(?<=[[:punct:]]) ", "", original, perl = TRUE)
  original_trimws <- trimws(original)
  original_with_spaces_after_sep_trimws <- trimws(original_with_spaces_after_sep)
  original_list <- list(original, original_with_spaces_after_sep, original_with_spaces_after_sep_trimws, original_trimws)
  return(any(un_con_list %in% original_list))
}


#######################################################################################
############################### Unify dates types #####################################
#######################################################################################
#' Unify dates format
#'
#' Unify every column in a date format to the same date format.
#' @param dataSet Matrix, data.frame or data.table
#' @param format Desired target format: Date, POSIXct or POSIXlt, (character, default to Date)
#' @details 
#' This function only handle Date, POSIXct and POSIXlt dates.  \cr
#' POSIXct format is a bit slower than Date but can keep hours-min.
#' @return The same dataSet set but with dates column with the desired format.
#' @examples
#' # build a data.table
#' require(data.table)
#' dataSet <- data.table( column1 = as.Date("2016-01-01"), column2 = as.POSIXct("2017-01-01") )
#'
#' # Use the function
#' dataSet = dateFormatUnifier(dataSet, format = "Date")
#'
#' # Control result
#' sapply(dataSet, class)
#' # return Date for both columns
#' @import data.table
#' @export
dateFormatUnifier <- function(dataSet, format = "Date"){
  ## Working environement
  function_name <- "dateFormatUnifier"
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  if (! any(format %in% c("Date", "POSIXct", "POSIXlt"))){
    stop(paste0(function_name, ": only format: Date, POSXIct, POSIXlt are implemented. You gave: ", format, "."))
  }
  
  ## Initialization
  date_cols <- names(dataSet)[sapply(dataSet, is.date)]
  format_function <- paste0("as.", format)
  
  ## Computation
  for ( col in date_cols){
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
getPossibleDatesFormats <- function(date_sep =  c("," , "/", "-", "_", ":"), date_hours = TRUE){
  ## Initialization
  hours_format <- c("%H:%M:%S", "%H:%M", "%H")
  base_year <- c("%Y", "%y")
  base_month <- c("%b", "%B", "%m")
  base_day <- c("%d", "%a", "%A")
  
  ## Computation
  # Build list of possible formats
  datesFormats <- NULL
  for (separator_1 in date_sep){
    for (separator_2 in date_sep){
      for (year in base_year){
        for (month in base_month){
          for (day in base_day){
            tempList <- c(
              paste(paste(year, month, sep = separator_1), day, sep = separator_2),
              paste(paste(year, day, sep = separator_1), month, sep = separator_2),
              paste(paste(day, month, sep = separator_1), year, sep = separator_2),
              paste(paste(month, day, sep = separator_1), year, sep = separator_2)
            )
            
            datesFormats <- c(datesFormats, tempList)
          }
        }
      }
    }
  }
  
  # Complete the list with the same formats but with a time format at the and separed by a ' ' or a "T" and optionaly with a "Z" at the end
  formats <- c(datesFormats, hours_format)
  if (date_hours){
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
  # listOfFastFormat <- getPossibleDatesFormats()
  # result <- NULL
  # for (format in listOfFastFormat){
  #   temp <- parse_date_time(format(Sys.Date(), format), orders = format)== Sys.Date()
  #   if (is.na(temp)){
  #     temp <- FALSE
  #   }
  #   if (temp){
  #     result <- c(result, format)
  #   }
  # }
  # result <- sapply(result, function(x)str_replace_all(x, "[[:punct:]]", ""))
  # result <- unique(result)
  
  result <- c("Ybd", "Ydb", "dbY", "bdY", "YBd", "YdB", "dBY", "BdY", "Ymd", "Ydm", "dmY", "mdY", "ybd", "ydb", "dby", "bdy", "yBd", "ydB", "dBy", "Bdy", "ymd", "ydm", "dmy", "mdy", "Ybd HMS", "Ybd HM", "Ybd H", "Ydb HMS", "Ydb HM", "Ydb H", "dbY HMS", "dbY HM", "dbY H", "bdY HMS", "bdY HM", "bdY H", "YBd HMS", "YBd HM", "YBd H", "YdB HMS", "YdB HM", "YdB H", "dBY HMS", "dBY HM", "dBY H", "BdY HMS", "BdY HM", "BdY H", "Ymd HMS", "Ymd HM", "Ymd H", "Ydm HMS", "Ydm HM", "Ydm H", "dmY HMS", "dmY HM", "dmY H", "mdY HMS", "mdY HM", "mdY H", "ybd HMS", "ybd HM", "ybd H", "ydb HMS", "ydb HM", "ydb H", "dby HMS", "dby HM", "dby H", "bdy HMS", "bdy HM", "bdy H", "yBd HMS", "yBd HM", "yBd H", "ydB HMS", "ydB HM", "ydB H", "dBy HMS", "dBy HM", "dBy H", "Bdy HMS", "Bdy HM", "Bdy H", "ymd HMS", "ymd HM", "ymd H", "ydm HMS", "ydm HM", "ydm H", "dmy HMS", "dmy HM", "dmy H", "mdy HMS", "mdy HM", "mdy H")
  
  
  ## Wrapp-up
  return(result)
}
