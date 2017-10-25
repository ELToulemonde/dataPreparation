#' Describe data set
#' 
#' Generate extensive description of a data set. 
#' @param dataSet Matrix, data.frame or data.table
#' @param level Level of description (0: generic, 1: column by column) 
#' (numeric, default to 1)
#' @param path_to_write Path where the report should be written (character, default to NULL)
#' @param verbose Should the algorithm talk? (Logical, default to TRUE)
#' @examples 
#' # Load exemple set
#' data(messy_adult)
#' 
#' # Describe it
#' description(messy_adult)
#' @import data.table
#' @export
description <- function(dataSet, level = 1, path_to_write = NULL, verbose = TRUE){
  ## Working environement
  function_name <- "description"
  
  ## Sanity check
  store_class <- class(dataSet)
  dataSet <- checkAndReturnDataTable(dataSet)
  is.verbose(verbose)
  # If path to write is set, check if it's writable
  if (! level %in% c(0, 1)){
    stop(paste0(function_name, ": level should be either 0 or 1."))
  }
  
  ## Initialization
  if (!is.null(path_to_write)){
    fileLog <- path_to_write
    sink(fileLog)
  }
  ## Computation
  # Level 0: general description
  cat("##################################\n## Level 0: General description ##\n##################################\n")
  printl("dataSet is a ", paste0(store_class, collapse = "-"))
  printl("dataSet contains ", nrow(dataSet), " rows and ", ncol(dataSet), " cols.")
  printl("Columns are of the following classes:")
  print(table(sapply(dataSet, function(x)paste0(class(x), collapse = "-"))))
  
  # Level 1: Univariate description
  if (level >= 1){
    cat("#####################################\n## Level 1: univariate description ##\n#####################################\n")
	card <- dataSet[, lapply(.SD, uniqueN)]
    for (col in colnames(dataSet)){
	  # Unique or distinct values
      if (card[[col]] == 1){
		printl(col, " only has 1 value.")
	  } 
	  if (card[[col]] == nrow(dataSet)){
	    printl(col, " has all unique values.")
	  }
	  # Numerical and date cols
      if (is.numeric(dataSet[[col]]) || is.date(dataSet[[col]])){
        if (is.numeric(dataSet[[col]])){
          printl("Summary for numeric variable ", col)  
        }
        if (is.date(dataSet[[col]])){
          printl("Summary for date variable ", col)  
        }
        print(summary(dataSet[[col]]))
      }
	  # Factor cols
      if (is.factor(dataSet[[col]]) || is.logical(dataSet[[col]])){
        if (is.factor(dataSet[[col]])){
          printl("Table of occurence for factor variable ", col)  
        }
        if (is.logical(dataSet[[col]])){
          printl("Table of occurence for logical variable ", col)  
        }
        print(table(dataSet[[col]]))
      }
	  # character
      if (is.character(dataSet[[col]])){
        printl("character variable ", col, " takes ", uniqueN(dataSet[[col]]), " different values.")
      }
    }
  }
  
  
  # Level 2: Multivariate description
  # To be developed
  # if (level >= 2){
  #   cat("#######################################\n## Level 2: multivariate description ##\n#######################################\n")
  # 
  # }
  ## Wrapp-up
  if (!is.null(path_to_write)){
    sink()
  }
}
