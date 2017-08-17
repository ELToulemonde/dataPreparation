#######################################################################################
############################### which are constant ####################################
#######################################################################################
#' Identify constant columns
#' 
#' Find all the columns that are constant. 
#' @param dataSet Matrix, data.frame or data.table
#' @param keep_cols list of columns not to drop (list of character, default to NULL)
#' @param verbose Should the algorithm talk (logical, default to TRUE)
#' @return List of column's indexes that are constant in the dataSet set.
#' @details
#' Algorithm is performing exponential search: it check constancy on row 1 to 10, 
#' if it's not constant it stops, if it's constant then on 11 to 100 ... \cr
#' If you have a lot of columns than aren't constant, this function is way faster than a simple 
#' length(unique())! The larger the dataSet set is, the more interesting it is to use this function.
#' @examples
#' # Let's load our dataSet
#' data("messy_adult") 
#'
#' # Let's try our function
#' whichAreConstant(messy_adult)
#' # Indeed it return constant the name of the constant column.
#' @import data.table
#' @export
whichAreConstant <- function(dataSet, keep_cols = NULL, verbose = TRUE){
  ## Working environement
  function_name <- "whichAreConstant"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  is.verbose(verbose)
  keep_cols <- real_cols(keep_cols, names(dataSet), function_name)
  
  ## Initialization
  listOfConstantCols <- NULL
  if (verbose){
	start_time <- proc.time()
    pb <- initPB(function_name, names(dataSet))
  }
  cols <- names(dataSet)[! names(dataSet) %in% keep_cols]
  
  ## Computation 
  if (nrow(dataSet) > 1 ){ # We check for constant only if there are at least two lines
    for (col in cols){
      if (fastMaxNbElt(dataSet[[col]], 1)){
        listOfConstantCols <- c(listOfConstantCols, col)
        if(verbose){
          printl(function_name, ": ", col, " is constant.")
        }
      }
      if(verbose){
        setPB(pb, col)
      }
    }
  }
  if (verbose){
    close(pb); rm(pb); gc()
  }
  
  ## Wrapp-up
  listOfConstantCols <- which(names(dataSet) %in% listOfConstantCols) # To return indexes
  if (verbose){
    printl(function_name, ": it took me ", round((proc.time() - start_time)[[3]], 2), 
           "s to identify ", length(listOfConstantCols), " constant column(s)")
  }
  
  return(listOfConstantCols)
}



#######################################################################################
############################### which are in double ###################################
#######################################################################################
#' Identify double columns
#' 
#' Find all the columns that are in double. 
#' @param dataSet Matrix, data.frame or data.table
#' @param keep_cols list of columns not to drop (list of character, default to NULL)
#' @param verbose Should the algorithm talk (logical, default to TRUE)
#' @return 
#' A list of index of columns that have an exact duplicate in the dataSet set. 
#' Ex: if column i and column j (with j > i) are equal it will return j.
#' @details 
#' This function is performing search by looking to every couple of columns. First it compares the 
#' first 10 lines of both columns. If they are not equal then the columns aren't identical, else
#' it compares lines 11 to 100; then 101 to 1000... So this function is fast with dataSet set 
#' with a large number of lines and a lot of columns that aren't equals. \cr
#' If \code{verbose} is TRUE, the column logged will be the one returned. \cr
#' @examples
#' # First let's build a matrix with 3 columns and a lot of lines, with 1's everywhere
#' M <- matrix(1, nrow = 1e6, ncol = 3)
#'
#' # Now let's check which columns are equals
#' whichAreInDouble(M)
#' # It return 2 and 3: you should only keep column 1.
#'
#' # Let's change the column 2, line 1 to 0. And check again
#' M[1, 2] <- 0
#' whichAreInDouble(M)
#' # It only returns 3
#'
#' # What about NA? NA vs not NA => not equal
#' M[1, 2] <- NA
#' whichAreInDouble(M)
#' # It only returns 3
#'
#' # What about NA?  Na vs NA => yep it's the same
#' M[1, 1] <- NA
#' whichAreInDouble(M)
#' # It only returns 2
#' @export
whichAreInDouble <- function(dataSet, keep_cols = NULL, verbose = TRUE){
  ## Working environement
  function_name = "whichAreInDouble"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  is.verbose(verbose)
  keep_cols <- real_cols(keep_cols, names(dataSet), function_name)
  
  ## Initialization
  
  ## Computation
  listOfDoubles <- bi_col_test(dataSet, keep_cols = keep_cols, verbose = verbose,
                               test_function = "fastIsEqual", function_name = function_name, test_log = " is exactly equal to ")
  
  ## Wrapp-up
  return(listOfDoubles)
}



#######################################################################################
############################### Fast find bijections ##################################
#######################################################################################
#' Identify bijections
#' 
#' Find all the columns that are bijections of another column
#' @param dataSet Matrix, data.frame or data.table
#' @param keep_cols list of columns not to drop (list of character, default to NULL)
#' @param verbose Should the algorithm talk (logical, default to TRUE)
#' @return A list of index of columns that have an exact bijection in the dataSet set. 
#' @details 
#' Bijection, meaning that there is another column containing the exact same information (but maybe
#'  coded differently) for example col1: Men/Women, col2 M/W. \cr
#' This function is performing search by looking to every couple of columns. 
#' It computes numbers of unique elements in each column, and number of unique tuples of values. \cr
#' Computation is made by exponential search, so that the function is faster. \cr
#' If \code{verbose} is TRUE, the column logged will be the one returned. \cr
#' Ex: if column i and column j (with j > i) are bijections it will return j, expect if j is a 
#' character then it return i.
#' @examples
#' # First let's get a data set
#' data("adult")
#'
#' # Now let's check which columns are equals
#' whichAreInDouble(adult)
#' # It doesn't give any result.
#'
#' # Let's look of bijections
#' whichAreBijection(adult)
#' # Return education_num index because education_num and education which
#' # contain the same info
#' @export
whichAreBijection <- function(dataSet, keep_cols = NULL, verbose = TRUE){
  ## Working environement
  function_name <- "whichAreBijection"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  is.verbose(verbose)
  keep_cols <- real_cols(keep_cols, names(dataSet), function_name)
  
  ## Initialization

  ## Computation # to-do dé-gorifier
  listOfBijection <- bi_col_test(dataSet, keep_cols, verbose = verbose, 
                                 test_function = "fastIsBijection", function_name = function_name, test_log = " is a bijection of ")
  
  ## Wrapp up
  return(listOfBijection)
}




#######################################################################################
############################### Fast are included #####################################
#######################################################################################
#' Identify columns that are included in others
#' 
#' Find all the columns that don't contain more information than another column. For example if 
#' you have a column with an amount and another with the same amount but rounded, the second 
#' column is included in the first.
#' @param dataSet Matrix, data.frame or data.table
#' @param keep_cols list of columns not to drop (list of character, default to NULL)
#' @param verbose Should the algorithm talk (logical, default to TRUE)
#' @details 
#' This function is performing exponential search and is looking to every couple of columns. \cr
#' Be very careful while using this function: \cr
#' - if there is an id column, it will say everything is included in the id column; \cr
#' - the order of columns will influence the result.\cr
#' \cr
#' And last but not least, with some machine learning algorithm it's not always smart to drop 
#' columns even if they don't give more info: the extreme example is the id example.
#' @return A list of index of columns that have an exact duplicate in the dataSet set.
#' @examples
#' # Load toy data set
#' require(data.table)
#' data(messy_adult)
#' 
#' # Check for included columns
#' whichAreIncluded(messy_adult)
#' 
#' # Return columns that are also constant, double and bijection
#' # Let's add a truly just included column
#' messy_adult$are50OrMore <- messy_adult$age > 50
#' whichAreIncluded(messy_adult)
#' 
#' # As one can, see this column that doesn't have additional info than age is spotted.
#' 
#' # But you should be careful, if there is a column id, every column will be dropped:
#' messy_adult$id = 1:nrow(messy_adult) # build id
#' whichAreIncluded(messy_adult)
#' @export
whichAreIncluded <- function(dataSet, keep_cols = NULL, verbose = TRUE){
  ## Working environement
  function_name <- "whichAreIncluded"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  is.verbose(verbose)
  keep_cols <- real_cols(keep_cols, names(dataSet), function_name)
  ## Initialization
  if (ncol(dataSet) <= 1){ # If there are less than 1 column we do nothing
    return(NULL)
  }
  listOfIncluded <- NULL
  I <- 1:max(ncol(dataSet) - 1, 1) 
  keep_cols_index <- which(names(dataSet) %in% keep_cols)
  if (verbose){
    pb <- initPB(function_name, names(dataSet))
  }
  nbr_various_val <- sapply(dataSet, uniqueN)
  ## Computation # to-do dé-gorifier
  while (length(I) > 0){
    i <- I[1]
    
    J <- (i+1):ncol(dataSet)
    J <- J[!J %in% listOfIncluded]
    while (length(J) > 0){
      j <- J[1]
      if (! all(c(i, j) %in% keep_cols_index)){
        temp_data <- dataSet[, c(i, j), with = FALSE]
        temp_data <- temp_data[!duplicated(temp_data)]
        
        if (nrow(temp_data) == nbr_various_val[i] & ! j %in% keep_cols_index){
          listOfIncluded <- c(listOfIncluded, j)
          if(verbose){
            printl(function_name, ": ", names(dataSet)[j], " is included in column ", names(dataSet)[i], ".")
          }
        }
        else{
          if (nrow(temp_data) == nbr_various_val[j] & ! i %in% keep_cols_index){
            listOfIncluded <- c(listOfIncluded, i)
            if(verbose){
              printl(function_name, ": ", names(dataSet)[i], " is included in column ", names(dataSet)[j], ".")
            }
            break # Break loop since i will be dropped.
          }
        } 
      }
      J <- J[-1] # drop handled j
    }
    I <- I[-1] # drop handled i
    I <- I[!I %in% listOfIncluded]
    if (verbose){
      setPB(pb, names(dataSet)[i])
    }
  }
  if (verbose){
    close(pb); rm(pb); gc()
  }
  ## Wrapp up
  if (! is.null(listOfIncluded)){
    listOfIncluded <- sort(unique(listOfIncluded))
  }  
  return(listOfIncluded)
}

###################################################################################################
############################### Bi test function  #################################################
###################################################################################################
# Internal function to compute test on couple of columns
# @param dataSet Matrix, data.frame or data.table
# @param keep_cols list of columns not to drop (list of character, default to NULL)
# @param verbose Should the algorithm talk (logical, default to TRUE)
# @param test_function function to perform test
# @param function_name function from which this function is called (for verbose)
# @param test_log what is this test result (for verbose)
bi_col_test <- function(dataSet, keep_cols = NULL, verbose = TRUE, test_function, 
                        function_name = "bi_col_test", test_log = " is a match of "){
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  is.verbose(verbose)
  keep_cols <- real_cols(keep_cols, names(dataSet), function_name)
  
  ## Initialization
  if (ncol(dataSet) <= 1){ # If there are less than 1 column we do nothing
    return(NULL)
  }
  result_list <- NULL
  col_I <- names(dataSet)[-ncol(dataSet)]
  if (verbose){
	start_time <- proc.time()
    pb <- initPB(function_name, names(dataSet))
  }
  
  ## Computation
  while (length(col_I) > 1){
    col_i <- col_I[1]
    col_J <- names(dataSet)[- (1:which(col_i == names(dataSet)))]
    col_J <- col_J[!col_J %in% result_list] # Drop cols that are alreaydy found
    for (col_j in col_J){
      # If one of the couple is droppable
      if (!all(c(col_i, col_j) %in% keep_cols)){
        # Compute test
        if (get(test_function)(dataSet[[col_i]], dataSet[[col_j]])){
          # If test is positive check which one is to drop
          if (! col_j %in% keep_cols & !is.character(dataSet[[col_j]])){
            result_list <- c(result_list, col_j)
            if (verbose){
              printl(function_name, ": ", col_j, test_log, col_i, ". I put it in drop list.")  
            }
            col_I <- col_I[col_I != col_j] # Drop it from search list
          }
          else{ # Drop i
            result_list <- c(result_list, col_i)
            if (verbose){
              printl(function_name, ": ", col_i, test_log, col_j, ". I put it in drop list.")  
            }
            break # Break loop since i will be dropped.
          }
        }
      }
    }
    if (verbose){
      setPB(pb, col_i)
    }
    col_I <- col_I[-1]
  }
  if (verbose){
    close(pb); rm(pb); gc()
  }
  ## Wrapp-up
  result_list <- which(names(dataSet) %in% result_list) # To return indexes
  if (! is.null(result_list)){
    result_list <- sort(unique(result_list))
  }
  if (verbose){
    printl(function_name, ": it took me ", round((proc.time() - start_time)[[3]], 2), "s to identify ", length(result_list), " column(s) to drop.")
  }
  return(result_list)
}
