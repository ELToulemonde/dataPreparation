#######################################################################################
############################### which are constant ####################################
#######################################################################################
#' Identify constant columns
#' 
#' Find all the columns that are constant. 
#' @param dataSet Matrix, data.frame or data.table
#' @param verbose Should the algorithm talk (logical, default to TRUE)
#' @return List of column's indexes that are constant in the dataSet set.
#' @details
#' Algorithm is constance equality by exponential search: it check constance on row 1 to 10, 
#' if it's ,not constant it stops, if it's constant then on 11 to 100 ... \cr
#' If you have a lot of columns than aren't constant, this function is way faster than a simple 
#' length(unique())! The larger the dataSet set is, the more interesting it is to use this function.
#' @examples
#' # Let's create a dataSet set to test
#' dataSet <- data.table(constantCol = rep("a",100), nonConstantCol = rnorm(100))
#' 
#' # Lets try our function
#' whichAreConstant(dataSet)
#' # Indeed it return constantCol the name of the constant column.
#' @export
whichAreConstant <- function(dataSet, verbose = TRUE){
  ## Working environement
  function_name = "whichAreConstant"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  
  ## Initialization
  listOfConstantCols <- NULL
  start_time <- proc.time()
  if (verbose){
	pb <- initPB(function_name, names(dataSet))
  }
  
  ## Computation 
  if (nrow(dataSet) > 1 ){ # We check for constant only if there are at least two lines
    for (col in names(dataSet)){
      isConstant <- fastMaxNbElt(dataSet[[col]], 1)
      if (isConstant == TRUE){
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
#' # Lets change the column 2, line 1 to 0. And check again
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
#' @importFrom tcltk tkProgressBar setTkProgressBar
whichAreInDouble <- function(dataSet, verbose = TRUE){
  ## Working environement
  function_name = "whichAreInDouble"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  
  
  ## Initialization
  if (ncol(dataSet) <= 1){ # If there are less than 1 column we do nothing
    return(NULL)
  }
  listOfDoubles <- NULL
  I <- 1:max(ncol(dataSet) - 1, 1) 
  start_time <- proc.time()
  if (verbose){
	pb <- initPB(function_name, names(dataSet))
  }
  ## Computation # to-do dé-gorifier
  while (length(I) > 0){
    i <- I[1]
    
    J <- (i+1):ncol(dataSet)
    J <- J[!J %in% listOfDoubles]
    while (length(J)>0){
      j <- J[1]
      if ( fastIsEqual(dataSet[[i]], dataSet[[j]])){
        listOfDoubles <- c(listOfDoubles, j)
		if(verbose){
		  printl(function_name, ": ", names(dataSet)[j], " is exactly equal to ", names(dataSet)[i], ". I put it in drop list.")
		}
      }
      J <- J[-1] # drop handled j
    }
    I <- I[!I %in% listOfDoubles]
    I <- I[-1] # drop handled i
    if (verbose){
	  setPB(pb, names(dataSet)[i])
    }
  }
  if (verbose){
    close(pb); rm(pb); gc()
  }
  ## Wrapp up
  listOfDoubles <- unique(listOfDoubles)
  if (verbose){
    printl(function_name, ": it took me ", round((proc.time() - start_time)[[3]], 2), "s to identify ", length(listOfDoubles), " double(s)")
  }
  
  return(listOfDoubles)
}



#######################################################################################
############################### Fast find bijections ##################################
#######################################################################################
#' Identify bijections
#' 
#' Find all the columns that are bijections of another column
#' @param dataSet Matrix, data.frame or data.table
#' @param verbose Should the algorithm talk (logical, default to TRUE)
#' @return A list of index of columns that have an exact bijection in the dataSet set. 
#' @details 
#' Bijection, meaning that there is another column containing the exact same information (but maybe
#'  coded differently) for example col1: Men/Women, col2 M/W. \cr
#' This function is performing search by looking to every couple of columns. 
#' It computes numbers of unique elements in each columns, and number of unique tuples of values. \cr
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
#' # Lets look of bijections
#' whichAreBijection(adult)
#' # Return education_num and education which contain the same info
#' @export
#' @importFrom tcltk tkProgressBar setTkProgressBar
whichAreBijection <- function(dataSet, verbose = TRUE){
  ## Working environement
  function_name = "whichAreBijection"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  
  ## Initialization
  if (ncol(dataSet) <= 1){ # If there are less than 1 column we do nothing
    return(NULL)
  }
  listOfBijection <- NULL
  I <- 1:max(ncol(dataSet) - 1, 1) 
  start_time <- proc.time()
  
  if (verbose){
    pb <- initPB(function_name, names(dataSet))
  }
  ## Computation # to-do dé-gorifier
  while (length(I) > 0){
    i <- I[1]
    
    J <- (i+1):ncol(dataSet)
    J <- J[!J %in% listOfBijection]
    while (length(J)>0){
      j <- J[1]
      if (fastIsBijection(dataSet[, c(i, j), with = FALSE])){
        if (any(class(dataSet[[j]]) %in% c("character", "factor"))){
          # If j is a character we keep it and drop i, we prefer to have character instead of "false" numerics.
          listOfBijection <- c(listOfBijection, i)
		  if(verbose){
		    printl(function_name, ": ", names(dataSet)[i], " is a bijection of ", names(dataSet)[j], ". I put it in drop list.")
		  }
		  break # Break loop since i will be dropped.
        }
        else{
          listOfBijection <- c(listOfBijection, j)
		  if(verbose){
		    printl(function_name, ": ", names(dataSet)[j], " is a bijection of ", names(dataSet)[i], ". I put it in drop list.")
		  }
        }
      }
      J <- J[-1] # drop handled j
    }
    I <- I[!I %in% listOfBijection]
    I <- I[-1] # drop handled i
    if (verbose){
      setPB(pb, names(dataSet)[i])
    }
  }
  if (verbose){
    close(pb); rm(pb); gc()
  }
  ## Wrapp up
  listOfBijection <- unique(listOfBijection)
  if (verbose){
    printl(function_name, ": it took me ", round((proc.time() - start_time)[[3]], 2), "s to identify ", length(listOfBijection), " bijection(s)")
  }
  
  return(listOfBijection)
}




#######################################################################################
############################### Fast are included #####################################
#######################################################################################
#' Identify columns that are included in others
#' 
#' Find all the columns that don't contain more information than another column. For example if 
#' you have a column with an amount, and another with the same amount but rounded, the second 
#' column is included in the first.
#' @param dataSet Matrix, data.frame or data.table
#' @param verbose Should the algorithm talk (logical, default to TRUE)
#' @details 
#' This function is performing exponential search and is looking to every couple of columns. \cr
#' Be very carefull while using this function: \cr
#' - if there is an id column, it will say everything is included in the id column, \cr
#' - the order of columns will influence the result.\cr
#' \cr
#' And last but not least, sing machine learning algorithm it's not always smart to drop columns
#'  even if they don't give more info: the extrem example is the id example.
#' @return A list of index of columns that have an exact duplicate in the dataSet set.
#' @examples
#' # Load toy data set
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
#' # But you should be carefull, if there is a column id, every column will be dropped:
#' messy_adult$id = 1:nrow(messy_adult) # build id
#' setcolorder(messy_adult, c("id", setdiff(names(messy_adult), "id"))) # Set id as first column
#' whichAreIncluded(messy_adult)
#' @export
#' @importFrom tcltk tkProgressBar setTkProgressBar
whichAreIncluded <- function(dataSet, verbose = TRUE){
  ## Working environement
  function_name <- "whichAreIncluded"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  
  ## Initialization
  if (ncol(dataSet) <= 1){ # If there are less than 1 column we do nothing
    return(NULL)
  }
  listOfIncluded <- NULL
  I <- 1:max(ncol(dataSet) - 1, 1) 
  
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
      
      temp_data <- dataSet[, c(i, j), with = FALSE]
      temp_data <- temp_data[!duplicated(temp_data)]
      
      if (nrow(temp_data) == nbr_various_val[i]){
          listOfIncluded <- c(listOfIncluded, j)
		  if(verbose){
		    printl(function_name, ": ", names(dataSet)[j], " is included in column ", names(dataSet)[i], ".")
		  }
      }
      else{
        if (nrow(temp_data) == nbr_various_val[j]){
          listOfIncluded <- c(listOfIncluded, i)
		  if(verbose){
		    printl(function_name, ": ", names(dataSet)[i], " is included in column ", names(dataSet)[j], ".")
		  }
        }
      }
      
      J <- J[-1] # drop handled j
    }
    I <- I[!I %in% listOfIncluded]
    I <- I[-1] # drop handled i
    if (verbose){
      setPB(pb, names(dataSet)[i])
    }
  }
  if (verbose){
    close(pb); rm(pb); gc()
  }
  ## Wrapp up
  listOfIncluded <- unique(listOfIncluded)
  
  return(listOfIncluded)
}
