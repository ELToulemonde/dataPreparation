#' Give same shape
#' 
#' Transform \code{dataSet} into the same shape as \code{referenceSet}. Espacially this 
#' function will be usefull to make your test set have the same shape as your train set.
#' @param dataSet Matrix, data.frame or data.table to transform
#' @param referenceSet Matrix, data.frame or data.table
#' @param verbose Should the algorithm talk? (logical, default to TRUE)
#' @return Return \code{dataSet} transformed in order to make it have the same shape as
#' \code{referenceSet}
#' @details
#' This function will make sure that \code{dataSet} and \code{referenceSet} 
#' \itemize{
#'    \item have the same class
#'    \item have exactly the same columns
#'    \item have columns with exactly the same class
#'    \item have factor factor with exactly the same levels
#' }
#' You should always use this function before applying your model on a new data set to make sure
#' that everything will go smoothly. But if this function change a lot of stuff you should have a 
#' look to your preparation process, there might be something wrong. 
#' @examples
#' \dontrun{
#' # Build a train and a test
#' data("messy_adult")
#' data("adult")
#' train <- messy_adult
#' test <- adult # So test will have missing columns
#' 
#' # Prepare them
#' train <- prepareSet(train, verbose = FALSE, key = "country")
#' test <- prepareSet(test, verbose = FALSE, key = "country")
#' 
#' # Give them the same shape
#' test <- sameShape(test, train)
#' # As one can see in log, a lot of small change had to be done. 
#' # This is an extreme case but you get the idea.
#' }
#' # "##NOT RUN:" mean that this example hasn't been run on CRAN since its long. But you can run it!
#' @import data.table
#' @export
sameShape <- function(dataSet, referenceSet, verbose = TRUE){
  ## Working environement
  function_name <- "sameShape"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  is.verbose(verbose)
  
  ## Initialization
  # Store class of reference set and transform it into data.table to make computation faster
  referenceSet_class <- class(referenceSet)
  referenceSet <- checkAndReturnDataTable(referenceSet, name = "referenceSet")
  ## Computation
  # Complete list of columns
  if (verbose){
    printl(function_name, ": verify that every column is present.")
  }
  create_list <- names(referenceSet)[! names(referenceSet) %in% names(dataSet)]
  if (length(create_list) > 0){
    if (verbose){
      printl(function_name, ": columns ", paste(create_list, collapse = ", "), " are missing, I create them.")
    }
	set(dataSet, NULL, create_list, NA)
  }
  
  # Drop unwanted columns
  if (verbose){
    printl(function_name, ": drop unwanted columns.")
  }
  drop_list <- names(dataSet)[! names(dataSet) %in% names(referenceSet)]
  if (length(drop_list) > 0){
    if (verbose){
      printl(function_name, ": the folowing columns are in dataSet but not in referenceSet: I drop them: ")
      print(drop_list)
    }
	set(dataSet, NULL, drop_list, NULL)
  }
  
  # Class of columns
  if (verbose){
    printl(function_name, ": verify that every column is in the right type.")
    pb <- initPB(function_name, names(dataSet))
  }
  for (col in names(dataSet)){
    trans_class <- class(dataSet[[col]])
    ref_class <- class(referenceSet[[col]])
    if (! all(trans_class == ref_class)){
      transfo_function <- paste0("as.", ref_class[[1]])
      if (exists(transfo_function)){
        if (verbose){
          printl(function_name, ": ", col, " class was ", trans_class, " i set it to ",
                 ref_class, ".")
        }
        set(dataSet, NULL, col, get(transfo_function)(dataSet[[col]]))
        # Control
        if (! all(class(dataSet[[col]]) == ref_class)){
           warning(paste0(function_name, ": transformation didn't work. Please control that function ", 
                          transfo_function, " indeed transform into ", ref_class, "."))
        }
      }
      else {
        warning(paste0(function_name, ": ", col, " class is ", trans_class, " but should be ", ref_class,
                       " and i don't know how to transform it."))
      }
    }
    if (verbose){
      setPB(pb, col)
    }    
  }
  gc(verbose = FALSE)
  # Factor levels
  if (verbose){
    printl(function_name, ": verify that every factor as the right number of levels.")
    pb <- initPB(function_name, names(dataSet))
  }
  for (col in names(dataSet)){
    print(is.factor(dataSet[[col]]))
    if (is.factor(dataSet[[col]])){
      transfo_levels <- levels(dataSet[[col]])
      ref_levels <- levels(referenceSet[[col]])
      print( identical(transfo_levels, ref_levels))
      if (! identical(transfo_levels, ref_levels)){
        set(dataSet, NULL, col, factor(dataSet[[col]], levels = ref_levels))
        if (verbose){
          printl(function_name, ": ", col, " class had different levels than in referenceSet I change it.")
        }
      }
    }
    if (verbose){
      setPB(pb, col)
    }    
  }
  gc(verbose = FALSE)
  
  # Re-order columns
  setcolorder(dataSet, names(referenceSet))
  
  # Set class
  if (! identical(referenceSet_class, class(dataSet))){
    if (is_class_dataframe(referenceSet_class)){
      setDF(dataSet)
    }
    if (is_class_matrix(referenceSet_class)){
      dataSet <- as.matrix(dataSet)
    }
  }
  
  ## Wrapp-up
  return(dataSet)
}

is_class_dataframe <- function(some_class){
  if (length(some_class) > 1){
    return(FALSE)
  }
  if (some_class == "data.frame"){
    return(TRUE)
  }
  return(FALSE)
}

is_class_matrix <- function(some_class){
  if (length(some_class) > 1){ # Might be future matrix
    if (all(some_class == c("matrix", "array"))){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }
  if (some_class == "matrix"){
    return(TRUE)
  }
  return(FALSE)
}
