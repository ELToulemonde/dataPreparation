#' Give same shape
#' 
#' Transform \code{transformSet} into the same shape as \code{referenceSet}. Espacially this 
#' function will be usefull to make your test set have the same shape as your train set.
#' @param transformSet Matrix, data.frame or data.table to transform
#' @param referenceSet Matrix, data.frame or data.table
#' @param verbose Should the algorithm talk? (logical, default to TRUE)
#' @return Retrun \code{transformSet} transformed in order to make it have the same shape as
#' \code{referenceSet}
#' @details
#' This function will make sure that \code{transformSet} and \code{referenceSet} 
#' \itemize{
#'    \item have the same class
#'    \item have exactly the same columns
#'    \item have columns with exactly the same class
#'    \item have factor factor with exactly the same levels
#' }
#' You should always use this function before applying your model on a new data set to make sure
#' that every thing will go smoothly. But if this function change a lot of stuff you should have a 
#' look to your preparation process, there might be something wrong. 
#' @examples
#' # Build a train and a test
#' data("messy_adult")
#' data("adult")
#' train <- messy_adult[1:100] 
#' test <- adult[101:150, ] # So test will have missing columns
#' 
#' # Prepare them
#' train <- prepareSet(train, verbose = FALSE, key = "country")
#' test <- prepareSet(test, verbose = FALSE, key = "country")
#' 
#' # Give them the same shape
#' test <- same_shape(test, train)
#' # As one can see in log, a lot of small change had to be done. 
#' # This is an extrem case but you get the idea.
#' @import data.table
#' @export
same_shape <- function(transformSet, referenceSet, verbose = TRUE){
  ## Working environement
  function_name <- "same_shape"
  
  ## Sanity check
  transformSet <- checkAndReturnDataTable(transformSet)
  is.verbose(verbose)
  
  ## Initialization
  # Store class of reference set and transform it into data.table to make computation faster
  referenceSet_class <- class(referenceSet)
  if (! any(referenceSet_class %in% c("data.table", "data.frame", "Matrix") )){
    stop(paste0(function_name, "referenceSet should be a data.table, data.frame or matrix."))
  }
  referenceSet <- checkAndReturnDataTable(referenceSet)
  
  
  ## Computation
  # Complete list of columns
  if (verbose){
    printl(function_name, ": verify that every column is present.")
  }
  for (col in names(referenceSet)){
    if (! col %in% names(transformSet)){
      transformSet[[col]] <- NA
      if (verbose){
        printl(function_name, ": column ", col, " was missing, I create it.")
      }
    }
  }
  
  # Drop unwanted columns
  if (verbose){
    printl(function_name, ": drop unwanted columns.")
    drop_list <- names(transformSet)[! names(transformSet) %in% names(referenceSet)]
    if (length(drop_list) > 0){
      if (verbose){
        printl(function_name, ": the folowing columns are in transform set but not in reference set: i drop them: ")
        print(drop_list)
      }
      transformSet[, (drop_list) := NULL]
    }
  }
  # Class of columns
  if (verbose){
    printl(function_name, ": verify that every column is in the right type.")
    pb <- initPB(function_name, names(transformSet))
  }
  for (col in names(transformSet)){
    trans_class <- class(transformSet[[col]])
    ref_class <- class(referenceSet[[col]])
    if (! all(trans_class == ref_class)){
      transfo_function <- paste0("as.", ref_class[[1]])
      if (exists(transfo_function)){
        if (verbose){
          printl(function_name, ": ", col, " class was ", trans_class, " i set it to ",
                 ref_class, ".")
        }
        set(transformSet, NULL, col, get(transfo_function)(transformSet[[col]]))
        
        # Control
        if (! all(class(transformSet[[col]]) == ref_class)){
           warning(paste0(function_name, ": bug in transformation please report it."))
        }
      }
      else {
        warning(paste0(function_name, ": ", col, " class is ", trans_class, " but should be ", ref_class,
                       " and i don't know how to transform it. "))
      }
    }
    if (verbose){
      setPB(pb, col)
    }    
  }
  if (verbose){ 
    close(pb); rm(pb)
  }
  
  # Factor levels
  if (verbose){
    printl(function_name, ": verify that every factor as the right number of levels.")
    pb <- initPB(function_name, names(transformSet))
  }
  for (col in names(transformSet)){
    if (is.factor(transformSet[[col]])){
      transfo_levels <- levels(transformSet[[col]])
      ref_levels <- levels(referenceSet[[col]])
      if (! identical(transfo_levels, ref_levels)){
        set(transformSet, NULL, col, factor(transformSet[[col]], levels = ref_levels))
        if (verbose){
          printl(function_name, ": ", col, " class add different levels than in referenceSet i change it.")
        }
      }
    }
    if (verbose){
      setPB(pb, col)
    }    
  }
  if (verbose){ 
    close(pb); rm(pb)
  }
  
  # Re-order columns
  setcolorder(transformSet, names(referenceSet))
  
  # Set class
  if (! identical(referenceSet_class, class(transformSet))){
    if (referenceSet_class == "data.frame"){
      setDF(transformSet)
    }
    if (referenceSet_class == "matrix"){
      transformSet <- as.matrix(transformSet)
    }
  }
  
  ## Wrapp-up
  return(transformSet)
}
  