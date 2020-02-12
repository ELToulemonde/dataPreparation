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
  print("Debug : 1")
  dataSet <- checkAndReturnDataTable(dataSet)
  print("Debug : 2")
  is.verbose(verbose)
  
  ## Initialization
  # Store class of reference set and transform it into data.table to make computation faster
  referenceSet_class <- class(referenceSet)
  referenceSet <- checkAndReturnDataTable(referenceSet, name = "referenceSet")
  print("Debug : 3")
  ## Computation
  # Complete list of columns
  if (verbose){
    printl(function_name, ": verify that every column is present.")
  }
  print("Debug : 4")
  create_list <- names(referenceSet)[! names(referenceSet) %in% names(dataSet)]
  if (length(create_list) > 0){
    if (verbose){
      printl(function_name, ": columns ", paste(create_list, collapse = ", "), " are missing, I create them.")
    }
	set(dataSet, NULL, create_list, NA)
  }
  print("Debug : 5")
  # Drop unwanted columns
  if (verbose){
    printl(function_name, ": drop unwanted columns.")
  }
  print("Debug : 6")
  drop_list <- names(dataSet)[! names(dataSet) %in% names(referenceSet)]
  print("Debug : 7")
  if (length(drop_list) > 0){
    if (verbose){
      printl(function_name, ": the folowing columns are in dataSet but not in referenceSet: I drop them: ")
      print(drop_list)
    }
	set(dataSet, NULL, drop_list, NULL)
  }
  print("Debug : 8")
  
  # Class of columns
  if (verbose){
    printl(function_name, ": verify that every column is in the right type.")
    pb <- initPB(function_name, names(dataSet))
  }
  print("Debug : 9")
  for (col in names(dataSet)){
    trans_class <- class(dataSet[[col]])
    ref_class <- class(referenceSet[[col]])
    print(paste("Debug : 10 : ", str(col)))
    if (! all(trans_class == ref_class)){
      print("Debug : 11")
      transfo_function <- paste0("as.", ref_class[[1]])
      if (exists(transfo_function)){
        print("Debug : 12")
        if (verbose){
          printl(function_name, ": ", col, " class was ", trans_class, " i set it to ",
                 ref_class, ".")
        }
        set(dataSet, NULL, col, get(transfo_function)(dataSet[[col]]))
        print("Debug : 13")
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
      print("Debug : 14")
    }
    if (verbose){
      setPB(pb, col)
    }    
  }
  gc(verbose = FALSE)
  print("Debug : 15")
  # Factor levels
  if (verbose){
    printl(function_name, ": verify that every factor as the right number of levels.")
    pb <- initPB(function_name, names(dataSet))
  }
  for (col in names(dataSet)){
    print(paste("Debug : 16", str(col)))
    print(is.factor(dataSet[[col]]))
    if (is.factor(dataSet[[col]])){
      print("Debug : 17")
      transfo_levels <- levels(dataSet[[col]])
      print("Debug : 18")
      ref_levels <- levels(referenceSet[[col]])
      print("Debug : 19")
      print( identical(transfo_levels, ref_levels))
      if (! identical(transfo_levels, ref_levels)){
        print("Debug : 20")
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
  print("Debug 21")
  setcolorder(dataSet, names(referenceSet))
  
  # Set class
  print("Debug 22")
  print(! identical(referenceSet_class, class(dataSet)))
  print(referenceSet_class)
  print(class(dataSet))
  if (! identical(referenceSet_class, class(dataSet))){
    if (referenceSet_class == "data.frame"){
      setDF(dataSet)
    }
    if (referenceSet_class == "matrix"){
      dataSet <- as.matrix(dataSet)
    }
  }
  
  ## Wrapp-up
  return(dataSet)
}
  