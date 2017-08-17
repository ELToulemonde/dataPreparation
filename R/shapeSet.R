#' Final preparation before ML algorithm
#'
#' Prepare a data.table by: \cr
#' - transforming numeric variables into factors whenever they take less than \code{thresh} unique 
#' variables \cr
#' - transforming characters using \code{generateFromCharacter} \cr
#' - transforming logical into binary integers \cr
#' - dropping constant columns \cr
#' - Sending the data.table to setAsNumericMatrix() (when finalForm == "numerical_matrix") will then allow 
#' you to get a numerical matrix usable by most Machine Learning Algorithms.
#' 
#' @param dataSet Matrix, data.frame or data.table
#' @param finalForm "data.table" or "numerical_matrix" (default to data.table)
#' @param thresh  numeric, threshold such that  a numerical column is transformed into
#'   a factor whenever its number of unique modalities is smaller or equal to
#' \code{thresh} (default to 10)
#' @param verbose Should the algorithm talk? (logical, default to TRUE)
#' @section Warning:
#' All these changes will happen \strong{by reference}.
#' @export
#' @importFrom tcltk tkProgressBar setTkProgressBar
shapeSet <- function(dataSet, finalForm = "data.table", thresh = 10, verbose = TRUE){
  ## Working environement
  function_name <- "prepareSet"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  is.verbose(verbose)
  
  ##  Initialization
  col_class <- sapply(dataSet, class)
  col_class_init <-  sapply(col_class, function(x){x[[1]]}) # Safety for classes with multiple values: ex: POSIXct, POSIXt
  ## Computation
  carac_cols <- names(col_class)[col_class %in% c("character")]
  if (length(carac_cols) > 0){
	dataSet <- setColAsFactor(dataSet, cols = carac_cols, n_levels = -1, verbose = verbose)
  }
  
  # NUMERIC INTO FACTORS (IF # OF MODALITIES <= THRESH)
  num_cols <- names(col_class)[col_class %in% c("numeric", "integer")]
  if (length(num_cols) > 0){
    if (verbose) {
      printl("Transforming numerical variables into factors when length(unique(col)) <= ",  thresh, ".")
      pb <- initPB(function_name, num_cols)
    }
    for (col in num_cols) {
      if (fastMaxNbElt(dataSet[[col]], thresh)){
        set(dataSet, NULL, col, as.factor(dataSet[[col]]))
      } 
      if (verbose){
        setPB(pb, col)
      }
    }
    if (verbose){
      close(pb); rm(pb);
    }
  }
  col_class <- sapply(dataSet, class)
  
  # LOGICAL INTO BINARY
  logical_col <- names(col_class)[which(col_class %in% c("logical"))]
  if (length(logical_col) > 0){
	if (verbose) {printl("Transforming logical into binaries.\n")}
	for (col in logical_col) set(dataSet, NULL, col, as.integer(dataSet[[col]] * 1))
  }

  # Distribution des types de colonnes
  if (verbose){
    col_class_end <- sapply(dataSet, class)
	col_class_end <- sapply(col_class_end, function(x){x[[1]]})
    printl("Previous distribution of column types:")
    print(table(col_class_init))
    printl("Current distribution of column types:")
    print(table(col_class_end))
    # Number of levels for each factor
    col_class <- sapply(dataSet, class)
    factor_cols <- names(col_class)[which(col_class %in% c("factor"))]
  }

  ## Wrapp-up
  if (finalForm == "numerical_matrix"){
    dataSet <- setAsNumericMatrix(dataSet)
  }
  return(dataSet)
}




#' Numeric matrix preparation for Machine Learning.
#'
#' Prepare a numeric matrix from a data.table. This matrix is suitable for
#' machine learning purposes, since factors are binarized. It may be sparsed,
#' include an intercept, and drop a reference column for each factor if
#' required (when using \code{lm()}, for instance)
#' 
#' @param dataSet data.table
#' @param intercept Should a constant column be added? (logical, default to FALSE)
#' @param allCols For each factor, should we create all possible
#' dummies, or should we drop a reference dummy? (logical, default to FALSE)
#' @param sparse Should the resulting matrix be of a (sparse) Matrix
#' class? (logical, default to FALSE)
#' @export
#' @importFrom stats as.formula model.matrix contrasts
#' @importFrom Matrix sparse.model.matrix
setAsNumericMatrix <- function(dataSet, intercept = FALSE, allCols = FALSE, 
                                     sparse = FALSE) {
  
  ## SANITY CHECKS
  if (!("data.table") %in% class(dataSet)) stop("setAsNumericMatrix: dataSet is not a data.table")
  # Checking that columns are of the proper type. Column type modification are
  # not done here, because we would either need to duplicate the dataSet in memory
  # (not memory efficient), or we would need to modify them by reference.
  # Modification by reference may be done through a separate function. It seems
  # preferable to keep functions that modify dataSet by reference separated from
  # functions that generate new dataSet.
  if (any(!sapply(dataSet, function(x) is.numeric(x) | is.logical(x) |
                 is.factor(x)))){
    stop("setAsNumericMatrix: some columns are not numerical/logical/factor. Consider using shapeSet() to prepare the dataSet.")
  } 
  
  # FORMULA
  if (intercept) fm <- as.formula("~ .") else fm <- as.formula("~ . - 1")
  
  # CONTRASTS
  # Getting full contrast (identity) matrices when allCols == TRUE (required to
  # build a full model matrix)
  factor_cols <- which(sapply(dataSet, is.factor))
  if (allCols & length(factor_cols) > 0) {
    ct <- lapply(dataSet[, factor_cols, with = FALSE], contrasts, contrasts = FALSE) 
  } else {
    ct <- NULL
  }
  
  # MATRIX
  if (sparse) {
    matOut <- sparse.model.matrix(fm, data = dataSet, contrasts.arg = ct)
  } else {
    matOut <- model.matrix(fm, data = dataSet, contrasts.arg = ct)
  }
  
  # RETURN
  return(matOut)
}