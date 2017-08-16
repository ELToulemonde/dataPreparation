#' Final preparation before ML algorithm
#'
#' Prepare a data.table by: \cr
#' - transforming numeric variables into factors whenever they take less than \code{thresh} unique 
#' variables \cr
#' - transforming characters using \code{generateFromCharacter} \cr
#' - transforming logicals into binary integers \cr
#' - dropping constant columns \cr
#' - Sending the data.table to setAsNumericMatrix() (when finalForm == "numerical_matrix") will then allow 
#' you to get a numerical matrix usable by most Machine Learning Algorithms.
#' 
#' @param dataSet Matrix, data.frame or data.table
#' @param finalForm "data.table" or "numerical_matrix" (default to data.table)
#' @param thresh  numeric, threshold such that  a numerical column is transformed into
#'   a factor whenever its number of unique modalities is smaller or equal to
#' \code{thresh} (default to 10)
#' @param verbose logical
#'
#' @section Warning:
#' All these changes will happen \strong{by reference}: please send a copy() of
#' your data.table to prepareSet if you do not want your
#' original dataSet to be modified.
#' @export
#' @importFrom tcltk tkProgressBar setTkProgressBar
#' @importFrom stats quantile
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
  if (verbose) {printl("Transforming characters into factor using setColAsFactor.")}
  carac_cols <- names(col_class)[col_class %in% c("character")]
  dataSet <- setColAsFactor(dataSet, cols = carac_cols, n_levels = -1, verbose = verbose)
  
  
  # NUMERIC INTO FACTORS (IF # OF MODALITIES <= THRESH)
  # TODO : paralleliser la recherche de variables à transformer
  if (verbose) {printl("Transforming numerical variables into factors when length(unique(col)) <=",  thresh, ".")}
  num_cols <- names(col_class)[col_class %in% c("numeric", "integer")]
  if (verbose) {
    cat("Going through", length(num_cols), "numerical variables to transform if necessary.\n")
    pb <- tkProgressBar(title=paste0(function_name, ": 0% done"), 
                         min = 1, max = ncol(dataSet), width = 300) # Construction d'une progress bar
  }
  for (col in num_cols) {
    nCat <- uniqueN(dataSet[[col]])
    if (nCat <= thresh) set(dataSet, NULL, col, as.factor(dataSet[[col]]))
    if (verbose){
      setTkProgressBar(pb, which(colnames(dataSet) == col), 
                        title=paste0(function_name, ": ", round(which(colnames(dataSet) == col)/ncol(dataSet)*100, 0), "% done"))  
    }
  }
  if (verbose){
    close(pb); rm(pb);
  }
  col_class <- sapply(dataSet, class)
  
  # LOGICAL INTO BINARY
  if (verbose) {printl("Transforming logicals into binaries.\n")}
  logical_col <- names(col_class)[which(col_class %in% c("logical"))]
  for (col in logical_col) set(dataSet, NULL, col, as.integer(dataSet[[col]] * 1))
  
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
    listCardLevels <- sapply(factor_cols, function(x, dataSet)
      length(levels(dataSet[[x]])), dataSet = dataSet)
    printl("Quantiles for the number of factor modalities:")
    print(quantile(listCardLevels, probs = seq(0, 1, .1)))
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
#' @param intercept logical. Should a constant column be added?
#' @param allCols logical. For each factor, should we create all possible
#' dummies, or should we drop a reference dummy?
#' @param sparse logical. Should the resulting matrix be of a (sparse) Matrix
#' class?
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