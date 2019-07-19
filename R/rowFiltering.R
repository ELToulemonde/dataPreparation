#' Standard deviation outlier filtering
#'
#' Remove outliers based on standard deviation thresholds. \cr
#' Only values within \code{mean - sd * n_sigmas} and \code{mean + sd * n_sigmas} are kept.
#' @param dataSet Matrix, data.frame or data.table
#' @param cols List of numeric column(s) name(s) of dataSet to transform. To transform all 
#' numeric columns, set it to "auto".  (character, default to "auto")
#' @param n_sigmas number of times standard deviation is accepted (interger, default to 3)
#' @param verbose Should the algorithm talk? (logical, default to TRUE)
#' @details Filtering is made column by column, meaning that extrem values from first element
#' of \code{cols} are removed, then extrem values from second element of \code{cols} are removed, 
#' ... \cr
#' So if filtering is perfomed on too many column, there ia high risk that a lot of rows will be dropped.
#' @return Same dataset with less rows, edited by \strong{reference}. \cr
#' If you don't want to edit by reference please provide set \code{dataSet = copy(dataSet)}.
#' @examples 
#' # Given
#' library(data.table)
#' col_vals <- runif(1000)
#' col_mean <- mean(col_vals)
#' col_sd <- sd(col_vals)
#' extrem_val <- col_mean + 6 * col_sd
#' dataSet <- data.table(num_col = c(col_vals, extrem_val))
#' 
#' # When
#' dataSet <- remove_sd_outlier(dataSet, cols = "auto", n_sigmas = 3, verbose = TRUE)
#' 
#' # Then extrem value is no longer in set
#' extrem_val %in% dataSet[["num_col"]] # Is false
#' @export
remove_sd_outlier <- function(dataSet, cols = "auto", n_sigmas = 3, verbose = TRUE){
  ## Environement
  function_name <- "remove_sd_outlier"  
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet = dataSet)
  cols <- real_cols(dataSet = dataSet, cols = cols, function_name = function_name, types = "numeric")
  
  ## Initialization
  if (verbose){
    printl(function_name, ": I start to filter categorical rare events")
    pb <- initPB(function_name, names(dataSet))
    start_time <- proc.time()
  }
  initial_nrow <- nrow(dataSet)
  
  ## Computation
  for (col in cols){
    tmp_nrow <- nrow(dataSet)
    col_mean <- dataSet[, mean(get(col))]
    col_sd <- dataSet[, sd(get(col))]
    dataSet <- dataSet[(get(col) <= col_mean + n_sigmas * col_sd) & 
                         (get(col) >= col_mean - n_sigmas * col_sd), ]
    if (verbose){
      printl(function_name, ": dropped ", tmp_nrow - nrow(dataSet), " row(s) that are rare event on ", col, ".")
      setPB(pb, col)
    }
  }
  
  if (verbose){
    printl(function_name, ": ", initial_nrow - nrow(dataSet), " have been dropped. It took ", 
           round( (proc.time() - start_time)[[3]], 2), " seconds. ")
  }
  
  
  ## Wrapp-up
  return(dataSet)
}

#' Filter rare categoricals
#'
#' Filter rows that have a rare occurences
#' @param dataSet Matrix, data.frame or data.table
#' @param cols List of column(s) name(s) of dataSet to transform. To transform all 
#' columns, set it to "auto".  (character, default to "auto")
#' @param threshold share of occurencies under which row should be removed (numeric, default to 0.01)
#' @param verbose Should the algorithm talk? (logical, default to TRUE)
#' @details Filtering is made column by column, meaning that extrem values from first element
#' of \code{cols} are removed, then extrem values from second element of \code{cols} are removed, 
#' ... \cr
#' So if filtering is perfomed on too many column, there ia high risk that a lot of rows will be dropped.
#' @return Same dataset with less rows, edited by \strong{reference}. \cr
#' If you don't want to edit by reference please provide set \code{dataSet = copy(dataSet)}.
#' @examples 
#' # Given a set with rare "C"
#' library(data.table)
#' dataSet <- data.table(cat_col = c(sample(c("A", "B"), 1000, replace=TRUE), "C"))
#' 
#' # When calling function
#' dataSet <- remove_rare_categorical(dataSet, cols = "cat_col",  
#'                                    threshold = 0.01, verbose = TRUE)
#'                                    
#' # Then there are no "C"
#' unique(dataSet[["cat_col"]])
#' @import data.table
#' @export
remove_rare_categorical <- function(dataSet, cols ="auto", threshold = 0.01, verbose = TRUE){
  ## Environement
  function_name <- "remove_rare_categorical"  
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet = dataSet)
  cols <- real_cols(dataSet = dataSet, cols = cols, function_name = function_name)
  
  ## Initialization
  if (verbose){
    printl(function_name, ": I start to filter categorical rare events")
    pb <- initPB(function_name, names(dataSet))
    start_time <- proc.time()
  }
  initial_nrow <- nrow(dataSet)
  
  ## Computation
  for ( col in cols){
    col_val_occ <- dataSet[, .N, by = col]
    acceptable_cat <- col_val_occ[get("N") >= initial_nrow * threshold, get(col)]
    
    tmp_nrow <- nrow(dataSet)
    dataSet <- dataSet[get(col) %in% acceptable_cat]
    
    if (verbose){
      printl(function_name, ": dropped ", tmp_nrow - nrow(dataSet), " row(s) that are rare event on ", col, ".")
      setPB(pb, col)
    }
  }
  
  if (verbose){
    printl(function_name, ": ", initial_nrow - nrow(dataSet), " have been dropped. It took ", 
           round( (proc.time() - start_time)[[3]], 2), " seconds. ")
  }
  
  ## Wrap-up
  return(dataSet)
}

#' Percentile outlier filtering
#'
#' Remove outliers based on percentiles. \cr
#' Only values within \code{n}th and \code{100 - n}th percentiles are kept.
#' @param dataSet Matrix, data.frame or data.table
#' @param cols List of numeric column(s) name(s) of dataSet to transform. To transform all 
#' numeric columns, set it to "auto".  (character, default to "auto")
#' @param percentile percentiles to filter (numeric, default to 1)
#' @param verbose Should the algorithm talk? (logical, default to TRUE)
#' @details Filtering is made column by column, meaning that extrem values from first element
#' of \code{cols} are removed, then extrem values from second element of \code{cols} are removed, 
#' ... \cr
#' So if filtering is perfomed on too many column, there ia high risk that a lot of rows will be dropped.
#' @return Same dataset with less rows, edited by \strong{reference}. \cr
#' If you don't want to edit by reference please provide set \code{dataSet = copy(dataSet)}.
#' @examples 
#' # Given
#' library(data.table)
#' dataSet <- data.table(num_col = 1:100)
#' 
#' # When
#' dataSet <- remove_percentile_outlier(dataSet, cols = "auto", percentile = 1, verbose = TRUE)
#' 
#' # Then extrem value is no longer in set
#' 1 %in% dataSet[["num_col"]] # Is false
#' 2 %in% dataSet[["num_col"]] # Is true
#' @importFrom stats quantile
#' @export
remove_percentile_outlier <- function(dataSet, cols = "auto", percentile = 1, verbose = TRUE){
  ## Environement
  function_name <- "remove_percentile_outlier"  
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet = dataSet)
  cols <- real_cols(dataSet = dataSet, cols = cols, function_name = function_name, types = "numeric")
  
  ## Initialization
  if (verbose){
    printl(function_name, ": I start to filter categorical rare events")
    pb <- initPB(function_name, names(dataSet))
    start_time <- proc.time()
  }
  initial_nrow <- nrow(dataSet)
  
  ## Computation
  for (col in cols){
    tmp_nrow <- nrow(dataSet)
    percentiles <- quantile(dataSet[[col]], 
                            c(percentile / 100, (100 - percentile) / 100), na.rm = TRUE)
    dataSet <- dataSet[(get(col) >=  percentiles[1]) & (get(col) <= percentiles[2]), ]
    if (verbose){
      printl(function_name, ": dropped ", tmp_nrow - nrow(dataSet), " row(s) that are rare event on ", col, ".")
      setPB(pb, col)
    }
  }
  
  if (verbose){
    printl(function_name, ": ", initial_nrow - nrow(dataSet), " have been dropped. It took ", 
           round( (proc.time() - start_time)[[3]], 2), " seconds. ")
  }
  
  
  ## Wrapp-up
  return(dataSet)
}
