#' Discretization
#' 
#' Discretization of numeric variable (either equal_width or equal_fred).
#' @param dataSet Matrix, data.frame or data.table
#' @param cols list of numeric column(s) name(s) of dataSet to transform. To transform all 
#' characters, set it to "auto"
#' @param n_bins number of group to compute (numeric, default to 10)
#' @param type type of discretization ("equal_width" or "equal_freq")
#' @param verbose Should the algorithm talk? (Logical, default to TRUE)
#' @return Same dataset edited by reference. With `cols` transformed into bins. \cr
#' If you don't want to edit by reference please provide set `dataSet = copy(dataSet)`.
#' @details NAs will be putted in an NA category. Using equal freq first bin will start at -Inf and last bin will end at +Inf.
#' @examples 
#' data(messy_adult)
#' head(messy_adult)
#' 
#' messy_adult <- fastDiscretization(messy_adult)
#' 
#' head(messy_adult)
#' @export
fastDiscretization <- function(dataSet, cols = "auto", n_bins = 10, type = "equal_width", verbose = TRUE){
  ## Working environement
  function_name <- "fastDiscretization"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  is.verbose(verbose)
  cols <- real_cols(dataSet, cols, function_name, types = c("numeric", "integer"))
  if (!type %in% c("equal_width", "equal_freq")){
    stop(paste0(function_name, ": type should either be equal_width or equal_freq"))
  }
  n_bins <- round(n_bins) # Just to be safe
  
  ## Initialization
  if (verbose){
    pb <- initPB(function_name, cols)
    printl(function_name, ": I will discretize ", length(cols), " numeric columns using, ", 
           type, " method.")
    start_time <- proc.time()
  }
  n_transformed <- length(cols)
  ## Computation 
  for (col in cols){
    # Compute splits
    if (type == "equal_width"){
      splits <- equal_width_splits(dataSet[[col]], n_bins = n_bins, col = col, verbose = verbose)
    }
    if (type == "equal_freq"){
      splits <- equal_freq_splits(dataSet[[col]], n_bins = n_bins, col = col, verbose = verbose)
    }
    split_names <- build_splits_names(splits)
    
    # Constant columns
    if (length(splits) == 1){
      printl(function_name, ": column ", col, " seems to be constant, I do nothing.")
      n_transformed <- n_transformed - 1
      next()
    }
    # Update column
    find_split <- function(x){
      if (is.na(x)){
        return(NA)
      }
      res <- which(splits[-length(splits)] <= x & x < splits[-1])
      if (length(res) == 0){
        res <- length(splits) -1
      }
      return(res)
    }
    set(dataSet, NULL, col, as.factor(split_names[sapply(dataSet[[col]], find_split)]))
    # Update progress bar
    if (verbose){
      setPB(pb, col)  
    }
  }
  if (verbose){
    printl(function_name, ": it took me: ", round( (proc.time() - start_time)[[3]], 2), 
           "s to transform ", n_transformed, " numeric columns into, binarised columns.")
  }
  ## Wrapp-up
  return(dataSet)
}


## equal_freq_splits
# ------------------
# @param dataSet a numeric verctor
# @param n_bins interger number of wanted bins
# @return a vector of limits (lim1, ..., lim(n_bins +1))
equal_width_splits <- function(dataSet, n_bins, col = "dataSet", verbose = TRUE){
  ## Working environement
  function_name <- "equal_width_splits"
  ## Sanity check
  if (!is.numeric(dataSet) || ! is.numeric(n_bins)){
    stop(paste0(function_name, ": dataSet should be a vector of numerics and n_bins a numeric."))
  }
  is.verbose(verbose)
  
  ## Computation
  min_val <- min(dataSet, na.rm = TRUE)
  max_val <- max(dataSet, na.rm = TRUE)
  splits <- seq(from = min_val, to = max_val, length.out = n_bins + 1) # +1 to have exactly n_bins intervals
  
  ## To be safe
  splits <- unique(splits)
  if (length(splits) < n_bins & verbose){
    printl(function_name, ": ", col, " can't provide ", n_bins,
           " equal width bins; instead you will have ", length(splits) - 1, " bins.")
  }
  ## Wrap-up
  return(splits)
}

## equal_freq_splits
# ------------------
# @param dataSet a numeric verctor
# @param n_bins interger number of wanted bins
# @return a vector of limits (-Inf, lim1, ..., +Inf)
equal_freq_splits <- function(dataSet, n_bins, col = "dataSet", verbose = TRUE){
  ## Working environement
  function_name <- "equal_freq_splits"
  
  ## Sanity check
  if (!is.numeric(dataSet) || ! is.numeric(n_bins)){
    stop(paste0(function_name, ": dataSet should be a vector of numerics and n_bins a numeric."))
  }
  is.verbose(verbose)
  
  ## Computation
  # Order
  dataSet <- dataSet[! is.na(dataSet)]
  order_set <- order(dataSet, decreasing = FALSE)
  
  # Number of value in each 
  wanted_index <- round(seq(from = 1, to = length(dataSet), length.out = n_bins + 1))
  
  splits <- dataSet[order_set[wanted_index]]
  splits[1] <- -Inf
  splits[n_bins + 1] <- +Inf
  
  ## To be safe
  splits <- unique(splits)
  if (length(splits) < n_bins & verbose){
   printl(function_name, ": ", col, " can't provide ", n_bins,
          " equal freq bins; instead you will have ", length(splits) - 1, " bins.")
  }
  ## Wrapp-up
  return(splits)
}

## build_splits_names
# -------------------
build_splits_names <- function(splits){
  split_names <- paste0("[", splits[-length(splits)], ", " , splits[-1], "[")
  
  split_names[length(split_names)] <- gsub("\\[$", "]", split_names[length(split_names)])
  split_names[1] <- gsub("\\[-Inf", "]-Inf", split_names[1])
  split_names[length(split_names)] <- gsub("+Inf\\]$", "+Inf[", split_names[length(split_names)])
  
  return(split_names)
}


#' Get bins
#' 
#' Retriveve bins from a data set containing discretized columns (using function \code{\link{fastDiscretization}})
#' @param dataSet Matrix, data.frame or data.table
#' @return List of list where each element name is the column name and the element is a the list of bins. \cr
#' @examples 
#' # Build a discretized data set
#' data(adult)
#' adult <- fastDiscretization(adult, verbose = FALSE)
#' 
#' # Retrieve bins
#' get_bins(adult)
#' @export
get_bins <- function(dataSet){
  ## Working environement
  function_name <- "get_bins"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  
  ## Initialization
  cols <- real_cols(dataSet, cols = "auto", function_name = function_name, types = "factor")
  bin_list = list()
  
  ## Computation
  for (col in cols){
    level_list <- levels(dataSet[[col]])
    if (all(sapply(level_list, is_bin))){
      bin_list[[col]] <- level_list  
    }
  }
  
  ## Wrapp-up
  return(bin_list)
}

## is_bin
# -------
# Control that it starts with a bracket and end with one
is_bin <- function(obj){
  return( (grepl("^\\[", obj) || grepl("^\\]", obj)) & (grepl("\\[$", obj) || grepl("\\]$", obj)))
}



#' Apply discretization
#' 
#' Apply discretization using pre-set bins.
#' @param dataSet Matrix, data.frame or data.table
#' @param bins vector of bins for discretization. They should come from \code{\link{get_bins}} \cr
#'  or be carefully hand written. 
#' @param verbose Should the algorithm talk? (Logical, default to TRUE)
#' @return dataSet with numerical columns discretized by \strong{reference}.
#' @details For now, opening and closing brackets are used the same way. 
#' @examples
#' # Build discretized data
#' data(adult)
#' adult <- fastDiscretization(adult)
#' bins <- get_bins(adult)
#' 
#' # Apply it to messy_adult
#' data(messy_adult)
#' messy_adult <- apply_bins(messy_adult, bins = bins)
#'
#' # To apply your own bins on column "age"
#' data(messy_adult)
#' messy_adult <- apply_bins(messy_adult, bins = list(age = c("[0,40[","[40, 100]")))
#' @export
#' @importFrom stringr str_split
#' @import magrittr
apply_bins <- function(dataSet, bins, verbose = TRUE){
  ## Working environement
  function_name <- "apply_bins"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  is.verbose(verbose)
  
  ## Initialization
  cols <- names(bins)
  cols <- real_cols(dataSet, cols, function_name = "apply_bins", types = "numeric")
  if (verbose){
    pb <- initPB(function_name, cols)
    printl(function_name, ": I will discretize ", length(cols), " numeric columns using given bins.")
    start_time <- proc.time()
  }
  
  ## Computation
  for (col in cols){
    # Interpret splits
    splits <- bins[[col]] %>%
      str_split(",") %>%
      #unlist() %>%
      stringr::str_extract_all("[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?") %>%
      unlist() %>%
      unique() %>%
      as.numeric() %>%
      sort()
    split_names <- bins[[col]]
    
    # Update column
    find_split <- function(x){
      if (is.na(x)){
        return(NA)
      }
      res <- which(splits[-length(splits)] <= x & x < splits[-1])
      if (length(res) == 0){
        res <- length(splits) -1
      }
      return(res)
    }
    set(dataSet, NULL, col, as.factor(split_names[sapply(dataSet[[col]], find_split)]))
    # Update progress bar
    if (verbose){
      setPB(pb, col)  
    }
  }
  if (verbose){
    printl(function_name, ": it took me: ", round( (proc.time() - start_time)[[3]], 2), 
           "s to transform ", length(cols), " numeric columns into, binarised columns.")
  }
  ## Wrapp-up
  return(dataSet)  
}
