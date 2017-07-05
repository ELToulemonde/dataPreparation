## Data set documentation

###################################################################################################
################################### messy_adult ###################################################
###################################################################################################
#' adult with some ugly columns added
#' 
#' For examples and tutorials, messy_adult has been built using UCI \code{adult}.\cr
#' 
#' We added 9 really ugly columns to the data set:
#' 
#' \itemize{
#'     \item 4 dates with various formats and NAs
#'     \item 1 constant column
#'     \item 3 numeric with different decimal separator
#'     \item 1 email adress
#' }
#'
#' 
#' @name messy_adult
#' @docType data
#' @keywords data
#' @usage data(messy_adult)
#' @format A data.table with 32561 rows and 24 variables
NULL



###################################################################################################
####################################### adult #####################################################
###################################################################################################
#' adult for UCI repository
#' 
#' For examples and tutorials, and in order to build \code{messy_adult}, UCI adult data set is used. \cr
#' Data Set Information: \cr
#' \cr
#' Extraction was done by Barry Becker from the 1994 Census database. A set of reasonably clean records was
#' extracted using the following conditions: ((AAGE>16) && (AGI>100) && (AFNLWGT>1)&& (HRSWK>0))\cr
#'\cr
#' Prediction task is to determine whether a person makes over 50K a year. 
#' @name adult
#' @docType data
#' @references \url{https://archive.ics.uci.edu/ml/datasets/adult}
#' @keywords data
#' @usage data(adult)
#' @format A data.table with 32561 rows and 15 variables
NULL