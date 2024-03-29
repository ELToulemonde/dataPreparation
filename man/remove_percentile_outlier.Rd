% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/row_filtering.R
\name{remove_percentile_outlier}
\alias{remove_percentile_outlier}
\title{Percentile outlier filtering}
\usage{
remove_percentile_outlier(
  data_set,
  cols = "auto",
  percentile = 1,
  verbose = TRUE
)
}
\arguments{
\item{data_set}{Matrix, data.frame or data.table}

\item{cols}{List of numeric column(s) name(s) of data_set to transform. To transform all
numeric columns, set it to "auto".  (character, default to "auto")}

\item{percentile}{percentiles to filter (numeric, default to 1)}

\item{verbose}{Should the algorithm talk? (logical, default to TRUE)}
}
\value{
Same dataset with less rows, edited by \strong{reference}. \cr
If you don't want to edit by reference please provide set \code{data_set = copy(data_set)}.
}
\description{
Remove outliers based on percentiles. \cr
Only values within \code{n}th and \code{100 - n}th percentiles are kept.
}
\details{
Filtering is made column by column, meaning that extreme values from first element
of \code{cols} are removed, then extreme values from second element of \code{cols} are removed,
... \cr
So if filtering is performed on too many column, there ia high risk that a lot of rows will be dropped.
}
\examples{
# Given
library(data.table)
data_set <- data.table(num_col = seq_len(100))

# When
data_set <- remove_percentile_outlier(data_set, cols = "auto", percentile = 1, verbose = TRUE)

# Then extreme value is no longer in set
1 \%in\% data_set[["num_col"]] # Is false
2 \%in\% data_set[["num_col"]] # Is true
}
