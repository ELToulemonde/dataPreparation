% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fast_functions.r
\name{fast_filter_variables}
\alias{fast_filter_variables}
\title{Filtering useless variables}
\usage{
fast_filter_variables(
  data_set,
  level = 3,
  keep_cols = NULL,
  verbose = TRUE,
  ...
)
}
\arguments{
\item{data_set}{Matrix, data.frame or data.table}

\item{level}{which columns do you want to filter (1 = constant, 2 = constant and doubles,
3 = constant doubles and bijections, 4 = constant doubles bijections and included)(numeric, default to 3)}

\item{keep_cols}{List of columns not to drop (list of character, default to NULL)}

\item{verbose}{Should the algorithm talk (logical or 1 or 2, default to TRUE)}

\item{...}{optional parameters to be passed to the function when called from another function}
}
\value{
The same data_set but with fewer columns. Columns that are constant, in double,
or bijection of another have been deleted.
}
\description{
Delete columns that are constant or in double in your data_set set.
}
\details{
\code{verbose} can be set to 2 have full details from which functions, otherwise they
don't log. (\code{verbose = 1} is equivalent to \code{verbose = TRUE}).
}
\examples{
# First let's build a data.frame with 3 columns: a constant column, and a column in double
\dontrun{
df <- data.frame(col1 = 1, col2 = rnorm(1e6), col3 = sample(c(1, 2), 1e6, replace = TRUE))
df$col4 <- df$col2
df$col5[df$col3 == 1] = "a"
df$col5[df$col3 == 2] = "b" # Same info than in col1 but with a for 1 and b for 2
head(df)

# Let's filter columns:
df <- fast_filter_variables(df)
head(df)
}
# Don't run for CRAN, you can run example
}
