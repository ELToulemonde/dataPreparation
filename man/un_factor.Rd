% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/factor_manipulation.R
\name{un_factor}
\alias{un_factor}
\title{Unfactor factor with too many values}
\usage{
un_factor(data_set, cols = "auto", n_unfactor = 53, verbose = TRUE)
}
\arguments{
\item{data_set}{Matrix, data.frame or data.table}

\item{cols}{List of column(s) name(s) of data_set to look into. To check all all columns, set it
to "auto". (characters, default to "auto")}

\item{n_unfactor}{Number of max element in a factor (numeric, default to 53)}

\item{verbose}{Should the algorithm talk? (logical, default to TRUE)}
}
\value{
Same data_set (as a data.table) with less factor columns.
}
\description{
To un-factorize all columns that have more than a given amount of various values. This
 function will be usefully after using some reading functions that put every string as factor.
}
\details{
If a factor has (strictly) more than \code{n_unfactor} values it is un-factored. \cr
It is recommended to use \code{\link{find_and_transform_numerics}} and
 \code{\link{find_and_transform_dates}} after this function.\cr
If \code{n_unfactor} is set to -1, nothing will be performed. \cr
If there are a lot of column that have been transformed, you might want to look at the
documentation of your data reader in order to stop transforming everything into a factor.
}
\examples{
# Let's build a data_set
data_set <- data.frame(true_factor = factor(rep(c(1,2), 13)),
                      false_factor = factor(LETTERS))

# Let's un factorize all factor that have more than 5 different values
data_set <- un_factor(data_set, n_unfactor = 5)
sapply(data_set, class)
# Let's un factorize all factor that have more than 5 different values
data_set <- un_factor(data_set, n_unfactor = 0)
sapply(data_set, class)

}
