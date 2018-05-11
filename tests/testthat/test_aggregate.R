context("test_aggregate.R")
requireNamespace("data.table")
verbose <- TRUE

## aggregateByKey
# ---------------
data("adult")
# reduce it to save time
adult <- adult[1:5000, ]
# create some other columns to test it
adult$country2 <- adult$country
adult$boolcall <- sample(c(TRUE, FALSE), nrow(adult), replace = TRUE)
adult$id <- 1:nrow(adult)
# Store nrows 
store_nrow <- nrow(adult)
# Create an aggregation function
power <- function(x){sum(x^2)}
attach(list(power=power))
adult_aggregated <- aggregateByKey(copy(adult), key = "country", functions = c("power", "min"), verbose = verbose)

test_that("aggegateByKey: add function",
          {
            expect_equal(ncol(adult_aggregated), 79)
            expect_true(all(adult_aggregated$country2 == adult_aggregated$country))
            expect_equal(nrow(aggregateByKey(copy(adult), key = "id", verbose = verbose)), store_nrow)
          })



## Test exceptions
data("messy_adult")
messy_adult <- messy_adult[1:5000, c("country", "date1"), with = FALSE] # Only one date is needed to control error, a few lines to be faster
messy_adult <- findAndTransformDates(messy_adult, verbose = FALSE)
test_that("aggegateByKey: testing exceptions",
          {
            expect_error(aggregateByKey(copy(messy_adult), key = "country", verbose = verbose), "I can only handle: numeric, integer, factor, logical, character columns. ")
          })



## aggregateAcolumn
# ------------------
rm(messy_adult)
data("messy_adult")
messy_adult$logical <- messy_adult$age > 25
unique_keys <- unique(messy_adult[, "country", with = FALSE])
n_country <- nrow(unique_keys)
functions <- c("mean", "sd")
test_that("private function: aggregateAcolumn",
          {
            expect_equal(nrow(aggregateAcolumn(messy_adult[, c("country", "mail"), with = FALSE], col = "mail", key = "country", unique_keys = unique_keys)), n_country)
            expect_equal(nrow(aggregateAcolumn(messy_adult[, c("country", "income"), with = FALSE], col = "income", key = "country", unique_keys = unique_keys)), n_country) 
            expect_equal(nrow(aggregateAcolumn(messy_adult[, c("country", "age"), with = FALSE], col = "age", key = "country", unique_keys = unique_keys, functions = functions)), n_country)
            expect_equal(nrow(aggregateAcolumn(messy_adult[, c("country", "logical"), with = FALSE], col = "logical", key = "country", unique_keys = unique_keys)), n_country)
            expect_equal(nrow(aggregateAcolumn(messy_adult[, c("country", "country"), with = FALSE], col = "country", key = "country", unique_keys = unique_keys)), n_country)
            expect_error(aggregateAcolumn(messy_adult, col = "country", key = "country", unique_keys = unique_keys), ": dataSet should have 2 columns.")
          })
