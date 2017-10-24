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
adult_aggregated <- aggregateByKey(copy(adult), key = "country", functions = c(power, min, max, mean, sd), verbose = verbose)

test_that("aggegateByKey: add function",
          {
            expect_equal(ncol(adult_aggregated), 100)
			expect_true(all(adult_aggregated$country2 == adult_aggregated$country))
			expect_equal(nrow(aggregateByKey(adult, key = "id", verbose = verbose)), store_nrow)
          })



## Test exceptions
data(messy_adult)
messy_adult <- messy_adult[1:5000, ]
messy_adult <- unFactor(messy_adult, verbose = FALSE)
messy_adult <- findAndTransformNumerics(messy_adult, verbose = FALSE)
messy_adult <- findAndTransformDates(messy_adult, verbose = FALSE)
test_that("aggegateByKey: testing execptions",
          {
            expect_error(aggregateByKey(copy(adult), key = 1, verbose = verbose), ": key should be a character, you provided a ")
			expect_warning(aggregateByKey(copy(adult), key = "country", functions = c(power, sqrt = sqrt), verbose = verbose))
            expect_error(aggregateByKey(copy(messy_adult), key = "country", verbose = verbose), "I can only handle: numeric, integer, factor, logical, character columns. ")
          }
)



## aggregateAcolumn
# ------------------
messy_adult$logical <- messy_adult$age > 25
uniqueN_byCol <- lapply(messy_adult, uniqueN)
functions <- c(mean = mean, min = min, max = max, sd = sd)
test_that("private function: aggregateAcolumn",
          {
            expect_equal(nrow(aggregateAcolumn(messy_adult, col = "mail", key = "country", uniqueN_byCol, functions,  name_separator = ".")), uniqueN_byCol[["country"]])
            expect_equal(nrow(aggregateAcolumn(messy_adult, col = "income", key = "country", uniqueN_byCol, functions,  name_separator = ".")), uniqueN_byCol[["country"]]) 
            expect_equal(nrow(aggregateAcolumn(messy_adult, col = "num2", key = "country", uniqueN_byCol, functions,  name_separator = ".")), uniqueN_byCol[["country"]])
            expect_equal(nrow(aggregateAcolumn(messy_adult, col = "logical", key = "country", uniqueN_byCol, functions,  name_separator = ".")), uniqueN_byCol[["country"]])
            expect_equal(nrow(aggregateAcolumn(messy_adult, col = "country", key = "country", uniqueN_byCol, functions,  name_separator = ".")), uniqueN_byCol[["country"]]) 
          })
