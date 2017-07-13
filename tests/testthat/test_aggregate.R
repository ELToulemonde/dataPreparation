requireNamespace("data.table")
verbose = TRUE
## aggregateByKey
# ---------------

data("adult")
# reduce it to save time
adult <- adult[1:5000, ]

# Aggregate it using aggregateByKey, in order to extract characteristics for each country
adult_aggregated <- aggregateByKey(adult, key = "country", verbose = verbose)


test_that("aggegateByKey: ",
          {
            expect_equal(ncol(adult_aggregated), 87)
          }
)

data(messy_adult)
messy_adult <- findAndTransformDates(messy_adult, verbose = FALSE)
test_that("aggegateByKey: testing execptions",
          {
            expect_error( aggregateByKey(adult, key = 1, verbose = verbose), ": key should be a character, you provided a ")
			expect_error( aggregateByKey(messy_adult, key = "country", verbose = verbose), "I can only handle: numeric, integer, factor, logical, character columns. ")
          }
)
## Exmple with other functions

data("adult")
# reduce it to save time
adult <- adult[1:5000, ]

power <- function(x){sum(x^2)}
adult_aggregated <- aggregateByKey(adult, key = "country", functions = power, verbose = verbose)

test_that("aggegateByKey: add function",
          {
            expect_equal(ncol(adult_aggregated), 69)
          })




data("adult")
# reduce it to save time
adult <- adult[1:5000, ]

test_that("aggegateByKey: add function that is not an agg function",
          {
            expect_warning(aggregateByKey(adult, key = "country", functions = c(power, sqrt = sqrt), verbose = verbose, name_separator = ".."))
          })

	
## No aggragtion to perform	
data("adult")	
# reduce it to save time
adult <- adult[1:5000, ]
adult$id <- 1:nrow(adult)
store_nrow <- nrow(adult)
test_that("aggegateByKey: no aggregation",
          {
            expect_equal(nrow(aggregateByKey(adult, key = "id", verbose = verbose)), store_nrow)
          })	
		  

## Column that is a copy of key		  
data("adult")	
adult <- adult[1:5000, ]
adult$country2 <- adult$country
adult$boolcall <- sample(c(TRUE, FALSE), nrow(adult), replace = TRUE)
adult_agg <- aggregateByKey(adult, key = "country", verbose = verbose)
test_that("aggegateByKey: Column that is a copy of key	",
          {
            expect_equal(all(adult_agg$country2 == adult_agg$country), TRUE)
          })	