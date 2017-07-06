requireNamespace("data.table")
## aggregateByKey
# ---------------

data("adult")
# reduce it to save time
adult <- adult[1:5000, ]

# Aggregate it using aggregateByKey, in order to extract characteristics for each country
adult_aggregated <- aggregateByKey(adult, key = 'country', verbose = FALSE)


test_that("aggegateByKey: ",
          {
            expect_equal(ncol(adult_aggregated), 87)
          }
)

# Exmple with other functions

data("adult")
# reduce it to save time
adult <- adult[1:5000, ]

power <- function(x){sum(x^2)}
adult_aggregated <- aggregateByKey(adult, key = 'country', functions = power, verbose = FALSE)

test_that("aggegateByKey: add function",
          {
            expect_equal(ncol(adult_aggregated), 69)
          })


# Exmple with other functions


data("adult")
# reduce it to save time
adult <- adult[1:5000, ]

test_that("aggegateByKey: add function that is not an agg function",
          {
            expect_warning(aggregateByKey(adult, key = 'country', functions = c(power, sqrt), verbose = FALSE))
          })
