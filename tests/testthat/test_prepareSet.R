context("test_prepareSet.RR")
verbose <- TRUE

## prepareSet
# -----------
data("adult")
adult <- adult[1:500, ]
adult_agg <-  prepareSet(adult, key = "country", digits = 2, n_unfactor = 10, verbose = verbose, functions = c("mean", "max"))

test_that("prepareSet:", 
          {
            expect_equal(length(unique(adult_agg$country)), nrow(adult_agg))
          })


