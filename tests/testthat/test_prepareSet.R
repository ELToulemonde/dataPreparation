verbose = TRUE

## prepareSet
# -----------
data(messy_adult)
adult_agg <-  prepareSet(messy_adult, key = "country", digits = 2, n_unfactor = 10, verbose = verbose, functions = c(mean, max))

test_that("prepareSet:", 
          {
            expect_equal(length(unique(adult_agg$country)), nrow(adult_agg))
          })
		  
		  
adult_agg <-  prepareSet(messy_adult, verbose = verbose)
