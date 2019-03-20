context("test_prepareSet.RR")
verbose <- TRUE

## prepareSet
# -----------
test_that("prepareSet: functionnal test: test full pipeline. Should give result with as many rows as unique key.", 
          {
            # Given
            data("adult")
            adult <- adult[1:500, ]
            key <- "country"
            n_unique_keys <- uniqueN(adult[[key]])
            
            # When
            adult_agg <-  prepareSet(adult, key = key, digits = 2, n_unfactor = 10, verbose = verbose, 
                                     functions = c("mean", "max"))
            
            # Then
            expect_equal(n_unique_keys, nrow(adult_agg))
          })


