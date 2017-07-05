
require(dataPreparation)
data(messy_adult)

adult_agg <-  prepareSet(messy_adult, key = "country", verbose = FALSE)

test_that("prepareSet:", 
          {
            expect_equal(length(unique(adult_agg$country)), nrow(adult_agg))
          })