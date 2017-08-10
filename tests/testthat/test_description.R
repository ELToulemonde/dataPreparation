## Load data set
data(messy_adult)
verbose = TRUE

description(messy_adult, level = 1, verbose = verbose)
description(messy_adult, level = 1, path_to_write="report.txt", verbose = verbose)


test_that("description: errors: ",
         {
           expect_error(description(messy_adult, level = -1, verbose = verbose))
         })
