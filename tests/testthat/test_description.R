context("test_description.R")
requireNamespace("data.table")
verbose <- TRUE
## Load data set
data(messy_adult)

description(messy_adult, level = 1, verbose = verbose)
description(messy_adult, level = 1, path_to_write="report.txt", verbose = verbose)
if (file.exists("report.txt")) file.remove("report.txt")

test_that("description: errors: ",
         {
           expect_error(description(messy_adult, level = -1, verbose = verbose))
         })


toydata <- data.table(col1 = 1:2, col2 = c(TRUE, FALSE), col3 = c(Sys.Date(), Sys.Date()))
description(toydata)