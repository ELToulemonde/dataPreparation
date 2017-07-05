## Documentation for unit testing
#--------------------------------
# http://r-pkgs.had.co.nz/tests.html
# http://stat545.com/packages05_foofactors-package-02.html

library(data.table)
## fastRound
##----------
## To-do



## Handle NA Values
#-------------------
dataSet <-  data.table(numCol = c(1, 2, 3, NA), 
                   charCol = c("", "a", NA, "c"), 
                   booleanCol = c(TRUE, NA, FALSE, NA))

# To set NAs to 0, FALSE and "" (respectivly for numeric, boolean, character)
data_withoutNA <- fastHandleNa(dataSet)

test_that("fastHandleNa: There are no more NAs", 
          {
            expect_equal(sum(is.na(data_withoutNA)), 0)
          })


## fastIsEqual
#--------------

## To-do



## fastFilterVariables
#---------------------
# To-do