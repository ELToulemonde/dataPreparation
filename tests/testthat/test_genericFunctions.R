## findNFirstNonNull
#-------------------

## checkAndReturnDataTable
#-------------------------


## checkIfIsColumn
#------------------

dataSet <- data.table(a = "1")
is.col(dataSet, cols = "a")

expect_error(is.col(dataSet, cols = "b"), ". should be column of dataSet")
             
## getPossibleSeparators
#------------------------
result <- getPossibleSeparators()

## printl
#--------

## controlNumberOfRows
#--------------------
dataSet <- data.table(col1 = c(1, 2, 3))
control_nb_rows(dataSet, 1)

test_that("control_nb_rows:", 
          {
            expect_equal(control_nb_rows(dataSet, 1), 1)
			expect_equal(control_nb_rows(dataSet, 10), 3)
			expect_warning(control_nb_rows(dataSet, 10), "You want to check more rows than there are in dataSet, I set")
			expect_warning(control_nb_rows(dataSet, 0), "You want to check at least a few rows than there are in dataSet, I set ")
          })