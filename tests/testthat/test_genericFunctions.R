## findNFirstNonNull
#-------------------


test_that("findNFirstNonNull: test numerics", 
          {
            expect_equal(all(findNFirstNonNull(1:50, 5) == 1:5), TRUE)
            expect_equal(all(findNFirstNonNull(1:50, 10) == 1:10), TRUE)
            expect_equal(all(findNFirstNonNull(c(NA, 1:50), 10) == 1:10), TRUE)
          })


test_that("findNFirstNonNull: test character", 
          {
            expect_equal(all(findNFirstNonNull(LETTERS, 3) == c("A", "B", "C")), TRUE)
            expect_equal(all(findNFirstNonNull(LETTERS, 5) == c("A", "B", "C", "D", "E")), TRUE)
            expect_equal(all(findNFirstNonNull(c(NA, LETTERS), 5) == c("A", "B", "C", "D", "E")), TRUE)
          })

## checkAndReturnDataTable
#-------------------------
data("messy_adult")

test_that("checkAndReturnDataTable", 
          {
            expect_equal(all(class(checkAndReturnDataTable(messy_adult)) == c("data.table", "data.frame")), TRUE)
            expect_equal(all(class(checkAndReturnDataTable(as.data.frame(messy_adult))) == c("data.table", "data.frame")), TRUE)
            expect_equal(all(class(checkAndReturnDataTable(as.matrix(messy_adult))) == c("data.table", "data.frame")), TRUE)
            
            expect_error(checkAndReturnDataTable("a"))
            expect_error(checkAndReturnDataTable(1))
            expect_error(checkAndReturnDataTable(list(1,2)))
            
            expect_error(checkAndReturnDataTable(data.table()))
          })


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
printl("printl", " is a private fuction ", " easier to use than print")

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