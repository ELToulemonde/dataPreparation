context("test_sameShape.R")
requireNamespace("data.table")
verbose <- TRUE

## sameShape
#------------
test_that("sameShape: find and create missing column",
          {
            # Given
            referenceSet <- data.table(col_1 = c(1, 2, 3),
                                        col_2 = c(2, 3, 4))
            dataSet <- data.table(col_1 = c(1, 2)) 
            
            # When
            dataSet_redone <- sameShape(dataSet, referenceSet, verbose = verbose)
            
            # Then
            expect_true("col_2" %in% colnames(dataSet_redone))
          })

test_that("sameShape: find and remove unwanted column",
          {
            # Given
            referenceSet <- data.table(col_1 = c(1, 2, 3))
            dataSet <- data.table(col_1 = c(1, 2),
                                  col_2 = c(2, 3)) 
            
            # When
            dataSet_redone <- sameShape(dataSet, referenceSet, verbose = verbose)
            
            # Then
            expect_false("col_2" %in% colnames(dataSet_redone))
          })

test_that("sameShape: find and and transform to numeric misstyped character col",
          {
            # Given
            referenceSet <- data.table(col_1 = c(1, 2, 3))
            dataSet <- data.table(col_1 = c("1", "2")) 
            
            # When
            dataSet_redone <- sameShape(dataSet, referenceSet, verbose = verbose)
            
            # Then
            expect_true(is.numeric(dataSet_redone[["col_1"]]))
          })

test_that("sameShape: find and and transform to POSIXct misstyped character col",
          {
            # Given
            referenceSet <- data.table(col_1 = as.POSIXct(c("2018-01-31", "2019-07-12")))
            dataSet <- data.table(col_1 = c("2018-02-12", "2018-09-26")) 
            
            # When
            dataSet_redone <- sameShape(dataSet, referenceSet, verbose = verbose)
            
            # Then
            expect_true(is.POSIXct(dataSet_redone[["col_1"]]))
          })

test_that("sameShape: find and and add missing levels on factors",
          {
            # Given
            referenceSet <- data.table(col_1 = as.factor(c(1, 2, 3)))
            dataSet <- data.table(col_1 = as.factor(c(1, 2)))
            
            # When
            dataSet_redone <- sameShape(dataSet, referenceSet, verbose = verbose)
            
            # Then
            expect_true("3" %in% levels(dataSet_redone[["col_1"]]))
          })

test_that("sameShape: find and and remove unwanted levels on factors",
          {
            # Given
            referenceSet <- data.table(col_1 = as.factor(c(1, 2)))
            dataSet <- data.table(col_1 = as.factor(c(1, 2, 3)))
            
            # When
            dataSet_redone <- sameShape(dataSet, referenceSet, verbose = verbose)
            
            # Then
            expect_false("3" %in% levels(dataSet_redone[["col_1"]]))
          })

test_that("sameShape: throw warning when column is in a weird class that we don't know how to transform",
          {
            # Given
            referenceSet <- data.table(col_1 = c(1, 2, 3))
            class(referenceSet[["col_1"]]) <- "a_weird_class" 
            dataSet <- data.table(col_1 = c(1, 2, 3)) 
            
            # When + Then
            expect_warning(sameShape(dataSet, referenceSet, verbose = verbose), 
                           " and i don't know how to transform it.")
          })

test_that("sameShape: when column is in a weird class if method to transform exist",
          {
            # Given
            referenceSet <- data.table(col_1 = c(1, 2, 3))
            class(referenceSet[["col_1"]]) <- "weirdClass" 
            as.weirdClass <- function(x){class(x) <- "weirdClass"; return(x)}
            attach(list(as.weirdClass=as.weirdClass))
            dataSet <- data.table(col_1 = c(1, 2, 3)) 
            
            # When
            dataSet_redone <- sameShape(dataSet, referenceSet, verbose = verbose)
            
            # Then
            expect_equal(class(dataSet_redone[["col_1"]]), "weirdClass")
            
            # Clean up
            detach(list(as.weirdClass=as.weirdClass))
          })

test_that("sameShape: throw warning when column is in a weird class if method to transform exist but doesn't perform correctly.",
          {
            # Given
            referenceSet <- data.table(col_1 = c(1, 2, 3))
            class(referenceSet[["col_1"]]) <- "weirdClass" 
            as.weirdClass <- function(x){x}
            attach(list(as.weirdClass=as.weirdClass))
            dataSet <- data.table(col_1 = c(1, 2, 3)) 
            
            # When + Then 
            expect_warning(sameShape(dataSet, referenceSet, verbose = verbose),
                           ": transformation didn't work. Please control that function ")
          })

# test df
test_that("sameShape: transform shape into numerical matrix",
          {
            # Given
            data("adult")
            adult <- adult[1:150, ] # reduce it to save time
            adult2 <- copy(adult)
            setDT(adult2)
            adult_num <- shapeSet(adult2, finalForm = "numerical_matrix", verbose = FALSE)
            
            # When
            adult_reshaped <- sameShape(adult, adult_num, verbose = verbose)
            print("Here")
            # Then
            expect_true(is.matrix(adult_reshaped))
          })


test_that("sameShape: transform shape into data.frame",
          {
            # Given
            dataSet_1 <-data.frame(col = 1:10)
            dataSet_2 <- copy(dataSet_1)
            setDT(dataSet_2)
            
            # When
            reshaped_dataSet_2 <- sameShape(dataSet_2, dataSet_1, verbose = verbose)
            
            # Then
            expect_true(is.data.frame(reshaped_dataSet_2))
          })
