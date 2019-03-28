context("test_generateFromCharacter.R")
requireNamespace("data.table")
verbose <- TRUE
## generateFromCharacter
# -------------------
test_that("generateFromCharacter: don't drop so generate 3 new cols",
          {
            # Given
            dataSet <- data.table(character_col = LETTERS,
                                  other_col = LETTERS)
            store_ncol <- ncol(dataSet)
            
            # When
            dataSet_transformed <- generateFromCharacter(dataSet, cols = "character_col")
            
            # Then
            expect_equal(ncol(dataSet_transformed), store_ncol + 3)
          })


test_that("generateFromCharacter: drop generate 3 col and suppress one",
          {
            # Given
            dataSet <- data.table(character_col = LETTERS)
            store_ncol <- ncol(dataSet)
            
            # When
            dataSet_transformed <- generateFromCharacter(dataSet, drop = TRUE)
            
            # Then
            expect_equal(ncol(dataSet_transformed), store_ncol + 2)
          })
