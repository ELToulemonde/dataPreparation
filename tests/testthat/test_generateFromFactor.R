context("test_generateFromFactor.R")
requireNamespace("data.table")
verbose <- TRUE
## generateFromFactor
# -------------------
test_that("generateFromFactor: drop: functionnal test on reference set",
          {
            # Given
            data("messy_adult")
            messy_adult <- messy_adult[1:500, ] # Reduce it to save time
            store_ncol <- ncol(messy_adult)
            n_factor <- sum(sapply(messy_adult, is.factor))
            
            # When
            messy_adult <- generateFromFactor(messy_adult, drop = TRUE, verbose = verbose)
            
            # Then
            expect_equal(ncol(messy_adult), store_ncol + 2 * n_factor)
          })

test_that("generateFromFactor: test don't drop => keep original col",
          {
            # Given
            dataSet <- data.table(factor_col = as.factor(LETTERS))
            store_ncol <- ncol(dataSet)
            n_factor <- sum(sapply(dataSet, is.factor))
            
            # When
            dataSet_transformed <- generateFromFactor(dataSet, drop = FALSE, verbose = verbose)
            
            # Then
            expect_equal(ncol(dataSet_transformed), store_ncol + 3 * n_factor)
            expect_true(all(colnames(dataSet) %in% colnames(dataSet_transformed)))
          })

## one_hot_encoder
# ----------------
test_that("one_hot_encoder: ",
          {
            # Given
            dataSet <- data.table(character_col = LETTERS)
            
            # When
            dataSet_one_hot_encoded <- one_hot_encoder(dataSet, verbose = verbose, drop = TRUE)
            
            # Then
            expect_equal(ncol(dataSet_one_hot_encoded), uniqueN(LETTERS))
          })


test_that("one_hot_encoder: expect error: ",
          {
            # Given 
            dataSet <- data.table(character_col = LETTERS)
            wrong_type <- "character"
            
            # When + Then
            expect_error(one_hot_encoder(dataSet, type = wrong_type), ": type should either be ")
          })


## build_encoding
# ----------------
test_that("build_encoding: ",
          {
            # Given
            dataSet <- data.table(factor_col = as.factor(LETTERS))
            n_factor <- sum(sapply(dataSet, is.factor))
            
            # When
            encoding <- build_encoding(dataSet)
            
            # Then
            expect_equal(length(encoding), n_factor)
            expect_equal(length(encoding[["factor_col"]]$new_cols), uniqueN(LETTERS))
            expect_equal(encoding[["factor_col"]]$values, unique(LETTERS))
          })


test_that("build_encoding: min_frequency allows to drop rare values",
          {
            # Given
            dataSet <- data.table(factor_col = c(rep("A", 100), "B"))
            
            # When
            encoding <- build_encoding(dataSet, verbose = verbose, min_frequency = 0.1)
            
            # Then
            expect_equal(encoding[["factor_col"]]$values, "A")
          })

# Target encoding
# ---------------
test_that("target_encode: should set correct value for each student",
          {
            # Given
            dataSet <- data.table(student = c("Marie", "Marie", "Pierre", "Louis", "Louis"), 
                                  grades = c(1, 1, 2, 3, 4))
            target_encoding <- build_target_encoding(dataSet, col_to_encode = "student", 
                                                     target_col = "grades", functions = c("mean"))
            
            expected_result <- data.table(student = c("Marie", "Marie", "Pierre", "Louis", "Louis"), 
                                          grades = c(1, 1, 2, 3, 4),
                                          grades_mean_by_student = c(1, 1, 2, 3.5, 3.5))
            # When
            result <- target_encode(dataSet, col_to_encode = "student", target_encoding)
            
            # Then
            expect_equal(result, expected_result)
          })

test_that("target_encode: if drop is asked col_to_encode should not be in columns anymore",
          {
            # Given
            col_to_encode <- "student"
            dataSet <- data.table(student = c("Marie", "Marie", "Pierre", "Louis", "Louis"), 
                                  grades = c(1, 1, 2, 3, 4))
            target_encoding <- build_target_encoding(dataSet, col_to_encode = col_to_encode, 
                                                     target_col = "grades", functions = c("mean"))
            
            # When
            result <- target_encode(dataSet, col_to_encode = col_to_encode, target_encoding, drop = TRUE)
            
            # Then
            expect_false(col_to_encode %in% names(result))
          })

# Build target encoding
# ---------------------
test_that("build_target_encoding: build_target_encoding should return a data.table with for each unique value mean of target",
          {
            # Given
            dataSet <- data.table(col1 = c("a", "a", "b", "c", "c"),
                             target = c(1, 1, 2, 3, 4))
            expected_target_encoded <- data.table(col1 = c("a", "b", "c"),
                                                  target_mean_by_col1 = c(1, 2, 3.5))
            # When
            target_encoded <- build_target_encoding(dataSet, col_to_encode="col1", target_col="target", functions="mean")
            
            # Then
            expect_equal(target_encoded, expected_target_encoded)
          })


test_that("build_target_encoding: build_target_encoding should return length and mean when asked for",
          {
            # Given
            dataSet <- data.table(col1 = c("a", "a", "b", "c", "c"),
                                  target = c(1, 1, 2, 3, 4))
            expected_target_encoded <- data.table(col1 = c("a", "b", "c"),
                                                  target_mean_by_col1 = c(1, 2, 3.5),
                                                  target_length_by_col1 = c(2, 1, 2))
            # When
            target_encoded <- build_target_encoding(dataSet, col_to_encode="col1", target_col="target", functions=c("mean", "length"))
            
            # Then
            expect_equal(target_encoded, expected_target_encoded)
          })
