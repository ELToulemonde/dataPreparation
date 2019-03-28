context("test_aggregate.R")
requireNamespace("data.table")
verbose <- TRUE

## aggregateByKey
# ---------------
test_that("aggregateByKey: with unique key, should preserve the number of line",
          {
            # Given
            data("adult")            
            adult <- adult[1:5000, ] # Reduce it to save time
            adult$key <- 1:nrow(adult)
            stored_nrow <- nrow(adult)
            
            # When
            aggregated_adult <- aggregateByKey(adult, key = "key", verbose = verbose)
            
            # Then
            expect_equal(stored_nrow, nrow(aggregated_adult))
          })

test_that("aggregateByKey: with at least 1 aggregated col should generate nbr_lines col",
          {
            # Given
            data("adult")            
            adult <- adult[1:5000, ] # Reduce it to save time
            cols <- c("country", "age")
            
            # When
            aggregated_adult <- aggregateByKey(adult[, cols], key = "country", verbose = verbose)
            
            # Then
            expect_true(all(c("country", "nbr_lines") %in% colnames(aggregated_adult)))
          })

test_that("aggregateByKey: date are not handled, error should be thrown",
          {
            # Given
            data("messy_adult")     
            messy_adult <- messy_adult[1:5000, ]
            messy_adult[["date3"]] <- as.Date(messy_adult[["date3"]], format = "%d-%m-%Y")
            cols <- c("country", "date3")
            
            # When + Then
            expect_error(aggregateByKey(messy_adult[, cols, with = FALSE], key = "country", verbose = verbose), 
                         "I can only handle: numeric, integer, factor, logical, character columns. ")
          })

## aggregateAcolumn
# ------------------
test_that("private function: aggregateAcolumn: aggregate a character with more than thresh unique values should generate one column count of unique.",
          {
            # Given
            data("adult")
            setDT(adult)
            unique_keys <- unique(adult[, "country", with = FALSE])
            n_country <- nrow(unique_keys)
            adult[["character_col"]] <- sample(LETTERS, nrow(adult), replace = TRUE)
            cols <- c("country", "character_col")
            
            # When
            aggregated_col <- aggregateAcolumn(adult[, cols, with = FALSE], col = "character_col", key = "country", 
                                               unique_keys = unique_keys, thresh = 2)
            # Then
            expect_equal(n_country, nrow(aggregated_col))
            expect_equal(2, ncol(aggregated_col))
            expect_equal(c("country", "n_unique.character_col"), colnames(aggregated_col))
          })

test_that("private function: aggregateAcolumn: aggregate a character with les than thresh unique values should generate one column per unique values.",
          {
            # Given
            data("adult")
            setDT(adult)
            unique_keys <- unique(adult[, "country", with = FALSE])
            n_country <- nrow(unique_keys)
            adult[["character_col"]] <- sample(c("A", "B"), nrow(adult), replace = TRUE)
            cols <- c("country", "character_col")
            
            # When
            aggregated_col <- aggregateAcolumn(adult[, cols, with = FALSE], col = "character_col", key = "country", 
                                               unique_keys = unique_keys, thresh = 2)
            # Then
            expect_equal(n_country, nrow(aggregated_col))
            expect_equal(3, ncol(aggregated_col))
            expect_equal(c("country", "character_col.A", "character_col.B"), colnames(aggregated_col))
          })

test_that("private function: aggregateAcolumn: aggregate a logical column. Should create 1 column with count of true value",
          {
            # Given
            data("adult")
            setDT(adult)
            unique_keys <- unique(adult[, "country", with = FALSE])
            n_country <- nrow(unique_keys)
            adult[["logical_col"]] <- sample(c(TRUE, FALSE), nrow(adult), replace = TRUE)
            cols <- c("country", "logical_col")
            
            # When
            aggregated_col <- aggregateAcolumn(adult[, cols, with = FALSE], col = "logical_col", key = "country", 
                                               unique_keys = unique_keys)
            # Then
            expect_equal(n_country, nrow(aggregated_col))
            expect_equal(2, ncol(aggregated_col))
            expect_equal(c("country", "nbr_true.logical_col"), colnames(aggregated_col))
          })

test_that("private function: aggregateAcolumn: aggregate a numeric column with 2 functions. Should create 1 column for each function result",
          {
            # Given
            data("adult")
            setDT(adult)
            unique_keys <- unique(adult[, "country", with = FALSE])
            n_country <- nrow(unique_keys)
            cols <- c("country", "age")
            functions <- c("mean", "sd")
            
            # When
            aggregated_col <- aggregateAcolumn(adult[, cols, with = FALSE], col = "age", key = "country", 
                                               unique_keys = unique_keys, functions = functions)
            # Then
            expect_equal(n_country, nrow(aggregated_col))
            expect_equal(3, ncol(aggregated_col))
            expect_equal(c("country", "mean.age", "sd.age"), colnames(aggregated_col))
          })

test_that("private function: aggregateAcolumn: with duplicated col should return same result on both.",
          {
            # Given
            data("adult")
            setDT(adult)
            unique_keys <- unique(adult[, "country", with = FALSE])
            n_country <- nrow(unique_keys)
            adult[["country2"]] <- adult[["country"]]
            cols <- c("country", "country2")
            
            # When
            aggregated_col <- aggregateAcolumn(adult[, cols, with = FALSE], col = "country2", key = "country", 
                                               unique_keys = unique_keys, functions = functions)
            # Then
            expect_equal(n_country, nrow(aggregated_col))
            expect_equal(aggregated_col[["country"]], aggregated_col[["country2"]])
          })

test_that("private function: aggregateAcolumn: should thow error on more than 2 cols set",
          {
            # Given
            data("adult")
            cols <- c("country", "age", "education")
            
            # When + Then
            expect_error(aggregateAcolumn(adult[, cols], col = "country", key = "country", unique_keys = unique_keys), 
                         ": dataSet should have 2 columns.")
          })
