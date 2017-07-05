## findAndTransformNumerics
#--------------------------


dataSet <- data.table(ID = 1:5,
                 col1 = c("1.2", "1.3", "1.2", "1", "6"), 
                 col2 = c("1,2", "1,3", "1,2", "1", "6")
                 )

data_transformed <- findAndTransformNumerics(dataSet, n_test = 5, verbose = FALSE)

test_that("findAndTransformNumerics:",
          {
            expect_equal(all(sapply(data_transformed, class) == c("integer", "numeric", "numeric")), TRUE)
          })
