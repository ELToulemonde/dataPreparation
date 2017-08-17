requireNamespace("data.table")
verbose <- TRUE

## sameShape
#------------
function_name <- "sameShape"

data("messy_adult")

# Exception handling

test_that(paste0( function_name, ": exeption handling "),
          {
            expect_error(sameShape(messy_adult, "a", verbose = verbose), "referenceSet should be a data.table, data.frame or matrix.")
          })


# Correct computation
data("messy_adult")
messy_adult <- messy_adult[1:150, ] # reduce to save time
messy_adult <- findAndTransformDates(messy_adult, verbose = FALSE)
messy_adult <- findAndTransformNumerics(messy_adult, verbose = FALSE)
messy_adult[, age := NULL] # drop it to check column droping
data("adult")


adult_redone <- sameShape(adult, messy_adult, verbose = verbose)

test_that(paste0( function_name, ": "),
          {
            expect_equal(ncol(adult_redone), ncol(messy_adult))
            expect_identical(names(adult_redone), names(messy_adult))
            expect_true(is.numeric(adult_redone$constant))
            expect_true(is.POSIXct(adult_redone$date1))
            expect_identical(levels(adult_redone$mail), levels(messy_adult$mail))
            expect_null(adult_redone$age)
          })


# test df
setDF(messy_adult)

test_that(paste0( function_name, ": transform to data frame"),
          {
            expect_equal(class(sameShape(adult, messy_adult, verbose = verbose)), "data.frame")
          }
)

# Different type
data(adult)
adult_num <- shapeSet(adult, finalForm = "numerical_matrix", verbose = FALSE)

test_that(paste0( function_name, ": transform to numerical matrix"),
          {
            expect_true(is.matrix(sameShape(adult, adult_num, verbose = verbose)))
          }
)

# Test warnings
rm(messy_adult)
data("messy_adult")
data("adult")
setDF(adult)
class(messy_adult[["age"]]) <- "weirdClass"
test_that(paste0( function_name, ": warnings"),
          {
            expect_warning(sameShape(adult, messy_adult, verbose = verbose), " and i don't know how to transform it.")
          }
)
