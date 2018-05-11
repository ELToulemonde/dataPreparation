context("test_sameShape.R")
requireNamespace("data.table")
verbose <- TRUE

## sameShape
#------------
function_name <- "sameShape"

# Correct computation
data("messy_adult")
messy_adult <- messy_adult[1:150, ] # reduce to save time
data("adult")
adult <- adult[1:150, ]

messy_adult <- findAndTransformDates(messy_adult, verbose = FALSE)
messy_adult <- findAndTransformNumerics(messy_adult, verbose = FALSE)
messy_adult[, age := NULL] # drop it to check column droping
class(messy_adult[["education_num"]]) <- "weirdClass" # Weird class transformation warning
class(messy_adult[["education"]]) <- "weirdClass2" # Weird class transformation warning
as.weirdClass2 <- function(x){as.numeric(x)}
attach(list(as.weirdClass2=as.weirdClass2))


test_that(paste0( function_name, ": "),
          {
            expect_warning(adult_redone <- sameShape(copy(adult), copy(messy_adult), verbose = verbose), " and i don't know how to transform it.")
            expect_equal(ncol(adult_redone), ncol(messy_adult))
            expect_identical(names(adult_redone), names(messy_adult))
            expect_true(is.numeric(adult_redone$constant))
            expect_true(is.POSIXct(adult_redone$date1))
            expect_identical(levels(adult_redone$mail), levels(messy_adult$mail))
            expect_null(adult_redone$age)
          })


# test df
data("adult")
setDF(adult)
adult2 <- copy(adult)
setDT(adult2)
adult_num <- shapeSet(adult2, finalForm = "numerical_matrix", verbose = FALSE)
test_that(paste0( function_name, ": transform shape"),
          {
            expect_equal(class(sameShape(copy(adult2), copy(adult), verbose = verbose)), "data.frame")
            expect_true(is.matrix(sameShape(adult, adult_num, verbose = verbose)))
          })