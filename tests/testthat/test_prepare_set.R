context("test_prepare_set.RR")
verbose <- TRUE

## prepare_set
# -----------
test_that("prepare_set: functionnal test: test full pipeline. Should give result with as many rows as unique key.", {
    # Given
    data("adult")
    adult <- adult[1 : 500, ]
    key <- "country"
    n_unique_keys <- uniqueN(adult[[key]])

    # When
    adult_agg <- prepare_set(adult, key = key, digits = 2, n_unfactor = 10, verbose = verbose,
    functions = c("mean", "max"), target_col = "capital_gain")

    # Then
    expect_equal(n_unique_keys, nrow(adult_agg))
})