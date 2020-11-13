context("test_on_load.R")
requireNamespace("data.table")
verbose <- TRUE

## dataPreparationNews
# -------------
data_preparation_news()

## .onAttach
# ----------
.onAttach("", "dataPreparation")
