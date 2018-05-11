context("test_onLoad.R")
requireNamespace("data.table")
verbose <- TRUE

## dataPrepNews
# -------------
dataPrepNews()

## .onAttach
# ----------
.onAttach("", "dataPreparation")