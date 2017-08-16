## ----results='hide', message=FALSE, warning=FALSE, echo=FALSE------------
library(dataPreparation)
library(data.table)
library(knitr)
library(kableExtra)
library(pander)
options(knitr.table.format = "html") 
Sys.setlocale("LC_TIME", "C")

## ----comment=""----------------------------------------------------------
data(messy_adult)
print(head(messy_adult, n = 4))

## ------------------------------------------------------------------------
print(class(messy_adult$mail))
messy_adult <- unFactor(messy_adult)
print(class(messy_adult$mail))

## ----echo = FALSE, results='hide'----------------------------------------
setDT(messy_adult)
store <- copy(messy_adult[,.(date1, date2, date3, date4)])

## ------------------------------------------------------------------------
messy_adult <- findAndTransformDates(messy_adult)

## ----echo = FALSE, results='asis'----------------------------------------
setnames(store, paste0(names(store), "_prev"))
toPlot <- cbind(head(store, n=6), data.frame("transfo" = rep("  =>", 6)), head(messy_adult[,.(date1, date2, date3, date4)], n = 6))

kable(toPlot) %>%
   kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE, font_size = 12)


## ----echo = FALSE, results='hide'----------------------------------------
store <- copy(messy_adult[,.(num1, num2, num3)])

## ------------------------------------------------------------------------
messy_adult <- findAndTransformNumerics(messy_adult)

## ----echo = FALSE, results='asis'----------------------------------------
setnames(store, paste0(names(store), "_prev"))
toPlot <- cbind(head(store, n=6), data.frame("transfo" = rep("  =>", 6)), head(messy_adult[,.(num1, num2, num3)], n = 6))

kable(toPlot) %>%
   kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE, font_size = 12)


## ---- results = 'hold'---------------------------------------------------
constant_cols <- whichAreConstant(messy_adult)

## ---- results = 'hold'---------------------------------------------------
double_cols <- whichAreInDouble(messy_adult)

## ---- results = 'hold'---------------------------------------------------
bijections_cols <- whichAreBijection(messy_adult)

## ------------------------------------------------------------------------
kable(head(messy_adult[, .(constant, date3, date4, num1, num3, education, education_num)])) %>%
   kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE, font_size = 12)

## ----results = "hold"----------------------------------------------------
ncols <- ncol(messy_adult)
messy_adult <- fastFilterVariables(messy_adult)
print(paste0("messy_adult now have ", ncol(messy_adult), " columns; so ", ncols - ncol(messy_adult), " less than before."))

## ------------------------------------------------------------------------
messy_adult <- fastRound(messy_adult, digits = 2)

## ----echo=FALSE----------------------------------------------------------
kable(cbind(head(messy_adult[, 1:6, with = FALSE], n = 6), data.frame("..." = rep("  ...", 6)))) %>%
   kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE, font_size = 12)

## ------------------------------------------------------------------------
messy_adult <- generateDateDiffs(messy_adult, analysisDate = as.Date("2018-01-01"), units = "days")

## ----echo=FALSE----------------------------------------------------------
kable(cbind(data.frame("..." = rep("  ...", 6)), head(messy_adult[, (ncol(messy_adult) - 4):ncol(messy_adult), with = FALSE], n = 6))) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE, font_size = 12)

## ------------------------------------------------------------------------
date_cols <- names(messy_adult)[sapply(messy_adult, is.POSIXct)]
messy_adult <- generateFactorFromDate(messy_adult, cols = date_cols, type = "quarter")

## ----echo=FALSE----------------------------------------------------------
kable(cbind(data.frame("..." = rep("  ...", 6)), head(messy_adult[, (ncol(messy_adult) - 2):ncol(messy_adult), with = FALSE], n = 6))) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE, font_size = 12)

## ------------------------------------------------------------------------
messy_adult[, c(date_cols) := NULL]

## ------------------------------------------------------------------------
messy_adult <- generateFromCharacter(messy_adult, cols = "auto", drop_cols = TRUE)

## ----echo=FALSE----------------------------------------------------------
kable(head(messy_adult[, .(mail.notnull, mail.num, mail.order)], n = 6)) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE, font_size = 12)

## ------------------------------------------------------------------------
agg_adult <- aggregateByKey(messy_adult, key = "country")

## ----echo=FALSE----------------------------------------------------------
print(paste0(ncol(agg_adult), " columns have been built; for ", nrow(agg_adult), " countries."))
kable(cbind(head(agg_adult[,c(1,13,23,35,45)]), data.frame("..." = rep("  ...", 6)))) %>%
   kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE, font_size = 12)

## ------------------------------------------------------------------------
messy_adult <- fastHandleNa(messy_adult)

## ----echo=FALSE----------------------------------------------------------
print(cbind(head(messy_adult[,1:4, with = FALSE], n = 4), data.frame("..." = rep("  ...", 4)), head(messy_adult[,15:ncol(messy_adult), with = FALSE], n = 4)))

## ------------------------------------------------------------------------
clean_adult = shapeSet(copy(messy_adult), finalForm = "data.table", verbose = FALSE)
print(table(sapply(clean_adult, class)))

## ------------------------------------------------------------------------
clean_adult <- shapeSet(copy(messy_adult), finalForm = "numerical_matrix", verbose = FALSE)

## ----echo=FALSE----------------------------------------------------------
kable(cbind(head(clean_adult[,1:6]), data.frame("..." = rep("  ...", 6)))) %>%
   kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE, font_size = 12)

## ----warning = FALSE-----------------------------------------------------
data("messy_adult")
agg_adult <- prepareSet(messy_adult, finalForm = "data.table", key = "country", analysisDate = Sys.Date(), digits = 2)

## ----echo=FALSE----------------------------------------------------------
print(paste0(ncol(agg_adult), " columns have been built; for ", nrow(agg_adult), " countries."))
kable(cbind(head(agg_adult[,1:7]), data.frame("..." = rep("  ...", 6)))) %>%
   kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE, font_size = 12)

## ---- tidy=TRUE, tidy.opts=list(length.cutoff=10)------------------------
description(agg_adult, path_to_write = "report.txt")

## ----echo=FALSE----------------------------------------------------------
if (file.exists("report.txt")) file.remove("report.txt")

