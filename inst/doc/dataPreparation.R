## ----results='hide', message=FALSE, warning=FALSE, echo=FALSE------------
library(dataPreparation)
library(data.table)
library(knitr)
library(kableExtra)
library(pander)
options(knitr.table.format = "html") 
Sys.setlocale("LC_TIME", "C")

# A Prefix nulling hook.
# source: https://stackoverflow.com/questions/22524822/how-can-i-remove-the-prefix-index-indicator-1-in-knitr-output
# Make sure to keep the default for normal processing.
default_output_hook <- knitr::knit_hooks$get("output")

# Output hooks handle normal R console output.
knitr::knit_hooks$set( output = function(x, options) {

  comment <- knitr::opts_current$get("comment")
  if( is.na(comment) ) comment <- ""
  can_null <- grepl( paste0( comment, "\\s*\\[\\d?\\]" ),
                     x, perl = TRUE)
  do_null <- isTRUE( knitr::opts_current$get("null_prefix") )
  if( can_null && do_null ) {
    # By default R print output aligns at the right brace.
    align_index <- regexpr( "\\]", x )[1] - 1
    # Two cases: start or newline
    re <- paste0( "^.{", align_index, "}\\]")
    rep <- comment
    x <- gsub( re, rep,  x )
    re <- paste0( "\\\n.{", align_index, "}\\]")
    rep <- paste0( "\n", comment )
    x <- gsub( re, rep,  x )
  }

  default_output_hook( x, options )

})

knitr::opts_template$set("kill_prefix"=list(comment=NA, null_prefix=TRUE))

## ----comment="#",  null_prefix=TRUE--------------------------------------
data(messy_adult)
print(head(messy_adult, n = 4))

## ----comment="#",  null_prefix=TRUE--------------------------------------
print(class(messy_adult$mail))
messy_adult <- unFactor(messy_adult)
print(class(messy_adult$mail))

## ----echo = FALSE, results='hide', comment="#",  null_prefix=TRUE--------
setDT(messy_adult)
store <- copy(messy_adult[,.(date1, date2, date3, date4)])

## ---- comment="#",  null_prefix=TRUE-------------------------------------
messy_adult <- findAndTransformDates(messy_adult)

## ----echo = FALSE, results='asis', comment="#",  null_prefix=TRUE--------
setnames(store, paste0(names(store), "_prev"))
toPlot <- cbind(head(store, n=6), data.frame("transfo" = rep("  =>", 6)), head(messy_adult[,.(date1, date2, date3, date4)], n = 6))

kable(toPlot) %>%
   kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE, font_size = 12)


## ----echo = FALSE, results='hide', comment="#",  null_prefix=TRUE--------
store <- copy(messy_adult[,.(num1, num2, num3)])

## ---- comment="#",  null_prefix=TRUE-------------------------------------
messy_adult <- findAndTransformNumerics(messy_adult)

## ----echo = FALSE, results='asis', comment="#",  null_prefix=TRUE--------
setnames(store, paste0(names(store), "_prev"))
toPlot <- cbind(head(store, n=6), data.frame("transfo" = rep("  =>", 6)), head(messy_adult[,.(num1, num2, num3)], n = 6))

kable(toPlot) %>%
   kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE, font_size = 12)


## ---- results = 'hold', comment="#",  null_prefix=TRUE-------------------
constant_cols <- whichAreConstant(messy_adult)

## ---- results = 'hold', comment="#",  null_prefix=TRUE-------------------
double_cols <- whichAreInDouble(messy_adult)

## ---- results = 'hold', comment="#",  null_prefix=TRUE-------------------
bijections_cols <- whichAreBijection(messy_adult)

## ----comment="#",  null_prefix=TRUE--------------------------------------
kable(head(messy_adult[, .(constant, date3, date4, num1, num3, education, education_num)])) %>%
   kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE, font_size = 12)

## ----results = "hold", comment="#",  null_prefix=TRUE--------------------
ncols <- ncol(messy_adult)
messy_adult <- fastFilterVariables(messy_adult)
print(paste0("messy_adult now have ", ncol(messy_adult), " columns; so ", ncols - ncol(messy_adult), " less than before."))

## ----comment="#",  null_prefix=TRUE--------------------------------------
date_cols <- names(messy_adult)[sapply(messy_adult, is.POSIXct)]
messy_adult <- generateDateDiffs(messy_adult, cols = "auto", analysisDate = as.Date("2018-01-01"), units = "days")

## ----echo=FALSE, comment="#",  null_prefix=TRUE--------------------------
kable(cbind(data.frame("..." = rep("  ...", 6)), head(messy_adult[, (ncol(messy_adult) - 4):ncol(messy_adult), with = FALSE], n = 6))) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE, font_size = 12)

## ----comment="#",  null_prefix=TRUE--------------------------------------
messy_adult <- generateFactorFromDate(messy_adult, cols = date_cols, type = "quarter", drop = TRUE)

## ----echo=FALSE, comment="#",  null_prefix=TRUE--------------------------
kable(cbind(data.frame("..." = rep("  ...", 6)), head(messy_adult[, (ncol(messy_adult) - 2):ncol(messy_adult), with = FALSE], n = 6))) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE, font_size = 12)

## ----comment="#",  null_prefix=TRUE--------------------------------------
messy_adult <- generateFromCharacter(messy_adult, cols = "auto", drop = TRUE)

## ----echo=FALSE, comment="#",  null_prefix=TRUE--------------------------
kable(head(messy_adult[, .(mail.notnull, mail.num, mail.order)], n = 6)) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE, font_size = 12)

## ----comment="#",  null_prefix=TRUE--------------------------------------
agg_adult <- aggregateByKey(messy_adult, key = "country")

## ----echo=FALSE, comment="#",  null_prefix=TRUE--------------------------
kable(cbind(head(agg_adult[,c(1,13,23,35,45)]), data.frame("..." = rep("  ...", 6)))) %>%
   kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE, font_size = 12)

## ----comment="#",  null_prefix=TRUE--------------------------------------
messy_adult <- fastRound(messy_adult, digits = 2)

## ----echo=FALSE, comment="#",  null_prefix=TRUE--------------------------
kable(cbind(head(messy_adult[, 1:6, with = FALSE], n = 6), data.frame("..." = rep("  ...", 6)))) %>%
   kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE, font_size = 12)

## ----comment="#",  null_prefix=TRUE--------------------------------------
messy_adult <- fastHandleNa(messy_adult)

## ----echo=FALSE, comment="#",  null_prefix=TRUE--------------------------
print(cbind(head(messy_adult[,1:4, with = FALSE], n = 4), data.frame("..." = rep("  ...", 4)), head(messy_adult[,15:ncol(messy_adult), with = FALSE], n = 4)))

## ----comment="#",  null_prefix=TRUE--------------------------------------
clean_adult = shapeSet(copy(messy_adult), finalForm = "data.table", verbose = FALSE)
print(table(sapply(clean_adult, class)))

## ----comment="#",  null_prefix=TRUE--------------------------------------
clean_adult <- shapeSet(copy(messy_adult), finalForm = "numerical_matrix", verbose = FALSE)

## ----echo=FALSE----------------------------------------------------------
kable(cbind(head(clean_adult[,1:6]), data.frame("..." = rep("  ...", 6)))) %>%
   kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE, font_size = 12)

## ----warning = FALSE, comment="#",  null_prefix=TRUE---------------------
data("messy_adult")
agg_adult <- prepareSet(messy_adult, finalForm = "data.table", key = "country", analysisDate = Sys.Date(), digits = 2)

## ----echo=FALSE, comment="#",  null_prefix=TRUE--------------------------
print(paste0(ncol(agg_adult), " columns have been built; for ", nrow(agg_adult), " countries."))
kable(cbind(head(agg_adult[,1:7]), data.frame("..." = rep("  ...", 6)))) %>%
   kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE, font_size = 12)

## ---- tidy=TRUE, tidy.opts=list(length.cutoff=10), comment="#",  null_prefix=TRUE----
description(agg_adult, path_to_write = "report.txt")

## ----results='hide', message=FALSE, warning=FALSE, echo=FALSE------------
if (file.exists("report.txt")){
  file.remove("report.txt")
}

