dataPreparation
===============

__dataPreparation__ will allow you to do most of the painful data preparation for a data science project with a minimum amount of code.


This package is
- fast (use `data.table` and exponential search)
- RAM efficient (perform operations by reference and column-wise to avoid copying data)
- stable (most exceptions are handled)
- verbose (log a lot)



--------------------------

Main preparation steps
=======================

Before using any machine learning (ML) algorithm, one need to prepare its data. Preparing a data set for a data science project can be long and tricky. The main steps are the followings:

  * __Read__: load the data set (this package don't treat this point: for csv we recommend `data.table::fread`)
  * __Correct__: most of the times, there are some mistake after reading, wrong format... one have to correct them
  * __Transform__: aggregating according to a key, computing differences between dates, ... in order to have information usable for a ML algorithm (aka: numeric or categorical)
  * __Filter__: get read of useless information in order to speed up computation
  * __Handle NA__: replace missing values
  * __Shape__: put your data set in a nice shape usable by a ML algorithm
  
Here are the functions available in this package to tackle those issues:

Correct                     | Transform                | Filter              | Handle NA    | Shape
---------                   |-----------               |--------             |-----------   |------
findAndTransformDates       | diffDates                | fastFilterVariables | fastHandleNa | shapeSet
findAndTransformNumerics    | aggregateByKey           | whichAreConstant    |              | setAsNumericMatrix
setColAsCharacter           | setColAsFactorOrLogical  | whichAreInDouble    |              |
setColAsNumeric             |                          | whichAreBijection   |              |
setColAsDate                |                          | fastRound           |              |


All of those functions are integrated in the __full pipeline__ function `prepareSet`.


For more details on how it work go check [our tutorial](https://github.com/ELToulemonde/dataPreparation/blob/master/inst/doc/dataPreparation.html)

Getting started: 30 seconds to dataPreparation
==============================================

### Installation
Install the package from CRAN:
```R
install.package("dataPreparation")
```

Install the package from github:
```R
library(devtools)
install_github("ELToulemonde/dataPreparation", build_vignettes=FALSE)
```

### Test it
Load a toy data set
```R
library(dataPreparation)
data(messy_adult)
head(messy_adult)
```

Perform full pipeline function
```R
clean_adult <- prepareSet(messy_adult)
head(clean_adult)
```

__That's it.__ For all functions, you can check out documentation and/or tutorial vignette. 

How to Contribute
=================

dataPreparation has been developed and used by many active community members. Your help is very valuable to make it better for everyone.

- Check out call for [contributions](https://github.com/ELToulemonde/dataPreparation/blob/master/CONTRIBUTING.md) to see what can be improved, or open an issue if you want something.
- Contribute to add new usesfull features.
- Contribute to the [tests](https://github.com/ELToulemonde/dataPreparation/tree/master/tests/testthat) to make it more reliable.
- Contribute to the documents to make it clearer for everyone.
- Contribute to the [examples](https://github.com/ELToulemonde/dataPreparation/tree/master/vignettes) to share your experience with other users.
- Open [issue](https://github.com/ELToulemonde/dataPreparation/issues/) if you met problems during development.

For more details, please refer to CONTRIBUTING.



