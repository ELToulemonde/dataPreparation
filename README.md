dataPreparation: preparing data for a data science project
==========================================================

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

  * __Read__: load the data set (this package don't treat this point: for csv we recommend `fread`)
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

Getting started: 30 seconds to dataPreparation
==============================================
