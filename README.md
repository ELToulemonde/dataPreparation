dataPreparation
===============
[![Github actions](https://github.com/ELToulemonde/dataPreparation/actions/workflows/r.yml/badge.svg)](https://github.com/ELToulemonde/dataPreparation/actions/workflows/r.yml) [![codecov](https://codecov.io/gh/ELToulemonde/dataPreparation/branch/master/graph/badge.svg)](https://codecov.io/gh/ELToulemonde/dataPreparation)   [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/dataPreparation)](https://cran.r-project.org/package=dataPreparation)  [![](http://cranlogs.r-pkg.org/badges/dataPreparation)](https://CRAN.R-project.org/package=dataPreparation) [![](https://cranlogs.r-pkg.org/badges/grand-total/dataPreparation)](https://CRAN.R-project.org/package=dataPreparation)
  [![HitCount](http://hits.dwyl.com/eltoulemonde/dataPreparation.svg?style=flat-square)](http://hits.dwyl.com/eltoulemonde/dataPreparation)

Data preparation accounts for about 80% of the work during a data science project. Let's take that number down.
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
  * __Transform__: creating new features from date, categorical, character... in order to have information usable for a ML algorithm (aka: numeric or categorical)
  * __Filter__: get rid of useless information in order to speed up computation
  * __Pre model transformation__: Specific manipulation for the chosen model (handling NA, discretization, one hot encoding, scaling...)
  * __Shape__: put your data set in a nice shape usable by a ML algorithm
 
Here are the functions available in this package to tackle those issues:

Correct                     | Transform                | Filter                  | Pre model manipulation| Shape             
---------                   |-----------               |--------                 |-----------            |------------------------
un_factor                    | generate_date_diffs        | fast_filter_variables     | fast_handle_na          | shape_set          
find_and_transform_dates       | generate_factor_from_date   | which_are_constant        | fast_discretization    | same_shape         
find_and_transform_numerics    | aggregate_by_key           | which_are_in_double        | fast_scale             | set_as_numeric_matrix
set_col_as_character           | generate_from_factor       | which_are_bijection       |                       | one_hot_encoder
set_col_as_numeric             | generate_from_character    |remove_sd_outlier        |                       |
set_col_as_date                | fast_round                |remove_rare_categorical  |                       |
set_col_as_factor              | target_encode            |remove_percentile_outlier|                       |

All of those functions are integrated in the __full pipeline__ function `prepare_set`.


For more details on how it work go check our [tutorial](https://cran.r-project.org/web/packages/dataPreparation/vignettes/dataPreparation.html).

Getting started: 30 seconds to dataPreparation
==============================================

### Installation
Install the package from CRAN:
```R
install.packages("dataPreparation")
```

To have the latest features, install the package from github:
```R
library(devtools)
install_github("ELToulemonde/dataPreparation")
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
clean_adult <- prepare_set(messy_adult)
head(clean_adult)
```

__That's it.__ For all functions, you can check out documentation and/or tutorial vignette.

How to Contribute
=================

dataPreparation has been developed and used by many active community members. Your help is very valuable to make it better for everyone.

- Check out call for [contributions](https://github.com/ELToulemonde/dataPreparation/blob/master/CONTRIBUTING.rst) to see what can be improved, or open an issue if you want something.
- Contribute to add new usesfull features.
- Contribute to the [tests](https://github.com/ELToulemonde/dataPreparation/tree/master/tests/testthat) to make it more reliable.
- Contribute to the documents to make it clearer for everyone.
- Contribute to the [examples](https://github.com/ELToulemonde/dataPreparation/tree/master/vignettes) to share your experience with other users.
- Open [issue](https://github.com/ELToulemonde/dataPreparation/issues/) if you met problems during development.

For more details, please refer to CONTRIBUTING.



