V 0.3.4
========
- Improvement of function whichAreBijection. It is 2 to 15 time faster than previous version.
- Bug fixes:
	- *generateFactorFromDate*: default value was missing. Fixed.
- New features:
	- New features in existing functions:
		- *fastFilterVariables* has a new parameter (level) to choose which types of filtering to perform
V 0.3.3
========
- New features:
	- New features in existing functions:
		- *findAndTransFormDates* now recognize date character even if there are multiple separator in date (ex: "2016, Jan-26").
		- *findAndTransFormDates* now recognize date character even if there are leading and tailing white spaces.
		
WARNING:
- *date3* column in *messy_adult* data set has changed in order to illustrate the recognition of date character even if there are leading and/or trailing white spaces.
- *date4* column in *messy_adult* data set has changed in order to illustrate the recognition of date character even if there are multiple separator.

V 0.3.2
========
- Change URLs to meet CRAN requirement

v 0.3.1
=======
- Fix bug in Latex documentation

v 0.3
=====
- New features:
	- New features in existing functions:
		- *findAndTransFormDates* now recognize date character even if "0" are not present in month or day part and month as lower strings.
		- *findAndTransFormDates* and *setColAsDate* now work with *factors*.
	- New functions:
		- *fastDiscretization*: to perform equal freq or equal width discretization on a data set using *data.table* power.
		- *fastScale*: to perform scaling on a data set using *data.table* power.
		- *one_hot_encoder*: to perform one_hot encoding on a data set using *data.table* power.
	- New documentation:
		- A new vignette to illustrate how to build a correct *train* and *test* set unising data preparation
- Minor changes in log (in particular regarding progress bars and typos)
	- Due to dependencies issues with *tcltk*, we stop using it and start using *progress*
- Refactoring: 
	- Private function *real_cols* take more importance to control that columns have the correct types and handling "auto" value.
	- Making code faster: some functions are up to **30% faster**
	- Review unit testing to be faster
	- Unit test evolution to be more readable

WARNING:
- *date1* column in *messy_adult* data set has changed in order to illustrate the recognition of date character even if "0" are not present in month or day part.


v 0.2
=====
- Improving unit testing and code coverage
- Improving documentation
- Solving minor bug in date conversion and in which functions
- New features: 
	- New functions:
		- *unFactor* to unfactor columns, when reading wasn't performed in expected way.
		- *sameShape* to make ure that train and test set have exactly the same shape.
		- generate new columns from existing columns (generate functions)
			- generate factor from dates: *generateFactorFromDate*
			- diffDates becomes *generateDateDiffs* (for better name understanding).
			- generate numerics and booleans from character of fators (using *generateFromFactor* and *generateFromCharacter*)
			
		- *setColAsFactor* a function to make multiple columns as factor and controling number of unique elements
		
	- New features in existing functions:	
		- which functions: add *keep_cols* argument to make sure that they are not dropped
		- fastFilterVariables: *verbose* can be T/F or 0, 1, 2 in order to control level of verbosity
		- *findAndTransFormDates* and *setColAsDates* now recognize and accept timestamp.

WARNING:
- If you were using *diffDates*, it is now called *generateDateDiffs*
- *date2* column in *messy_adult* data set have changed in order to illustrate new timestamp features
- *setColAsFactorOrLogical* doesn't exist anymore: it as been splitted between *setColAsFactor* and *generateFromCat*
- Considering all those changes: *shapeSet* and *prepareSet* don't give the same result anymore.


v 0.1: release on CRAN July 2017
================================