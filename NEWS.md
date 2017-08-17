v 0.2
=====
- Improving unit testing and code coverage
- Improving documentation
- Solving minor bug in date conversion and in which functions
- New features: 
	* New functions:
		O *unFactor* to unfactor columns, when reading wasn't performed in expected way.
		O *sameShape* to make ure that train and test set have exactly the same shape.
		O generate new columns from existing columns (generate functions)
			~ generate factor from dates: *generateFactorFromDate*
			~ diffDates becomes *generateDateDiffs* (for better name understanding).
			~ generate numerics and booleans from character of fators (using *generateFromFactor* and *generateFromCharacter*)
		O *setColAsFactor* a function to make multiple columns as factor and controling number of unique elements
	* New features in existing functions:
		O which functions: add *keep_cols* argument to make sure that they are not dropped
		O fastFilterVariables: *verbose* can be T/F or 0, 1, 2 in order to control level of verbosity
		O *findAndTransFormDates* and *setColAsDates* now recognize and accept timestamp.

WARNING:
- If you were using *diffDates*, it is now called *generateDateDiffs*
- *date2* column in *messy_adult* data set have changed in order to illustrate new timestamp features
- *setColAsFactorOrLogical* doesn't exist anymore: it as been splitted between *setColAsFactor* and *generateFromCat*
- Considering all those changes: *shapeSet* and *prepareSet* don't give the same result anymore.


v 0.1: release on CRAN July 2017
================================