v 0.2
=====
- Improving unit testing and code coverage
- Improving documentation
- Solving minor bug in date conversion and in which functions
- New features: 
	* unFactor to unfactor columns, when reading wasn't performed in expected way.
	* sameShape to make ure that train and test set have exactly the same shape.
	* generate new columns from existing columns (generate functions)
		O generate factor from dates: generateFactorFromDate
		O diffDates becomes generateDateDiffs (from better name understanding).

WARNING:
If you where using diffDates, it is now called generateDateDiffs

v 0.1: release on CRAN July 2017
================================