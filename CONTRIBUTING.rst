============
Contributing
============

Contributions are welcome, and they are greatly appreciated! Every
little bit helps, and credit will always be given.

You can contribute in many ways:

Types of Contributions
----------------------

Report Bugs
~~~~~~~~~~~

Report bugs at https://github.com/ELToulemonde/dataPreparation/issues.

If you are reporting a bug, please include:

* Your operating system name and version.
* Any details about your local setup that might be helpful in troubleshooting.
* Detailed steps to reproduce the bug.

Fix Bugs
~~~~~~~~

Look through the GitHub issues for bugs. Anything tagged with "bug"
and "help wanted" is open to whoever wants to implement it.

Implement Features
~~~~~~~~~~~~~~~~~~

Look through the GitHub issues for features. Anything tagged with "enhancement"
and "help wanted" is open to whoever wants to implement it.

If you have some new features ideas, you can also open an issue and tag it with
"enhancement".

All implementation should
- respect code conventions listed bellow,
- should be testted (using same testing schema as other tests).

Write Documentation
~~~~~~~~~~~~~~~~~~~

dataPreparation could always use more documentation, whether as part of the
official dataPreparation docs, vignettes, or even on the web in blog posts,
articles, and such.

Submit Feedback
~~~~~~~~~~~~~~~

The best way to send feedback is to file an issue at https://github.com/ELToulemonde/dataPreparation/issues.

If you are proposing a feature:

* Explain in detail how it would work.
* Keep the scope as narrow as possible, to make it easier to implement.
* Remember that this is a volunteer-driven project, and that contributions
  are welcome :)


Controlling your developments
-----------------------------

1. Build new functionality in the best R file

2. Document function. Make sure every param is commented.

3. Change package version in DESCRIPTION and add what's new in NEWS.md

3. Build unit tests in tests\testthat\test_.....R (one test file per R code file). Unit test should make sure that your function works exactly the way you want it.

4. Generate documentation using devtools::document() and control unit tests using devtools::test()

5. Build and install package using devtools::build(), devtools::install()

6. Control that every thing is conform to CRAN requirement using devtools::check(pkg = "dataPreparation"). You should have no error, no warning and no notes.

7. Control code coverage with cov <- covr::package_coverage() and then covr::zero_coverage(cov). Your new lines of codes shouldn't appear.

8. Push on github and travis will check if everything is ok. There would also be a code coverage control.
9. If everything passed: create a Pull Request.

10. Thank you very much for your contributions! :)


Code conventions
-----------------

+---------------+-------------+----------------------------------------+
|Use            | convention  | interpretation                         |
+===============+=============+========================================+
|Function names | set...      | Change into ...                        |
+               +-------------+----------------------------------------+
|               | which...    | Identify ...                           |
+               +-------------+----------------------------------------+
|               | fast...     | Perform in an efficient way            |
+               +             + (col by col or by exponential search)  +
|               |             |                                        |
+               +-------------+----------------------------------------+
|               | is.XXX      | Check if is XXX                        |
+               +-------------+----------------------------------------+
|               | generate... | Create new columns                     |
+---------------+-------------+----------------------------------------+
|Arguments      | drop        | Should original columns be dropped     |
+               +-------------+----------------------------------------+
|               | verbose     | Boolean to handle if algorithm talk    |
+               +-------------+----------------------------------------+
|               | dataSet     | Input data set                         |
+               +-------------+----------------------------------------+
|               | cols        | A list of columns names                |
+---------------+-------------+----------------------------------------+
| Variables     | data_sample | Slice of data set copied for calcul    |
+               +-------------+----------------------------------------+
|               | result      | The result that will be returned       |
+               +-------------+----------------------------------------+
|               | ..._tmp     | Partially build object                 |
+               +-------------+----------------------------------------+
|               | ...s        | Iteritable (list, array, ...)          |
+               +-------------+----------------------------------------+
|               | n...        | Number of ...                          |
+               +-------------+----------------------------------------+
|               | col         | A column name                          |
+               +-------------+----------------------------------------+
|               | I           | a list of index.                       |
+               +-------------+----------------------------------------+
|               | n_test      | Number of rows on which we perform test|
+               +-------------+----------------------------------------+
|               | args        | Agruments from "..."                   |
+               +-------------+----------------------------------------+
|               | start_time  | From proc.time() to keep track of time |
+---------------+-------------+----------------------------------------+ 