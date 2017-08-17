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

Write Documentation
~~~~~~~~~~~~~~~~~~~

MLBox could always use more documentation, whether as part of the
official MLBox docs, in docstrings, or even on the web in blog posts,
articles, and such.

Submit Feedback
~~~~~~~~~~~~~~~

The best way to send feedback is to file an issue at https://github.com/ELToulemonde/dataPreparation/issues.

If you are proposing a feature:

* Explain in detail how it would work.
* Keep the scope as narrow as possible, to make it easier to implement.
* Remember that this is a volunteer-driven project, and that contributions
  are welcome :)

  
  
  
  
Code conventions
-----------------

## Function names
set...      # Change into ...
which...    # Identify ...
fast...     # Perform in an efficient way (col by col or by exponential search on lines)
is.XXX      # Check if is XXX
generate... # Create new columns

## Variables
dataSet     # Input data set 
data_sample # A piece of data set copied for work purpuse
result      # The result that will be returned
..._tmp     # Partially build object
...s        # Iteritable (list, array, ...)
nb_...      # Number of ...

cols        # A list of columns names
col         # A column name
I           # a list of index.
verbose     # Boolean to handle if algorithm talk
n_test      # Number of rows on which we perform test
drop        # for generate functions, should original columns be dropped

args        # Agruments from "..."

start_time  # From proc.time() to keep track of time
end_time    # From proc.time() to keep track of time