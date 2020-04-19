# Marking

To mark your answers, first run the following in a terminal **on a lab machine** to prepare:

```
$ module load func-prog
$ cd fp-learning-2019-2020/Assignments/Formative/Formative1
$ git pull
```

Also make sure the directory `fp-learning-2019-2020/Assignments/Formative/Formative1` contains a copy of your solution file named `Formative1.hs`.

You can now mark using
```
$ runhaskell --ghc-arg=-package-env --ghc-arg=marking-ghc-env Formative1Marking
```

This script will test your answers and give feedback on whether they are correct.

When a test fails, the script gives you the inputs that were used to cause the failure.

To get definitions for the grids used in testing, take a look at `Formative1TestCases.hs`. This contains a list of grid definitions along with the name that is displayed in the test output and possibly a link to more information on this grid.
