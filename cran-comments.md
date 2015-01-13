This is a resubmission. Compared to the previous submission, I have:

* replaced `as.person()` with `person()` in Authors@R

---

## Test environments
* local OS X install, R 3.1.2
* ubuntu 12.04 (on travis-ci), R 3.1.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 

There were 2 NOTEs:

* Possibly mis-spelled words in DESCRIPTION:
  API (2:42) BigQuery (2:33, 3:38)
  
  These are correctly spelled.

* checking dependencies in R code ... NOTE
  Namespace in Imports field not imported from: 'R6'

  R6 is a build-time dependency.

## Downstream dependencies
This is a new submission.
