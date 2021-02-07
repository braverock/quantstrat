# A test for walk.forward() using the macdWFA demo
#
#

options(warn=-1)
stopifnot(require(testthat))
stopifnot(require(quantstrat))
context("deom/macdWFA.R")

source(paste0(path.package("quantstrat"),"/demo/macdWFA.R"))

test_that("wfresults environment has 13 objects", {
  expect_equal(length(wfresults), 13)
})

# Cannot test End.Equity since we use a random sample of all possible parameter combinations,
# but testing for the number of objects in wfresults should suffice to identify breaking 
# walk.forward code

# Commands for running this test file from the console if required:
#
# require(testthat)
# test_file("~/quantstrat/tests/testthat/test_demo_macdWFA.R")
