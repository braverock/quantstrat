# A test for apply.paramset() using the macdParameters demo
#
#

stopifnot(require(testthat))
stopifnot(require(quantstrat))
context("deom/macdParameters.R")

source(paste0(path.package("quantstrat"),"/demo/macdParameters.R"))

test_that("results environment has 33 objects", {
  expect_equal(length(results), 33)
})

# Cannot test End.Equity since we use a random sample of all possible parameter combinations,
# but testing for the number of objects in results should suffice to identify breaking 
# apply.paramset() code
#
# Since we call various overfitting functions in this demo, we could test those results
# too, in order to catch other hidden errors related to those functions, namely:
#
# 1. degrees.of.freedom()
# 2. SharpeRatio.deflated()
# 3. SharpeRatio.haircut()
#
# However, for the same reason we cannot test End.Equity, we cannot test asserts for actual
# values returned by the overfitting functions so we will limit the test to number of 
# objects returned. Since these functions are mostly complete, these tests should not need
# to be modified in the foreseeable future

test_that("df list has 12 objects", {
  expect_equal(length(df), 12)
})

test_that("defSR list has 5 objects", {
  expect_equal(length(defSR), 5)
})

test_that("hcSR list has 16 objects", {
  expect_equal(length(hcSR), 16)
})

# Commands for running this test file from the console if required:
#
# require(testthat)
# test_file("~/quantstrat/tests/testthat/test_demo_macdParameters.R")
