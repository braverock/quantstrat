# A test for walk.forward() using the bbandsWFA demo
#
# See issue #101 - https://github.com/braverock/quantstrat/issues/101

options(warn=-1)
stopifnot(require(testthat))
stopifnot(require(quantstrat))
context("deom/bbandsWFA.R")

source(paste0(path.package("quantstrat"),"/demo/bbandsWFA.R"))

test_that("results environment has 11 objects", {
  expect_equal(length(results), 11)
})

test_that("test TradeStats End.Equity equals -2835", {
  expect_equal(round(results$tradeStats$End.Equity), -2835)
})

# Commands for running this test file from the console if required:
#
# require(testthat)
# test_file("~/quantstrat/tests/testthat/test_demo_bbandsWFA.R")
