# A test for indicators, signals and rules using the macd demo
#
#

stopifnot(require(testthat))
stopifnot(require(quantstrat))
context("demo/macd.R")

devAskNewPage(ask = FALSE) # dont prompt for new page, mainly for R CMD check
source(paste0(path.package("quantstrat"),"/demo/macd.R")) # source demo

test_that("macd End.Equity equals 7202", {
  expect_equal(round(tradeStats('macd','AAPL')$End.Equity), 7202)
})

test_that("macd num txns equals 38", { # one more than orders since we pad the start with zeros on 1950-01-01
  expect_equal(nrow(getTxns('macd','AAPL')), 38)
})

test_that("macd num orders equals 37", {
  expect_equal(nrow(obook$macd$AAPL), 37)
})

# Commands for running this test file from the console if required:
#
# require(testthat)
# test_file("~/quantstrat/tests/testthat/test_demo_macd.R")
