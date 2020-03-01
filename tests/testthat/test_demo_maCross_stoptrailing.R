# A test for indicators, signals and rules using the macd demo
#
#

stopifnot(require(testthat))
stopifnot(require(quantstrat))
context("demo/test_maCross_stoptrailing.R")

devAskNewPage(ask = FALSE) # dont prompt for new page, mainly for R CMD check
source(paste0(path.package("quantstrat"),"/demo/maCross_stoptrailing.R")) # source demo

test_that("End.Equity equals 4899", {
  expect_equal(round(tradeStats('Port.Luxor','AAPL')$End.Equity), 4899)
})

test_that("num txns equals 3", { # note we pad the start with zeros
  expect_equal(nrow(getTxns('Port.Luxor','AAPL')), 3)
})

test_that("num orders equals 2", {
  expect_equal(nrow(obook$Port.Luxor$AAPL), 11)
})

# Commands for running this test file from the console if required:
#
# require(testthat)
# test_file("~/quantstrat/tests/testthat/test_demo_maCross_stoptrailing.R")
