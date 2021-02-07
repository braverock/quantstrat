# A test for indicators, signals and rules using the macd demo
#
#

stopifnot(require(testthat))
stopifnot(require(quantstrat))
context("demo/test_maCross_stoptrailing.R")

devAskNewPage(ask = FALSE) # dont prompt for new page, mainly for R CMD check
source(paste0(path.package("quantstrat"),"/demo/maCross_stoptrailing.R")) # source demo

test_that("End.Equity equals 7809", {
  expect_equal(round(tradeStats('Port.Luxor','AAPL')$End.Equity), 2771)
})

test_that("num txns equals 11", { # note we pad the start with zeros
  expect_equal(nrow(getTxns('Port.Luxor','AAPL')), 11)
})

test_that("num orders equals 19", {
  expect_equal(nrow(obook$Port.Luxor$AAPL), 19)
})

test_that("sum closed order prices equals sum txn prices", {
  expect_equal(sum(as.numeric(getOrderBook("Port.Luxor")$Port.Luxor$AAPL$Order.Price[which(getOrderBook("Port.Luxor")$Port.Luxor$AAPL$Order.Status == "closed")])),
               sum(getTxns("Port.Luxor","AAPL")$Txn.Price))
})

test_that("stoptrailing order prices equal 138.7663, 141.5517, 141.6479, 150.7013, 144.4037, 147.0306, 147.0788, 148.2753, 149.5006, 151.4304, 151.2416, 163.0308, 164.8702, 168.1434", {
  expect_equal(round(as.numeric(getOrderBook("Port.Luxor")$Port.Luxor$AAPL$Order.Price[which(getOrderBook("Port.Luxor")$Port.Luxor$AAPL$Order.Type == "stoptrailing")]$Order.Price), 4),
               
               c(138.7663, 141.5517, 141.6479, 150.7013, 144.4037, 147.0306, 147.0788, 148.2753, 149.5006, 151.4304, 151.2416, 163.0308, 164.8702, 168.1434))
})
       
# Commands for running this test file from the console if required:
#
# require(testthat)
# test_file("~/quantstrat/tests/testthat/test_demo_maCross_stoptrailing.R")
