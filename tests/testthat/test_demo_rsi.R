# A test for indicators, signals and rules using the rsi demo
#
#

stopifnot(require(testthat))
stopifnot(require(quantstrat))
context("demo/rsi.R")

devAskNewPage(ask = FALSE) # dont prompt for new page, mainly for R CMD check
source(paste0(path.package("quantstrat"),"/demo/rsi.R")) # source demo

test_that("rsi End.Equity for 'XLB' equals 8329", {
  expect_equal(round(tradeStats('RSI','XLB')$End.Equity), 8329)
})

test_that("rsi num txns for 'XLB' equals 2356", {
  expect_equal(nrow(getTxns('RSI','XLB')), 2356)
})

test_that("rsi End.Equity for 'XLE' equals 26003", {
  expect_equal(round(tradeStats('RSI','XLE')$End.Equity), 26003)
})

test_that("rsi num txns for 'XLE' equals 2376", {
  expect_equal(nrow(getTxns('RSI','XLE')), 2376)
})

test_that("rsi End.Equity for 'XLF' equals 11397", {
  expect_equal(round(tradeStats('RSI','XLF')$End.Equity), 11397)
})

test_that("rsi num txns for 'XLF' equals 2414", {
  expect_equal(nrow(getTxns('RSI','XLF')), 2414)
})

test_that("rsi End.Equity for 'XLI' equals 2260", {
  expect_equal(round(tradeStats('RSI','XLI')$End.Equity), 2260)
})

test_that("rsi num txns for 'XLI' equals 2345", {
  expect_equal(nrow(getTxns('RSI','XLI')), 2345)
})

test_that("rsi End.Equity for 'XLK' equals 12316", {
  expect_equal(round(tradeStats('RSI','XLK')$End.Equity), 12316)
})

test_that("rsi num txns for 'XLK' equals 2372", {
  expect_equal(nrow(getTxns('RSI','XLK')), 2372)
})

test_that("rsi End.Equity for 'XLP' equals 6491", {
  expect_equal(round(tradeStats('RSI','XLP')$End.Equity), 6491)
})

test_that("rsi num txns for 'XLP' equals 2439", {
  expect_equal(nrow(getTxns('RSI','XLP')), 2439)
})

test_that("rsi End.Equity for 'XLU' equals 12432", {
  expect_equal(round(tradeStats('RSI','XLU')$End.Equity), 12432)
})

test_that("rsi num txns for 'XLU' equals 2417", {
  expect_equal(nrow(getTxns('RSI','XLU')), 2417)
})

test_that("rsi End.Equity for 'XLV' equals 11352", {
  expect_equal(round(tradeStats('RSI','XLV')$End.Equity), 11352)
})

test_that("rsi num txns for 'XLV' equals 2404", {
  expect_equal(nrow(getTxns('RSI','XLV')), 2404)
})

test_that("rsi End.Equity for 'XLY' equals 14080", {
  expect_equal(round(tradeStats('RSI','XLY')$End.Equity), 14080)
})

test_that("rsi num txns for 'XLY' equals 2340", {
  expect_equal(nrow(getTxns('RSI','XLY')), 2340)
})

# Commands for running this test file from the console if required:
#
# require(testthat)
# test_file("~/quantstrat/tests/testthat/test_demo_rsi.R")
