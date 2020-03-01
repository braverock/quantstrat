# # A test for indicators, signals and rules using the bbands demo
# #
# #
# 
# stopifnot(require(testthat))
# stopifnot(require(quantstrat))
# context("demo/bbands.R")
# 
# devAskNewPage(ask = FALSE) # dont prompt for new page, mainly for R CMD check
# source(paste0(path.package("quantstrat"),"/demo/bbands.R")) # source demo
# 
# test_that("bbands End.Equity equals 9378", {
#   expect_equal(round(tradeStats('bbands','IBM')$End.Equity), 9378)
# })
# 
# test_that("bbands num txns equals 340", {
#   expect_equal(nrow(getTxns('bbands','IBM')), 340)
# })
# 
# test_that("macd num orders equals 340", {
#   expect_equal(nrow(as.data.frame(getOrderBook('bbands')[[1]])), 340)
# })
# 
# # Commands for running this test file from the console if required:
# #
# # require(testthat)
# # test_file("~/quantstrat/tests/testthat/test_demo_bbands.R")
