stopifnot(require(testthat))
stopifnot(require(quantstrat))
context("osMaxPos")

# setup
symbol <- "test"
portfolio <- "test_osMaxPos"
longPos  <-  6000
shortPos <- -2000
initPortf(portfolio, symbol, shortPos)

addPosLimit(portfolio, symbol, "2013-09-01", 8000, 8, -4000, 4)
timestamp <- as.Date("2013-09-09")

###############################################################################
# short position, ruletype == "risk"
side <- "short"
type <- "risk"

test_that("risk-flatten short w/orderqty = 'all'", {
  qty <- osMaxPos(NULL, timestamp, "all", "", side, portfolio, symbol, type)
  expect_equal(qty, -shortPos)
  qty <- osMaxPos(NULL, timestamp, "all", "", NULL, portfolio, symbol, type)
  expect_equal(qty, -shortPos)
})

# FIXME: Should risk be able to take position across zero?
test_that("risk-flatten short w/orderqty > abs(position)", {
  qty <- osMaxPos(NULL, timestamp, -shortPos*2, "", side, portfolio, symbol, type)
  expect_equal(qty, -shortPos*2)
  qty <- osMaxPos(NULL, timestamp, -shortPos*2, "", NULL, portfolio, symbol, type)
  expect_equal(qty, -shortPos*2)
})

test_that("risk-reduce short", {
  qty <- osMaxPos(NULL, timestamp, 2000, "", side, portfolio, symbol, type)
  expect_equal(qty, 2000)
  qty <- osMaxPos(NULL, timestamp, 2000, "", NULL, portfolio, symbol, type)
  expect_equal(qty, 2000)
})

# FIXME: What should these do? Risk rules probably shouldn't be able to increase position
#test_that("risk-increase short within limits", {
#  qty <- osMaxPos(NULL, timestamp, -1000, "", side, portfolio, symbol, type)
#  qty <- osMaxPos(NULL, timestamp, -1000, "", NULL, portfolio, symbol, type)
#})
#
#test_that("risk-increase short beyond limits", {
#  qty <- osMaxPos(NULL, timestamp, -2000, "", side, portfolio, symbol, type)
#  qty <- osMaxPos(NULL, timestamp, -2000, "", NULL, portfolio, symbol, type)
#})

###############################################################################
# short position, ruletype != "risk"
type <- ""

test_that("flatten short w/orderqty = 'all'", {
  # ordertype="all" is not currently supported for non-risk ruletypes
  expect_error(osMaxPos(NULL, timestamp, "all", "", side, portfolio, symbol, type))
})

test_that("flatten short w/orderqty > abs(position)", {
  qty <- osMaxPos(NULL, timestamp, -shortPos*2, "", side, portfolio, symbol, type)
  expect_equal(qty, -shortPos)
  qty <- osMaxPos(NULL, timestamp, -shortPos*2, "", NULL, portfolio, symbol, type)
  expect_equal(qty, -shortPos)
})

test_that("reduce short", {
  qty <- osMaxPos(NULL, timestamp, 2000, "", side, portfolio, symbol, type)
  expect_equal(qty, 2000)
  qty <- osMaxPos(NULL, timestamp, 2000, "", NULL, portfolio, symbol, type)
  expect_equal(qty, 2000)
})

test_that("increase short within limits, < clip size", {
  qty <- osMaxPos(NULL, timestamp, -1000, "", side, portfolio, symbol, type)
  expect_equal(qty, -1000)
  qty <- osMaxPos(NULL, timestamp, -1000, "", NULL, portfolio, symbol, type)
  expect_equal(qty, -1000)
})

test_that("increase short within limits, > clip size", {
  qty <- osMaxPos(NULL, timestamp, -1500, "", side, portfolio, symbol, type)
  expect_equal(qty, -1000)
  qty <- osMaxPos(NULL, timestamp, -1500, "", NULL, portfolio, symbol, type)
  expect_equal(qty, -1000)
})

test_that("increase short beyond limits", {
  qty <- osMaxPos(NULL, timestamp, -2000, "", side, portfolio, symbol, type)
  expect_equal(qty, -1000)
  qty <- osMaxPos(NULL, timestamp, -2000, "", NULL, portfolio, symbol, type)
  expect_equal(qty, -1000)
})

# add transactions to make portfolio long
suppressWarnings({
  addTxn(portfolio, symbol, timestamp-2L, -shortPos, 1, verbose=FALSE)
  addTxn(portfolio, symbol, timestamp-1L, longPos, 1, verbose=FALSE)
})

###############################################################################
# long position, ruletype == "risk"
side <- "long"
type <- "risk"

test_that("risk-flatten long w/orderqty = 'all'", {
  qty <- osMaxPos(NULL, timestamp, "all", "", side, portfolio, symbol, type)
  expect_equal(qty, -longPos)
  qty <- osMaxPos(NULL, timestamp, "all", "", NULL, portfolio, symbol, type)
  expect_equal(qty, -longPos)
})

test_that("risk-reduce long", {
  qty <- osMaxPos(NULL, timestamp, -2000, "", side, portfolio, symbol, type)
  expect_equal(qty, -2000)
  qty <- osMaxPos(NULL, timestamp, -2000, "", NULL, portfolio, symbol, type)
  expect_equal(qty, -2000)
})

# FIXME: Should risk be able to take position across zero?
test_that("risk-flatten long w/orderqty > abs(position)", {
  qty <- osMaxPos(NULL, timestamp, -longPos*2, "", side, portfolio, symbol, type)
  expect_equal(qty, -longPos*2)
  qty <- osMaxPos(NULL, timestamp, -longPos*2, "", NULL, portfolio, symbol, type)
  expect_equal(qty, -longPos*2)
})

# FIXME: What should these do? Risk rules probably shouldn't be able to increase position
#test_that("risk-increase long within limits", {
#  qty <- osMaxPos(NULL, timestamp, 1000, "", side, portfolio, symbol, type)
#  qty <- osMaxPos(NULL, timestamp, 1000, "", NULL, portfolio, symbol, type)
#})
#
#test_that("risk-increase long beyond limits", {
#  qty <- osMaxPos(NULL, timestamp, 2000, "", side, portfolio, symbol, type)
#  qty <- osMaxPos(NULL, timestamp, 2000, "", NULL, portfolio, symbol, type)
#})

###############################################################################
# long position, ruletype != "risk"
type <- ""

test_that("flatten long w/orderqty = 'all'", {
  # ordertype="all" is not currently supported for non-risk ruletypes
  expect_error(osMaxPos(NULL, timestamp, "all", "", side, portfolio, symbol, type))
})

test_that("reduce long", {
  qty <- osMaxPos(NULL, timestamp, -2000, "", side, portfolio, symbol, type)
  expect_equal(qty, -2000)
  qty <- osMaxPos(NULL, timestamp, -2000, "", NULL, portfolio, symbol, type)
  expect_equal(qty, -2000)
})

test_that("flatten long w/orderqty > abs(position)", {
  qty <- osMaxPos(NULL, timestamp, -longPos*2, "", side, portfolio, symbol, type)
  expect_equal(qty, -longPos)
  qty <- osMaxPos(NULL, timestamp, -longPos*2, "", NULL, portfolio, symbol, type)
  expect_equal(qty, -longPos)
})

test_that("increase long within limits, < clip size", {
  qty <- osMaxPos(NULL, timestamp, 1000, "", side, portfolio, symbol, type)
  expect_equal(qty, 1000)
  qty <- osMaxPos(NULL, timestamp, 1000, "", NULL, portfolio, symbol, type)
  expect_equal(qty, 1000)
})

test_that("increase long within limits, > clip size", {
  qty <- osMaxPos(NULL, timestamp, 1500, "", side, portfolio, symbol, type)
  expect_equal(qty, 1000)
  qty <- osMaxPos(NULL, timestamp, 1500, "", NULL, portfolio, symbol, type)
  expect_equal(qty, 1000)
})

test_that("increase long beyond limits", {
  qty <- osMaxPos(NULL, timestamp, 2000, "", side, portfolio, symbol, type)
  expect_equal(qty, 1000)
  qty <- osMaxPos(NULL, timestamp, 2000, "", NULL, portfolio, symbol, type)
  expect_equal(qty, 1000)
})

###############################################################################
test_that("zero order quantity", {
  qty <- osMaxPos(NULL, timestamp, 0, "", "long", portfolio, symbol, "")
  expect_equal(qty, 0)
  qty <- osMaxPos(NULL, timestamp, 0, "", NULL, portfolio, symbol, "")
  expect_equal(qty, 0)
  qty <- osMaxPos(NULL, timestamp, 0, "", "short", portfolio, symbol, "")
  expect_equal(qty, 0)
  qty <- osMaxPos(NULL, timestamp, 0, "", NULL, portfolio, symbol, "")
  expect_equal(qty, 0)
})

