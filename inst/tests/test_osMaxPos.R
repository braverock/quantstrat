stopifnot(require(testthat))
stopifnot(require(quantstrat))
context("osMaxPos")

# setup
symbol <- "test"
portfolio <- "test_osMaxPos"
initPortf(portfolio, symbol, -3000)

addPosLimit(portfolio, symbol, "2013-09-01", 8000, 8, -4000, 4)
timestamp <- as.Date("2013-09-09")

###############################################################################
# short position, ruletype == "risk"
test_that("risk-flatten short w/orderqty = 'all'", {
  qty <- osMaxPos(NULL, timestamp, "all", "", "short", portfolio, symbol, "risk")
  expect_equal(qty, 3000)
  qty <- osMaxPos(NULL, timestamp, "all", "", NULL, portfolio, symbol, "risk")
  expect_equal(qty, 3000)
})

# FIXME: Should risk be able to take position across zero?
test_that("risk-flatten short w/orderqty > abs(position)", {
  qty <- osMaxPos(NULL, timestamp, 4000, "", "short", portfolio, symbol, "risk")
  expect_equal(qty, 4000)
  qty <- osMaxPos(NULL, timestamp, 4000, "", NULL, portfolio, symbol, "risk")
  expect_equal(qty, 4000)
})

test_that("risk-reduce short", {
  qty <- osMaxPos(NULL, timestamp, 2000, "", "short", portfolio, symbol, "risk")
  expect_equal(qty, 2000)
  qty <- osMaxPos(NULL, timestamp, 2000, "", NULL, portfolio, symbol, "risk")
  expect_equal(qty, 2000)
})

# FIXME: What should these do? Risk rules probably shouldn't be able to increase position
#test_that("risk-increase short within limits", {
#  qty <- osMaxPos(NULL, timestamp, -1000, "", "short", portfolio, symbol, "risk")
#  qty <- osMaxPos(NULL, timestamp, -1000, "", NULL, portfolio, symbol, "risk")
#})
#
#test_that("risk-increase short beyond limits", {
#  qty <- osMaxPos(NULL, timestamp, -2000, "", "short", portfolio, symbol, "risk")
#  qty <- osMaxPos(NULL, timestamp, -2000, "", NULL, portfolio, symbol, "risk")
#})

###############################################################################
# short position, ruletype != "risk"
test_that("flatten short w/orderqty = 'all'", {
  # ordertype="all" is not currently supported for non-risk ruletypes
  expect_error(osMaxPos(NULL, timestamp, "all", "", "short", portfolio, symbol, ""))
})

test_that("flatten short w/orderqty > abs(position)", {
  qty <- osMaxPos(NULL, timestamp, 4000, "", "short", portfolio, symbol, "")
  expect_equal(qty, 3000)
  qty <- osMaxPos(NULL, timestamp, 4000, "", NULL, portfolio, symbol, "")
  expect_equal(qty, 3000)
})

test_that("reduce short", {
  qty <- osMaxPos(NULL, timestamp, 2000, "", "short", portfolio, symbol, "")
  expect_equal(qty, 2000)
  qty <- osMaxPos(NULL, timestamp, 2000, "", NULL, portfolio, symbol, "")
  expect_equal(qty, 2000)
})

test_that("increase short within limits", {
  qty <- osMaxPos(NULL, timestamp, -1000, "", "short", portfolio, symbol, "risk")
  expect_equal(qty, -1000)
  qty <- osMaxPos(NULL, timestamp, -1000, "", NULL, portfolio, symbol, "risk")
  expect_equal(qty, -1000)
})

test_that("increase short beyond limits", {
  qty <- osMaxPos(NULL, timestamp, -2000, "", "short", portfolio, symbol, "risk")
  expect_equal(qty, -1000)
  qty <- osMaxPos(NULL, timestamp, -2000, "", NULL, portfolio, symbol, "risk")
  expect_equal(qty, -1000)
})

# add transaction to make portfolio long
suppressWarnings(addTxn(portfolio, symbol, timestamp-1L, 10000, 1, verbose=FALSE))

###############################################################################
# long position, ruletype == "risk"
test_that("risk-flatten long w/orderqty = 'all'", {
  qty <- osMaxPos(NULL, timestamp, "all", "", "long", portfolio, symbol, "risk")
  expect_equal(qty, -7000)
  qty <- osMaxPos(NULL, timestamp, "all", "", NULL, portfolio, symbol, "risk")
  expect_equal(qty, -7000)
})

test_that("risk-reduce long", {
  qty <- osMaxPos(NULL, timestamp, -2000, "", "long", portfolio, symbol, "risk")
  expect_equal(qty, -2000)
  qty <- osMaxPos(NULL, timestamp, -2000, "", NULL, portfolio, symbol, "risk")
  expect_equal(qty, -2000)
})

# FIXME: Should risk be able to take position across zero?
test_that("risk-flatten long w/orderqty > abs(position)", {
  qty <- osMaxPos(NULL, timestamp, -8000, "", "long", portfolio, symbol, "risk")
  expect_equal(qty, -8000)
  qty <- osMaxPos(NULL, timestamp, -8000, "", NULL, portfolio, symbol, "risk")
  expect_equal(qty, -8000)
})

# FIXME: What should these do? Risk rules probably shouldn't be able to increase position
#test_that("risk-increase long within limits", {
#  qty <- osMaxPos(NULL, timestamp, 1000, "", "long", portfolio, symbol, "risk")
#  qty <- osMaxPos(NULL, timestamp, 1000, "", NULL, portfolio, symbol, "risk")
#})
#
#test_that("risk-increase long beyond limits", {
#  qty <- osMaxPos(NULL, timestamp, 2000, "", "long", portfolio, symbol, "risk")
#  qty <- osMaxPos(NULL, timestamp, 2000, "", NULL, portfolio, symbol, "risk")
#})

###############################################################################
# long position, ruletype != "risk"
test_that("flatten long w/orderqty = 'all'", {
  # ordertype="all" is not currently supported for non-risk ruletypes
  expect_error(osMaxPos(NULL, timestamp, "all", "", "short", portfolio, symbol, ""))
})

test_that("reduce long", {
  qty <- osMaxPos(NULL, timestamp, -2000, "", "long", portfolio, symbol, "")
  expect_equal(qty, -2000)
  qty <- osMaxPos(NULL, timestamp, -2000, "", NULL, portfolio, symbol, "")
  expect_equal(qty, -2000)
})

test_that("flatten long w/orderqty > abs(position)", {
  qty <- osMaxPos(NULL, timestamp, -8000, "", "long", portfolio, symbol, "")
  expect_equal(qty, -7000)
  qty <- osMaxPos(NULL, timestamp, -8000, "", NULL, portfolio, symbol, "")
  expect_equal(qty, -7000)
})

test_that("increase long within limits", {
  qty <- osMaxPos(NULL, timestamp, 1000, "", "long", portfolio, symbol, "risk")
  expect_equal(qty, 1000)
  qty <- osMaxPos(NULL, timestamp, 1000, "", NULL, portfolio, symbol, "risk")
  expect_equal(qty, 1000)
})

test_that("increase long beyond limits", {
  qty <- osMaxPos(NULL, timestamp, 2000, "", "long", portfolio, symbol, "risk")
  expect_equal(qty, 1000)
  qty <- osMaxPos(NULL, timestamp, 2000, "", NULL, portfolio, symbol, "risk")
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

