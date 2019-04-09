stopifnot(require(testthat))
stopifnot(require(quantstrat))
context("osFixedPercent")

Sys.setenv(TZ = 'UTC')

symbol <- "sample_matrix"
portfolio <- "test_osFixedPercent"

defaultcurrency <- 'USD'
currency(defaultcurrency)
stock(symbol, currency = defaultcurrency, multiplier = 1)

initEq <- 1000

rm.strat(portfolio)
initPortf(portfolio, symbol)
addPosLimit(portfolio, symbol, "2007-05-30", initEq)

data(sample_matrix)
sample_matrix <- xts(sample_matrix, order.by = as.POSIXct(strptime(row.names(sample_matrix), "%Y-%m-%d")))
assign(symbol, sample_matrix)

###############################################################################

orderqty <- 1
tradesize <- 0.3
leverage <- 5

# test_that("test 1", {
#   timestamp <- as.Date("2007-05-30")
#   qty <- osFixedPercent(sample_matrix, timestamp, orderqty, portfolio, initEq, tradesize, leverage)
#   expect_equal(qty * as.numeric(Cl(sample_matrix[timestamp, ])) / leverage < initEq * tradesize, T)
# })
# 
# test_that("test 2", {
#   timestamp <- as.Date("2007-05-08")
#   qty <- osFixedPercent(sample_matrix, timestamp, orderqty, portfolio, initEq, tradesize, leverage)
#   expect_equal(qty * as.numeric(Cl(sample_matrix[timestamp, ])) / leverage < initEq * tradesize, T)
# })
