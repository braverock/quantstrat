# test_blotter_break.R
# test for breaking trade with another trade that day in blotter
#   Author: Brian M Bielinski

Sys.setenv(TZ = "GMT")
require(blotter)
require(testthat)

port = "testBreak"
acct = "testBreak"
symbol = c("IBM")

data(IBM)

startDate = first(index(IBM))
endDate   = last(index(IBM))

lines = "date,shrs,price,symbol
2007-01-10,100,98.0,IBM
2007-01-16,-200,99.5,IBM
2007-01-16,150,99.45,IBM
2007-01-18,-50,99.0,IBM"
con       = textConnection(lines)
tt.trades = read.csv(con, as.is = TRUE)

tt.trades[,"date"] = make.time.unique(as.POSIXct(tt.trades[,"date"]))

currency("USD")
stock(symbol,"USD")

initPortf(port,
          symbol,
          initDate = startDate)
initAcct(port,
         portfolios = c(port),
         initDate   = startDate,
         initEq=10^6)

for(i in 1:nrow(tt.trades)){
   addTxn(port,Symbol = tt.trades[i,"symbol"],
          TxnDate     = tt.trades[i,"date"],
          TxnPrice    = tt.trades[i,"price"],
          TxnQty      = tt.trades[i,"shrs"])
}

updatePortf(port)
updateAcct(acct)
updateEndEq(acct)

context("check that breaking trade with another trade that day is accounted for correctly.")

# should be flat
test_that("input trades position is flat",
          expect_that(sum(tt.trades[,"shrs"]) == 0, is_true()))
test_that("recorded trades position in Txns is flat",
          expect_that(sum(getTxns(port,"IBM")[,"Txn.Qty"]) == 0, is_true()))
test_that("times of the trades are unique",
          expect_that(length(tt.trades[,"date"]) == length(unique(tt.trades[,"date"])),is_true()))
test_that("calculated position is flat",
          expect_that(getPosQty(port,"IBM",Date = endDate) == 0, is_true()))

#chart.Posn(port,"IBM")
