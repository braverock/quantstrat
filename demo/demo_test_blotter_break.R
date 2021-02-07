# demo script for testing purposes

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
2007-01-17,150,99.45,IBM
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
