################## LOAD TESTING FRAMEWORK ######################

require(testthat)
require(PortfolioAnalytics)

######### ACTIVATE TEST DATES AND SOURCE BEE #####################

options(in_test=TRUE)
source(system.file("demo/bee.R", package="quantstrat"))

################### DEFINE VARIABLES TO BE TESTED ##############

qty    = book$bug$GLD[,"Order.Qty"]
price  = book$bug$GLD[,"Order.Price"]
type   = book$bug$GLD[,"Order.Type"]
side   = book$bug$GLD[,"Order.Side"]
status = book$bug$GLD[,"Order.Status"]
fees   = book$bug$GLD[,"Txn.Fees"]
rule   = book$bug$GLD[,"Rule"]

Tqty   = txns$Txn.Qty
Tprice = txns$Txn.Price
Tfees  = txns$Txn.Fees
Tvalue = txns$Txn.Value
Tcost  = txns$Txn.Cost
TPL    = txns$Txn.PL

###################### BEE ORDER BOOK ############################

context("Bee order book is consistent ")

## quantity
test_that("The long position is liquidated in mid-Oct 2012", 
          { expect_that(as.character(qty[28]) =="all", is_true()) })
test_that("A short position is intiated in mid Oct-2012", 
          { expect_that(as.character(qty[29]) =="-100", is_true()) })
## price
test_that("The long position is exited in mid-Oct 2012 at 168.42", 
          { expect_that(as.character(price[28]) =="168.42", is_true()) })
test_that("The last short position is intiated at 165.45", 
          { expect_that(as.character(price[33]) =="165.45", is_true()) })
## type
test_that("The first trade is a market order", 
          { expect_that(as.character(type[2]) =="market", is_true()) })
## side
test_that("The position liquidated in mid-Oct 2012 is the long one", 
          { expect_that(as.character(side[28]) =="long", is_true()) })
## status
test_that("The long exit rejected when position is short", 
          { expect_that(as.character(status[21]) =="rejected", is_true()) })
## fees
test_that("The first trade has no transaction fees", 
          { expect_that(as.character(fees[2]) =="0", is_true()) })
## rule
test_that("The run in Mar 2012 is entered short", 
          { expect_that(as.character(rule[20]) =="EnterSHORT", is_true()) })
test_that("The run in late-July 2012 is entered long", 
          { expect_that(as.character(rule[27]) =="EnterLONG", is_true()) })
test_that("The run in late-July 2012 is exited short", 
          { expect_that(as.character(rule[26]) =="ExitSHORT", is_true()) })

######################## BEE TRADE STATS  ###############################

context("Bee trade statistics are consistent ")

test_that("Num.Txns is 27", 
          { expect_that(stats$Num.Txns , equals(27)) })
test_that("Num.Trades is 13", 
          { expect_that(stats$Num.Trades , equals(13)) })
test_that("Net.Trading.PL is 2126", 
          { expect_that(stats$Net.Trading.PL, equals(2126)) })
test_that("Avg.Trade.PL is 131.3846", 
          { expect_equal(stats$Avg.Trade.PL, 131.3846, .0001) })
test_that("Med.Trade.PL is 214", 
          { expect_that(stats$Med.Trade.PL, equals(214)) })
test_that("Largest.Winnner is 1242", 
          { expect_that(stats$Largest.Winner, equals(1242)) })
test_that("Largest.Loser is -1147", 
          { expect_that(stats$Largest.Loser, equals(-1147)) })
test_that("Gross.Profits is 4458", 
          { expect_that(stats$Gross.Profits, equals(4458)) })
test_that("Gross.Losses is -2750", 
          { expect_that(stats$Gross.Losses, equals(-2750)) })
test_that("Std.Dev.Trade.PL is 688.8931", 
          { expect_equal(stats$Std.Dev.Trade.PL, 688.8931, .0001) })
test_that("Percent.Positive is 61.53846", 
          { expect_equal(stats$Percent.Positive, 61.53846, .0001) })
test_that("Percent.Negative is 38.46154", 
          { expect_equal(stats$Percent.Negative, 38.46154, .0001) })
test_that("Profit.Factor is 1.621091", 
          { expect_equal(stats$Profit.Factor, 1.621091, .0001) })
test_that("Avg.Win.Trade is 557.25", 
          { expect_that(stats$Avg.Win.Trade, equals(557.25)) })
test_that("Med.Win.Trade is 401", 
          { expect_that(stats$Med.Win.Trade, equals(401)) })
test_that("Avg.Losing.Trade is -550", 
          { expect_equal(stats$Avg.Losing.Trade, -550, .0001)})
test_that("Med.Losing.Trade is -402", 
          { expect_equal(stats$Med.Losing.Trade, -402, .0001) })
test_that("Avg.Daily.PL is 131.3846", 
          { expect_equal(stats$Avg.Daily.PL, 131.3846, .0001) })
test_that("Med.Daily.PL is 214", 
          { expect_that(stats$Med.Daily.PL, equals(214)) })
test_that("Std.Dev.Daily.PL is 688.8931", 
          { expect_equal(stats$Std.Dev.Daily.PL, 688.8931, .0001) })
test_that("Max.Drawdown is -4431", 
          { expect_that(stats$Max.Drawdown, equals(-4431)) })
test_that("Profit.To.Max.Draw is 0.4798014", 
          { expect_equal(stats$Profit.To.Max.Draw, 0.4798014, .0001) })
test_that("Avg.WinLoss.Ratio is 1.013182", 
          { expect_equal(stats$Avg.WinLoss.Ratio, 1.013182, .0001) })
test_that("Med.WinLoss.Ratio is 0.9975124", 
          { expect_equal(stats$Med.WinLoss.Ratio, 0.9975124, .0001) })
test_that("Max.Equity is 3007", 
          { expect_that(stats$Max.Equity, equals(3007)) })
test_that("Min.Equity is -1424", 
          { expect_equal(stats$Min.Equity, -1424, .0001) })
test_that("End.Equity is 2126", 
          { expect_equal(stats$End.Equity, 2126, .0001) })

######################## BEE RETURNS ######################

context("bee portfolio returns are consistent ")


test_that("min return is -0.00925", 
          { expect_that(min(rets), equals(-0.00925))})
test_that("max return is 0.00597", 
          { expect_that(max(rets), equals(0.00597))})
test_that("skewness of returns is -0.4637252", 
          { expect_equal(skewness(rets), -0.4637252, .0001) })
test_that("kurtosis of returns is 3.926906", 
          { expect_equal(kurtosis(rets), 3.926906, .0001) })

######################## BEE TRANSACTIONS ######################

context("Bee transactions are consistent ")

Tqty   = txns$Txn.Qty
Tprice = txns$Txn.Price
Tfees  = txns$Txn.Fees
Tvalue = txns$Txn.Value
Tcost  = txns$Txn.Avg.Cost
TPL    = txns$Net.Txn.Realized.PL

## Transaction qty
test_that("The max position is 100 shares",
          { expect_that(max(Tqty) == 100, is_true()) })
test_that("The min position is -100 shares",
          { expect_that(min(Tqty) == -100, is_true()) })
## Transaction price
test_that("The first transacted price is the open of the day after the first signal",
          { expect_that(as.numeric(Tprice[2]) == 136.92, is_true()) })
## Transaction fees
test_that("There are no transaction fees",
          { expect_that(sum(Tfees) == 0, is_true()) })
## Transaction value
test_that("The system exits and enters with equal value",
          { expect_that(as.numeric(Tvalue[3]) - as.numeric(Tvalue[4]) == 0, is_true()) })
## Transaction cost
test_that("The transaction cost is equal to the transaction price (no fees)",
          { expect_that(as.numeric(Tcost[2]) == as.numeric(Tprice[2]), is_true()) })
## Transaction PL
test_that("The Net PnL on the first trade is 1004",
          { expect_equal(as.numeric(TPL[3]), 1004, .0001) })
test_that("The sum of Net PnL is 1708",
          { expect_equal(sum(TPL), 1708, .0001) })
test_that("The max of Net PnL is 1242",
          { expect_equal(max(TPL), 1242, .0001) })
test_that("The min of Net PnL is -1147",
          { expect_equal(min(TPL), -1147, .0001) })
