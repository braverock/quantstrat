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

######################## ORDER BOOK ############################

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

######################## STATS  #################################

context("Bee trade statistics are consistent ")

test_that("Num.Txns is 27", 
          { expect_that(stats$Num.Txns , equals(27)) })
test_that("Num.Trades is 13", 
          { expect_that(stats$Num.Trades , equals(13)) })
test_that("Net.Trading.PL is -4802", 
          { expect_that(stats$Net.Trading.PL, equals(-4802)) })
test_that("Avg.Trade.PL is -360.7692", 
          { expect_equal(stats$Avg.Trade.PL, -360.7692, .0001) })
test_that("Med.Trade.PL is -455", 
          { expect_that(stats$Med.Trade.PL, equals(-455)) })
test_that("Largest.Winnner is 1426", 
          { expect_that(stats$Largest.Winner, equals(1426)) })
test_that("Largest.Loser is -1324", 
          { expect_that(stats$Largest.Loser, equals(-1324)) })
test_that("Gross.Profits is 1820", 
          { expect_that(stats$Gross.Profits, equals(1820)) })
test_that("Gross.Losses is -6510", 
          { expect_that(stats$Gross.Losses, equals(-6510)) })
test_that("Std.Dev.Trade.PL is 749.0479", 
          { expect_equal(stats$Std.Dev.Trade.PL, 749.0479, .0001) })
test_that("Percent.Positive is 30.76923", 
          { expect_equal(stats$Percent.Positive, 30.76923, .0001) })
test_that("Percent.Negative is 69.23077", 
          { expect_equal(stats$Percent.Negative, 69.23077, .0001) })
test_that("Profit.Factor is 0.2795699", 
          { expect_equal(stats$Profit.Factor, 0.2795699, .0001) })
test_that("Avg.Win.Trade is 455", 
          { expect_that(stats$Avg.Win.Trade, equals(455)) })
test_that("Med.Win.Trade is 164", 
          { expect_that(stats$Med.Win.Trade, equals(164)) })
test_that("Avg.Losing.Trade is -723.3333", 
          { expect_equal(stats$Avg.Losing.Trade, -723.3333, .0001)})
test_that("Med.Losing.Trade is -685", 
          { expect_equal(stats$Med.Losing.Trade, -685, .0001) })
test_that("Avg.Daily.PL is -360.7692", 
          { expect_equal(stats$Avg.Daily.PL, -360.7692, .0001) })
test_that("Med.Daily.PL is -455", 
          { expect_that(stats$Med.Daily.PL, equals(-455)) })
test_that("Std.Dev.Daily.PL is 749.0479", 
          { expect_equal(stats$Std.Dev.Daily.PL, 749.0479, .0001) })
test_that("Max.Drawdown is -6253", 
          { expect_that(stats$Max.Drawdown, equals(-6253)) })
test_that("Profit.To.Max.Draw is -0.02012297", 
          { expect_equal(stats$Profit.To.Max.Draw, -0.7679514, .0001) })
test_that("Avg.WinLoss.Ratio is 0.6290323", 
          { expect_equal(stats$Avg.WinLoss.Ratio, 0.6290323, .0001) })
test_that("Med.WinLoss.Ratio is 0.2394161", 
          { expect_equal(stats$Med.WinLoss.Ratio, 0.2394161, .0001) })
test_that("Max.Equity is 1446", 
          { expect_that(stats$Max.Equity, equals(1446)) })
test_that("Min.Equity is -4807", 
          { expect_equal(stats$Min.Equity, -4807, .0001) })
test_that("End.Equity is -4802", 
          { expect_equal(stats$End.Equity, -4802, .0001) })

######################## bug RETURNS ######################

context("bee portfolio returns are consistent ")


test_that("min return is -0.00597", 
          { expect_that(min(rets),equals(-0.00597))})
test_that("max return is 0.0092", 
          { expect_that(max(rets), equals(0.0092))})
test_that("skewness of returns is 0.1321215", 
          { expect_equal(skewness(rets), 0.1321215, .0001) })
test_that("kurtosis of returns is 3.063772", 
          { expect_equal(kurtosis(rets), 3.063772, .0001) })


