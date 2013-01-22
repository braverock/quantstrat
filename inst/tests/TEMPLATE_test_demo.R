##### PLACE DEMO AND TEST DATES HERE #################
# 
# if(isTRUE(options('in_test')$in_test))
#   # use test dates
#   {initDate="2011-01-01" 
#   endDate="2012-12-31"   
#   } else
#   # use demo defaults
#   {initDate="1999-12-31"
#   endDate=Sys.Date()}
#       
###############################################################

##### PLACE THIS BLOCK AT END OF DEMO SCRIPT ################### 
# book  = getOrderBook(port)
# stats = tradeStats(port)
# rets  = PortfReturns(acct)
################################################################


################## TEMPLATE FILL OUT ###########################
#####                                                       ####
##### DEMO      =  %s/DEMO/_the_name_of_my_demo/g           ####
##### PORTFOLIO =  %s/PORTFOLIO/_the_name_of_my_portfolio/g ####
##### STOCK     =  %s/STOCK/_the_name_of_my_stock/g         ####
#####                                                       ####
################################################################

################## LOAD TESTING FRAMEWORK ######################

require(testthat)
require(PortfolioAnalytics)

######### ACTIVATE TEST DATES AND SOURCE DEMO #####################

options(in_test=TRUE)
source(system.file("demo/DEMO.R", package="quantstrat"))

################### DEFINE VARIABLES TO BE TESTED ##############

qty    = book$PORTFOLIO$STOCK[,"Order.Qty"]
price  = book$PORTFOLIO$STOCK[,"Order.Price"]
type   = book$PORTFOLIO$STOCK[,"Order.Type"]
side   = book$PORTFOLIO$STOCK[,"Order.Side"]
status = book$PORTFOLIO$STOCK[,"Order.Status"]
fees   = book$PORTFOLIO$STOCK[,"Txn.Fees"]
rule   = book$PORTFOLIO$STOCK[,"Rule"]

######################## ORDER BOOK ############################

context("DEMO order book is consistent ")

## quantity
test_that("The first entry is 100", 
          { expect_that(as.character(qty[2]) =="100", is_true()) })

test_that("The first exit is all", 
          { expect_that(as.character(qty[3]) =="all", is_true()) })
## price
test_that("The first entry price is 88.9", 
          { expect_that(as.character(price[2]) =="88.9", is_true()) })

test_that("The first exit price is 87.42", 
          { expect_that(as.character(price[3]) =="87.42", is_true()) })
## type
test_that("The first trade is a market order", 
          { expect_that(as.character(type[2]) =="market", is_true()) })
## side
test_that("The first trade is entered long", 
          { expect_that(as.character(side[2]) =="long", is_true()) })
## status
test_that("The last trade is closed", 
          { expect_that(as.character(status[20]) =="closed", is_true()) })
## fees
test_that("The first trade has no transaction fees", 
          { expect_that(as.character(fees[2]) =="0", is_true()) })
## rule
test_that("The first transaction is an enter long", 
          { expect_that(as.character(rule[2]) =="EnterLONG", is_true()) })

test_that("The second transaction is an exit long", 
          { expect_that(as.character(rule[3]) =="ExitLONG", is_true()) })

######################## STATS  #################################

context("DEMO trade statistics are consistent ")

test_that("Num.Txns is 19", 
          { expect_that(stats$Num.Txns , equals(19)) })
test_that("Num.Trades is 9", 
          { expect_that(stats$Num.Trades , equals(9)) })
test_that("Net.Trading.PL is -36", 
          { expect_that(stats$Net.Trading.PL, equals(-36)) })
test_that("Avg.Trade.PL is -4", 
          { expect_that(stats$Avg.Trade.PL, equals(-4)) })
test_that("Med.Trade.PL is 6", 
          { expect_that(stats$Med.Trade.PL, equals(6)) })
test_that("Largest.Winnner is 581", 
          { expect_that(stats$Largest.Winner, equals(581)) })
test_that("Largest.Loser is -1064", 
          { expect_that(stats$Largest.Loser, equals(-1064)) })
test_that("Gross.Profits is 1425", 
          { expect_that(stats$Gross.Profits, equals(1425)) })
test_that("Gross.Losses is -1461", 
          { expect_that(stats$Gross.Losses, equals(-1461)) })
test_that("Std.Dev.Trade.PL is 477.7599", 
          { expect_equal(stats$Std.Dev.Trade.PL, 477.7599, .0001) })
test_that("Percent.Positive is 55.55556", 
          { expect_equal(stats$Percent.Positive, 55.55556, .0001) })
test_that("Percent.Negative is 44.44444", 
          { expect_equal(stats$Percent.Negative, 44.44444, .0001) })
test_that("Profit.Factor is 0.9753593", 
          { expect_equal(stats$Profit.Factor, 0.9753593, .0001) })
test_that("Avg.Win.Trade is 285", 
          { expect_that(stats$Avg.Win.Trade, equals(285)) })
test_that("Med.Win.Trade is 221", 
          { expect_that(stats$Med.Win.Trade, equals(221)) })
test_that("Avg.Losing.Trade is -365.25", 
          { expect_that(stats$Avg.Losing.Trade, equals(-365.25)) })
test_that("Med.Losing.Trade is -159.5", 
          { expect_that(stats$Med.Losing.Trade, equals(-159.5)) })
test_that("Avg.Daily.PL is -4", 
          { expect_that(stats$Avg.Daily.PL, equals(-4)) })
test_that("Med.Daily.PL is 6", 
          { expect_that(stats$Med.Daily.PL, equals(6)) })
test_that("Std.Dev.Daily.PL is 477.7599", 
          { expect_equal(stats$Std.Dev.Daily.PL, 477.7599, .0001) })
test_that("Max.Drawdown is -1789", 
          { expect_that(stats$Max.Drawdown, equals(-1789)) })
test_that("Profit.to.Max.Draw is -0.02012297", 
          { expect_equal(stats$Profit.to.Max.Draw, -0.02012297, .0001) })
test_that("Avg.WinLoss.Ratio is 0.7802875", 
          { expect_equal(stats$Avg.WinLoss.Ratio, 0.7802875, .0001) })
test_that("Med.WinLoss.Ratio is 1.38558", 
          { expect_equal(stats$Med.WinLoss.Ratio , 1.38558, .0001) })
test_that("Max.Equity is 390", 
          { expect_that(stats$Max.Equity , equals(390)) })
test_that("Min.Equity is -1539", 
          { expect_equal(stats$Min.Equity , -1539, .0001) })
test_that("End.Equity is -36", 
          { expect_equal(stats$End.Equity , -36, .0001) })

######################## PORTFOLIO RETURNS ######################

context("DEMO portfolio returns are consistent ")

test_that("min return is -0.00597", 
          { expect_equal(min(rets), -0.00597, .0001) })
test_that("max return is 0.0092", 
          { expect_equal(min(rets),0.0092, .0001) })
test_that("skewness of returns is 0.234199", 
          { expect_equal(min(rets), 0.234199, .0001) })
test_that("kurtosis of returns is 5.04833", 
          { expect_equal(min(rets), 5.04833, .0001) })
