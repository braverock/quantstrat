require(testthat)

################## bee.r  #########################

source('bbands_version_for_tests.R')

stratstat   = tradeStats(portfolio.st)

Txns      = stratstat$Num.Txns
Trades    = stratstat$Num.Trades
NetPL     = stratstat$Net.Trading.PL
LWinner   = stratstat$Largest.Winner
LLoser    = stratstat$Largest.Loser
MaxDD     = stratstat$Max.Drawdown
# KFactor   = stratstat$K.Factor
# RINAindex = stratstat$RINA.Index
# InMarket  = stratstat$In.Market
# BuyHold   = stratstat$Buy.Hold

# suppressWarnings(rm(list=ls()))
# suppressWarnings(rm(list=ls(), pos=.strategy))
# suppressWarnings(rm(list=ls(), pos=.blotter))

# suppressWarnings(rm("order_book.bbands",pos=.strategy))
# suppressWarnings(rm("account.bbands","portfolio.bbands",pos=.blotter))
# suppressWarnings(rm("account.st","portfolio.st","stock.str","stratBBands","initDate","initEq",'start_t','end_t'))
# 
######################## RUN TEST SUITE #######################

context('Consistent trade statistics for bbands.R')

test_that("Number of transactions is consistent", 
          { expect_that(Txns, equals(40)) })

test_that("Number of the number of trades is consistent", 
          { expect_that(Trades, equals(16)) })

test_that("Net Trading PL is consistent", 
          { expect_that(NetPL, equals(-1637)) })

test_that("Largest Winner is consistent", 
          { expect_that(LWinner, equals(656)) })

test_that("Largest Loser is consistent", 
          { expect_that(LLoser, equals(-1303)) })

test_that("Max Drawdown is consistent", 
          { expect_that(MaxDD, equals(-3037)) })

# test_that("K Factor is consistent", 
#           { expect_that(KFactor, equals(0)) })
# 
# test_that("RINA Index is consistent", 
#           { expect_that(RINAindex, equals(0)) })
# 
# test_that("Time in Market is consistent", 
#           { expect_that(InMarket, equals(0)) })
# 
# test_that("Buy and Hold is consistent", 
#           { expect_that(BuyHold, equals(0)) })


