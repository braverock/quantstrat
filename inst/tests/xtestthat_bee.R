require(testthat)

################## bee.r  #########################

source("bee_version_for_tests.R")

stratstat   = tradeStats(port)

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

######################## RUN TEST SUITE #######################

context("Consistent trade statistics for bee.R")

test_that("Number of transactions is consistent", 
          { expect_that(Txns, equals(18)) })

test_that("Number of the number of trades is consistent", 
          { expect_that(Trades, equals(12)) })

test_that("Net Trading PL is consistent", 
          { expect_that(NetPL, equals(199)) })

test_that("Largest Winner is consistent", 
          { expect_that(LWinner, equals(1336)) })

test_that("Largest Loser is consistent", 
          { expect_that(LLoser, equals(-525)) })

test_that("Max Drawdown is consistent", 
          { expect_that(MaxDD, equals(-20359)) })

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


