require(testthat)

################## bee.r  #########################

source('bee_version_for_tests.R')

#  bing = ls()
#  bang = ls(.strategy)
#  boom = ls(.blotter)

stratstat   = tradeStats(port)

Txns      = stratstat$Num.Txns
Trades    = stratstat$Num.Trades
NetPL     = stratstat$Net.Trading.PL
LWinner   = stratstat$Largest.Winner
LLoser    = stratstat$Largest.Loser
MaxDD     = stratstat$Max.Drawdown


# suppressWarnings(rm(list=ls()))
# suppressWarnings(rm(list=ls(), pos=.strategy))
# suppressWarnings(rm(list=ls(), pos=.blotter))

######################## RUN TEST SUITE #######################

context('Consistent trade statistics for bee.R')

test_that('Number of transactions is consistent', {

  expect_that(Txns, equals(24))
})

test_that('Number of the number of trades is consistent', {

  expect_that(Trades, equals(16))
})

test_that('Net Trading PL is consistent', {

  expect_that(NetPL, equals(63))
})

test_that('Largest Winner is consistent', {

  expect_that(LWinner, equals(12))
})

test_that('Largest Loser is consistent', {

  expect_that(LLoser, equals(-32))
})

test_that('Max Drawdown is consistent', {

  expect_that(MaxDD, equals(-867))
       
})


