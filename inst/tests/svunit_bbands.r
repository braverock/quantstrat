
require(svUnit)

source('bbands_version_for_tests.r')

bbands = 'Testing consistent trade stats'

test(bbands) = function() {

  stats = tradeStats(portfolio.st)

  checkEquals( stats$Num.Txns, 51)
  checkEquals( stats$Num.Trades, 21)
  checkEquals( stats$Net.Trading.PL, 46)
  checkEquals( stats$Largest.Winner, 24)
  checkEquals( stats$Largest.Loser, -38)
  checkEquals( stats$Max.Drawdown, -84)
}


clearLog()
runTest(bbands)
Log()
summary(Log())
