
require(svUnit)

source('bbands_version_for_tests.r')

bbands = 'Testing consistent trade stats'

test(bbands) = function() {

  stats = tradeStats(portfolio.st)

  checkEquals( stats$Num.Txns, 622)
  checkEquals( stats$Num.Trades, 258)
  checkEquals( stats$Net.Trading.PL, -204)
  checkEquals( stats$Largest.Winner, 417)
  checkEquals( stats$Largest.Loser, -824)
  checkEquals( stats$maxDrawdown, -2618)
}


clearLog()
runTest(bbands)
Log()
#summary(Log())
