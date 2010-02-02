
s <- strategy("simplestrat")
s <- add.indicator(strategy = s, name = "EMA", arguments = list(x = quote(Cl(mktdata)), n=10))
s <- add.indicator(strategy = s, name = "BBands", arguments = list(HLC = quote(HLC(mktdata)), sd = 1.8,maType=quote(SMA)))

getSymbols("IBM")
applyIndicators(s,IBM)