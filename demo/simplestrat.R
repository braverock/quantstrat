
s <- strategy("simplestrat")
s <- add.indicator(strategy = s, name = "SMA", arguments = list(x = quote(Cl(mktdata)), n=10), label="SMA10")

# this indicator fails on dots, so we'll not enable it for now
#s <- add.indicator(strategy = s, name = "BBands", arguments = list(HLC = quote(HLC(mktdata)), sd = 1.8,maType=quote(SMA)))

getSymbols("IBM")
IBM.mod=applyIndicators(s,mktdata=IBM)

#manually apply a signal function for demonstration
IBM.mod = cbind(IBM.mod,sigComparison(label="Close.gt.Open",data=IBM.mod,columns=c("Close","Open"),">"))
IBM.mod = cbind(IBM.mod,sigComparison(label="Adjusted.gt.SMA",data=IBM.mod,columns=c("Adjusted","SMA10"),">"))

#or, do it properly and add it to the strategy:
s<- add.indicator(s,name="sigComparison",arguments = list(data=quote(mktdata),columns=c("Close","Open"),relationship=">"))label="Close.gt.Open")