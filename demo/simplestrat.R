require(quantstrat)
try(rm("order_book.simplestrat",pos=.strategy),silent=TRUE)
try(rm("account.simplestrat","portfolio.simplestrat",pos=.blotter),silent=TRUE)
try(rm("account.st","portfolio.st","IBM","s","initDate","initEq",'start_t','end_t'),silent=TRUE)

initDate='1997-12-31'
initEq=1000000

portfolio.st='simplestrat'
account.st='simplestrat'

initPortf(portfolio.st,symbols='IBM', initDate=initDate)
initAcct(account.st,portfolios='simplestrat', initDate=initDate)
initOrders(portfolio=portfolio.st,initDate=initDate)

s <- strategy("simplestrat")
#s <- add.indicator(strategy = s, name = "SMA", arguments = list(x = quote(Cl(mktdata)), n=10), label="SMA10")
s <- add.indicator(strategy = s, name = "BBands", arguments = list(HLC = quote(HLC(mktdata)), sd = 1.5,maType=quote(SMA)))


#if you wanted to manually apply a signal function for demonstration
#cbind(IBM.mod,sigComparison(label="Close.gt.Open",data=IBM.inds,columns=c("Close","Open"),">"))
#cbind(IBM.mod,sigComparison(label="Adjusted.gt.SMA",data=IBM.inds,columns=c("Adjusted","SMA10"),">"))

#do it properly and add it to the strategy:
#s<- add.signal(s,name="sigComparison",arguments = list(data=quote(mktdata),columns=c("Close","Open"),relationship="gt"),label="Cl.gt.Op")
s<- add.signal(s,name="sigCrossover",arguments = list(data=quote(mktdata),columns=c("Close","up"),relationship="gt"),label="Cl.gt.UpperBand")
s<- add.signal(s,name="sigCrossover",arguments = list(data=quote(mktdata),columns=c("Close","dn"),relationship="lt"),label="Cl.lt.LowerBand")

#IBM.sigs<-applySignals(s,mktdata=IBM.inds)

# lets add some rules
s 
s <- add.rule(s,name='ruleSignal', arguments = list(data=quote(mktdata),sigcol="Cl.gt.UpperBand",sigval=TRUE, orderqty=-100, ordertype='market', orderside=NULL, threshold=NULL),type='enter')
s <- add.rule(s,name='ruleSignal', arguments = list(data=quote(mktdata),sigcol="Cl.lt.LowerBand",sigval=TRUE, orderqty= 100, ordertype='market' , orderside=NULL, threshold=NULL),type='enter')
#TODO add thresholds and stop-entry and stop-exit handling to test

getSymbols("IBM")
start_t<-Sys.time()
out<-try(applyStrategy(strategy='s' , portfolios='simplestrat'))
end_t<-Sys.time()
end_t-start_t