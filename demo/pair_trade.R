#Kindly contributed to quantstrat by Garrett See
#code borrowed heavily from existing quantstrat demos

#This is a simple pairs trading example intended to illustrate how you can extend
#existing quantstrat functionality.  Also, it uses addPosLimits to specify
#levels and position limits, and shows how to pass a custom order sizing function to osFUN 

#Note that it would be easier to build a spread first and treat it as a single instrument
#instead of dealing with a portfolio of stocks.

## given 2 stocks, calculate their ratio.  If the ratio falls below it's
# 2 stdev band, then when it crosses back above it, buy stock 1 and sell stock 2.  
# If the ratio rises above it's 2 stdev band, then when it crosses back below
# it, sell stock 1 and buy stock 2.  If the ratio crosses it's moving average,
# then flatten any open positions.

# The Qty of Stock A that it buys (sells) = MaxPos / lvls
# The Qty of Stock B that is sells (buys) = MaxPos * Ratio / lvls  

try(rm("order_book.pair1",pos=.strategy),silent=TRUE)
try(rm("account.pairs", "portfolio.pair1", pos=.blotter), silent=TRUE)
try(rm("initDate", "endDate", "startDate", "initEq", "SD", "N", "symb1", "symb2", 
	"portfolio1.st", "account.st", "pairStrat", "out1"), silent=TRUE)

require(quantstrat)
initDate = '2009-01-01'		
endDate = '2011-05-01'
startDate = '2009-01-02'
initEq = 100000
SD = 2
N = 20

MaxPos = 1500  #max position in stockA; 
#max position in stock B will be max * ratio, i.e. no hard position limit in Stock B
lvls = 3	#how many times to fade; Each order's qty will = MaxPos/lvls

symb1 <- 'CVX' #change these to try other pairs
symb2 <- 'XOM' #if you change them, make sure position limits still make sense

portfolio1.st <- 'pair1'
account.st <- 'pairs'

getSymbols(c(symb1, symb2), from=startDate, to=endDate, adjust=TRUE) 

currency("USD")
stock(symb1, currency="USD", multiplier=1)
stock(symb2, currency="USD", multiplier=1)

#Initialize Portfolio, Account, and Orders
initPortf(name=portfolio1.st, c(symb1,symb2), initDate=initDate)
initAcct(account.st, portfolios=portfolio1.st, initDate=initDate, initEq=initEq)
initOrders(portfolio=portfolio1.st,initDate=initDate)

#create a slot in portfolio for symb1 and symb2 to make them available to osFUN
pair <- c('long','short')
names(pair) <- c(symb1,symb2)
.blotter[[paste('portfolio',portfolio1.st,sep='.')]]$pair <- pair

# Create initial position limits and levels by symbol
# allow 3 entries for long and short.
addPosLimit(portfolio=portfolio1.st, timestamp=initDate, symbol=symb1, maxpos=MaxPos, longlevels=lvls, minpos=-MaxPos, shortlevels=lvls)
addPosLimit(portfolio=portfolio1.st, timestamp=initDate, symbol=symb2, maxpos=MaxPos, longlevels=lvls, minpos=-MaxPos, shortlevels=lvls)

# Create a strategy object 
pairStrat <- strategy('pairStrat')

calcRatio <- function(x) { #returns the ratio of close prices for 2 symbols
	x1 <- get(x[1])
	x2 <- get(x[2])
	rat <- Ad(x1) / Ad(x2)
	colnames(rat) <- 'Ratio'
	rat
} 
Ratio <- calcRatio(c(symb1[1],symb2[1]))
#let's go ahead and put this in a slot in portfolio
.blotter[[paste('portfolio',portfolio1.st,sep='.')]]$Ratio <- Ratio
#and make a function to get the most recent Ratio
getRatio <- function(portfolio, timestamp) {
	portf <- getPortfolio(portfolio)
	toDate <- paste("::", timestamp, sep="")
	Ratio <- last(portf$Ratio[toDate])
	as.numeric(Ratio)
}

# Create an indicator - BBands on the Ratio
pairStrat <- add.indicator(strategy = pairStrat, name = "calcRatio", arguments = list(x=c(symb1,symb2)))
pairStrat <- add.indicator(strategy = pairStrat, name = "BBands", arguments = list(HLC=quote(Ratio), sd=SD, n=N, maType='SMA'))

#applyIndicators(strategy=pairStrat,mktdata=get(symb1[1])) #for debugging

# Create signals - buy when crossing lower band from below, sell when crossing upper band from above, flatten when crossing mavg from above or from below
pairStrat <- add.signal(strategy = pairStrat, name = "sigCrossover", arguments= list(columns=c("Ratio","up"), relationship="lt"), label="cross.up")
pairStrat <- add.signal(strategy = pairStrat, name = "sigCrossover", arguments= list(columns=c("Ratio","dn"), relationship="gt"), label="cross.dn")
pairStrat <- add.signal(strategy = pairStrat, name = "sigCrossover", arguments= list(columns=c("Ratio","mavg"), relationship="lt"), label="cross.mid.fa")
pairStrat <- add.signal(strategy = pairStrat, name = "sigCrossover", arguments= list(columns=c("Ratio","mavg"), relationship="gt"), label="cross.mid.fb")

#make an order sizing function
#######################_ORDER SIZING FUNCTION_##########################################################
#check to see which stock it is. If it's the second stock, reverse orderqty and orderside
osSpreadMaxPos <- function (data, timestamp, orderqty, ordertype, orderside, portfolio, symbol, ruletype, ..., orderprice) 
{
	portf <- getPortfolio(portfolio)
    legside <- portf$pair[symbol] #"long" if symbol=symb1, "short" if symbol=symb2
	if (legside != "long" && legside != "short") stop('pair must contain "long" and "short"')
	ratio <- getRatio(portfolio, timestamp)
	
	pos <- getPosQty(portfolio, symbol, timestamp) 	    
    PosLimit <- getPosLimit(portfolio, symbol, timestamp) 
	qty <- orderqty
	if (legside == "short") {#symbol is 2nd leg
		## Comment out next line to use equal ordersizes for each stock. 
		addPosLimit(portfolio=portfolio, timestamp=timestamp, symbol=symbol, maxpos=MaxPos*ratio, longlevels=lvls, minpos=-MaxPos*ratio, shortlevels=lvls)
		#TODO: is it okay that MaxPos and lvls come from .GlobalEnv ?
		qty <- -orderqty #switch orderqty for Stock B
	}
	
	if (qty > 0) orderside = 'long'
	if (qty < 0) orderside = 'short'
 
	orderqty <- osMaxPos(data=data,timestamp=timestamp,orderqty=qty,ordertype=ordertype,
					orderside=orderside,portfolio=portfolio,symbol=symbol,ruletype=ruletype, ...)
	orderqty <- round(orderqty,0)

	#Add the order here instead of in the ruleSignal function
	if (!is.null(orderqty) & !orderqty == 0 & !is.null(orderprice)) {
            addOrder(portfolio = portfolio, symbol = symbol, 
                timestamp = timestamp, qty = orderqty, price = as.numeric(orderprice), 
                ordertype = ordertype, side = orderside, 
                status = "open", ... = ...)
    }
	return(0) #so that ruleSignal function doesn't also try to place an order
}
########################################################################################################

# Create entry and exit rules for longs  and for shorts. Both symbols will get the same buy/sell signals, but osMaxPos will reverse those for the second symbol.
# orderqty's are bigger than PosLimits allow. osMaxPos will adjust the orderqty down to 1/3 the max allowed. (1/3 is because we are using 3 levels in PosLimit)
pairStrat <- add.rule(strategy = pairStrat, name='ruleSignal', arguments = list(sigcol="cross.dn", sigval=TRUE, orderqty=1e6, ordertype='market', orderside=NULL, osFUN='osSpreadMaxPos'), type='enter' ) 
pairStrat <- add.rule(strategy = pairStrat, name='ruleSignal', arguments = list(sigcol="cross.up", sigval=TRUE, orderqty=-1e6, ordertype='market', orderside=NULL, osFUN='osSpreadMaxPos'), type='enter')
pairStrat <- add.rule(strategy = pairStrat, name='ruleSignal', arguments = list(sigcol="cross.mid.fb", sigval=TRUE, orderqty='all', ordertype='market', orderside=NULL), type='exit') 
pairStrat <- add.rule(strategy = pairStrat, name='ruleSignal', arguments = list(sigcol="cross.mid.fa", sigval=TRUE, orderqty='all', ordertype='market', orderside=NULL), type='exit')

#applySignals(strategy=pairStrat, mktdata=applyIndicators(strategy=pairStrat,mktdata=get(symb1))) #for debugging

out1<-try(applyStrategy(strategy=pairStrat, portfolios=portfolio1.st))

updatePortf(Portfolio=portfolio1.st,Dates=paste("::",as.Date(Sys.time()),sep=''))
updateAcct(account.st,Dates=paste(startDate,endDate,sep="::")) 
updateEndEq(account.st,Dates=paste(startDate,endDate,sep="::"))
getEndEq(account.st,Sys.time())

dev.new()
chart.Posn(Portfolio=portfolio1.st,Symbol=symb1)
dev.new()
chart.Posn(Portfolio=portfolio1.st,Symbol=symb2)
dev.new()
chartSeries(Cl(get(symb1))/Cl(get(symb2)),TA="addBBands()")

ret1 <- PortfReturns(account.st)
ret1$total <- rowSums(ret1)
#ret1

if("package:PerformanceAnalytics" %in% search() || require("PerformanceAnalytics",quietly=TRUE)) {
	getSymbols("SPY", from='1999-01-01')
	SPY.ret <- Return.calculate(SPY$SPY.Close)
	tmp <- merge(SPY.ret,ret1$total,all=FALSE)
	dev.new()
	charts.PerformanceSummary(cbind(tmp[,2],tmp[,1]),geometric=FALSE,wealth.index=TRUE)
}


###############################################################################
# R (http://r-project.org/) Quantitative Strategy Model Framework
#
# Package Copyright (c) 2009-2010
# Peter Carl, Dirk Eddelbuettel, Brian G. Peterson, Jeffrey Ryan, and Joshua Ulrich 
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
