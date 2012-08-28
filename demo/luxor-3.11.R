#!/usr/bin/Rscript --vanilla
#
# Jan Humme (@opentrades) - August 2012
#
# Tested and found to work correctly using blotter r1123
#
# From Jaekle & Tamasini: A new approach to system development and portfolio
# optimisation (ISBN 978-1-905641-79-6)
#
# Figure 3.11: MAE graph of Luxor system

options(width = 240)
#Sys.setenv(TZ="GMT")

require(xts)

###############################################################################

data('luxor-3.09', package='quantstrat')

posPL <- portfolio$symbols$GBPUSD$posPL

###############################################################################

trades <- list()

trades$Start <- index(posPL[which(posPL$Pos.Value!=0 & lag(posPL$Pos.Value)==0),])
trades$End <- index(posPL[which(posPL$Pos.Value==0 & lag(posPL$Pos.Value)!=0),])

trades$Start <- trades$Start[1:length(trades$End)]  # discard open last trade, if any

for(i in 1:length(trades$End))
{
	timespan <- paste(format(trades$Start[[i]], "%Y-%m-%d %H:%M:%OS6"),
			  format(trades$End[[i]], "%Y-%m-%d %H:%M:%OS6"), sep="::")

	trade <- posPL[timespan]

	if(first(trade)$Pos.Qty==0) trade <- tail(trade, -1)
	if(last(trade)$Pos.Qty!=0) trade <- head(trade, -1)

	trades$Net.Trading.PL[i] <- sum(trade$Net.Trading.PL)
	trades$Drawdown[i] <- min(0,cumsum(trade$Net.Trading.PL))
}

trades <- as.data.frame(trades)

###############################################################################

profitable <- (trades$Net.Trading.PL > 0)

plot(abs(trades[, c('Drawdown','Net.Trading.PL')]), type='n',
	xlab='Drawdown ($)', ylab='Profit (Loss) in $',
	main='Figure 3.11: Maximum Adverse Excursion (MAE)')

points(abs(trades[ profitable, c('Drawdown','Net.Trading.PL')]), pch=2, col='green')
points(abs(trades[!profitable, c('Drawdown','Net.Trading.PL')]), pch=25, col='red')

grid()

legend(
	x='right', inset=0.1,
	legend=c('Profitable Trade','Losing Trade'),
	pch=c(2,6),
	col=c('green','red')
)

