#!/usr/bin/Rscript --vanilla

require(rgl)

load('../data/luxor.250.RData')

# parameters

range.Fast = range(stats$Param.indicator.1.nFast)
min.Fast = range.Fast[1]
max.Fast = range.Fast[2]

range.Slow = range(stats$Param.indicator.2.nSlow)
min.Slow = range.Slow[1]
max.Slow = range.Slow[2]

nrows = range.Fast[2] - range.Fast[1] + 1
ncols = range.Slow[2] - range.Slow[1] + 1

# results

range.Net.Trading.PL = range(stats$Net.Trading.PL)
min.Net.Trading.PL = range.Net.Trading.PL[1]
max.Net.Trading.PL = range.Net.Trading.PL[2]
length.Net.Trading.PL = range.Net.Trading.PL[2] - range.Net.Trading.PL[1] + 1

range.Max.Drawdown = range(stats$maxDrawdown)
min.Max.Drawdown = range.Max.Drawdown[1]
max.Max.Drawdown = range.Max.Drawdown[2]
length.Max.Drawdown = range.Max.Drawdown[2] - range.Max.Drawdown[1] + 1

range.Avg.Trade.PL = range(stats$Avg.Trade.PL)
min.Avg.Trade.PL = range.Avg.Trade.PL[1]
max.Avg.Trade.PL = range.Avg.Trade.PL[2]
length.Avg.Trade.PL = range.Avg.Trade.PL[2] - range.Avg.Trade.PL[1] + 1

range.Num.Trades = range(stats$Num.Trades)
min.Num.Trades = range.Num.Trades[1]
max.Num.Trades = range.Num.Trades[2]
length.Num.Trades = range.Num.Trades[2] - range.Num.Trades[1] + 1

Net.Trading.PL = matrix(NA, nrows, ncols)
Max.Drawdown = matrix(NA, nrows, ncols)
Avg.Trade.PL = matrix(NA, nrows, ncols)
Num.Trades = matrix(NA, nrows, ncols)

for(i in 1:length(stats$Net.Trading.PL))
{
	fast = stats$Param.indicator.1.nFast[i]
	slow = stats$Param.indicator.2.nSlow[i]

	Net.Trading.PL	[fast-min.Fast+1, slow-min.Slow+1] = stats$Net.Trading.PL[[i]]
	Max.Drawdown	[fast-min.Fast+1, slow-min.Slow+1] = stats$maxDrawdown[[i]]
	Avg.Trade.PL	[fast-min.Fast+1, slow-min.Slow+1] = stats$Avg.Trade.PL[[i]]
	Num.Trades	[fast-min.Fast+1, slow-min.Slow+1] = stats$Num.Trades[[i]]
}

# make graphs

options(scipen=99)

colors <- rainbow(length.Net.Trading.PL, start=0, end=6/6)[Net.Trading.PL - min.Net.Trading.PL + 1]
rgl.open()
rgl.surface(x=min.Fast:max.Fast, z=min.Slow:max.Slow, y=Net.Trading.PL, color=colors)
rgl.planes(a=1, alpha=0.7)
aspect3d(1,1,1)
axes3d()
title3d('luxor',NULL,'FastSMA','Net.Trading.PL','SlowSMA')

colors <- rainbow(length.Max.Drawdown, start=0, end=4/6)[Max.Drawdown - min.Max.Drawdown + 1]
rgl.open()
rgl.surface(x=min.Fast:max.Fast, z=min.Slow:max.Slow, y=Max.Drawdown, color=colors)
aspect3d(1,1,1)
axes3d()
title3d('luxor',NULL,'FastSMA','Max.Drawdown','SlowSMA')

colors <- rainbow(length.Avg.Trade.PL, start=0, end=6/6)[Avg.Trade.PL - min.Avg.Trade.PL + 1]
rgl.open()
rgl.surface(x=min.Fast:max.Fast, z=min.Slow:max.Slow, y=Avg.Trade.PL, color=colors)
rgl.planes(a=1, alpha=0.7)
aspect3d(1,1,1)
axes3d()
title3d('luxor',NULL,'FastSMA','Avg.Trade.PL','SlowSMA')

colors <- rainbow(length.Num.Trades, start=0, end=3/6)[Num.Trades - min.Num.Trades + 1]
rgl.open()
rgl.surface(x=min.Fast:max.Fast, z=min.Slow:max.Slow, y=Num.Trades, color=colors)
aspect3d(1,1,1)
axes3d()
title3d('luxor',NULL,'FastSMA','Num.Trades','SlowSMA')

