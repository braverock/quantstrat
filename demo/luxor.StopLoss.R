
source('luxor.exits.R')

############################

require(foreach)

registerDoSEQ()

#require(doMC)
#registerDoMC(cores=2)

#require(doParallel)
#registerDoParallel(cores=2)

#require(doRedis)
#registerDoRedis('jobs')

############################

results <- apply.paramset(s, paramset.label='StopLoss', portfolio.st=p, verbose=TRUE)

print(results$tradeStats)
